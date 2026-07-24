# assignWorkLocations.R
#
# Assigns each employed agent a work SA1 using the "balanced" allocation: as the
# run progresses, per-worker destination probabilities are pulled toward the
# census-observed distributions (work-location, SA3-pair distance, and global
# distance histograms) by three running counters that are read and incremented
# once per worker. That feedback makes the loop inherently SEQUENTIAL and
# floating-point sensitive - a single perturbed probability eventually flips a
# sample() and the counters diverge - so the whole model is order-dependent.
#
# This is the performance-optimised implementation. It is BIT-IDENTICAL to the
# previous version (verified over all 2,079,869 workers against the prior
# workers_sa1_balance1.rds); the arithmetic and candidate row order are preserved
# EXACTLY, and only the data structures and lookups changed:
#   * the counter-independent candidate table is precomputed once per work-SA3
#     ("skeleton") instead of being rebuilt (5+ dplyr joins) for every worker;
#   * the running counters are plain integer vectors / a 3-D array updated in
#     O(1) by position, replacing the per-iteration which() linear scan and the
#     data.frame `[<<-` row writes of the previous version;
#   * the per-worker distance/probability assembly is vectorised integer indexing
#     rather than repeated inner_join()s.
# adjustPr() and the overal_pr formula are used verbatim on bit-identical inputs
# in identical order, so every sample() draw sees the same probability vector and
# consumes the RNG identically. Runtime dropped from hours to ~10 minutes.
#
# The original loop-per-worker implementation is preserved in git history and at
# ../microData/archive/pre_typofix_20260724_162351/assignWorkLocations.R.original
# The equivalence tests are assignWorkLocations_verify.R (first K workers) and
# assignWorkLocations_fullverify.R (all workers, regenerates the output).

library(dplyr)
library(tidyr)
library(data.table)
library(logger)
library(progress)
library(bit64)

roundPreserveSum <- function(x) {
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y
}

# Pull an expected probability vector toward the census target given the running
# actual counts. Unchanged from the original implementation.
adjustPr <- function(expected,actual) {
  if (sum(expected)==0) expected[]<-1/length(expected)
  if (sum(expected)!=1) expected <- expected/sum(expected)
  if (sum(actual)==0) return(expected)
  actual_normalised <- actual/sum(actual)
  delta <- expected-actual_normalised
  delta[delta<0] <- 0
  if (sum(delta)==0) delta<-expected
  delta_normalised <- delta/sum(delta)
  return(delta_normalised)
}

# outputDir    : ../output/synthetic_population
# workers      : the population data frame (employed agents are filtered here)
# max_workers  : process only the first N workers (verification only); Inf = all
# oracle       : optional data.table (PlanId, sa1_work) to assert bit-identity
#                against each assignment (stops at first divergence)
# save_outputs : write the workers/counter RDS files (only on a full run)
assignWorkLocations <- function(outputDir, workers, max_workers = Inf,
                                oracle = NULL, save_outputs = TRUE) {
  log_info("Commencing work location assignment")
  log_info("Prepare census and other datasets to assign work locations.")
  source('prepWorkData.R', local=TRUE)
  prepWorkData(outputDir)

  log_info("Import data")
  work_hist_global <- readRDS(paste0(outputDir,"/work_hist_global.rds")) %>%
    mutate(distance=row_number()) %>%
    select(distance,global_dist_pr=pr) %>%
    data.table()

  work_hist_sa3 <- readRDS(paste0(outputDir,"/work_hist_sa3.rds"))
  work_sa3_movement <- readRDS(paste0(outputDir,"/work_sa3_movement.rds"))
  workers <- workers %>%
    ungroup() %>%
    data.frame() %>%
    filter(is_employed) %>%
    mutate(PlanId=AgentId) %>%
    dplyr::select(PlanId,SA1_MAINCODE_2016)
  workers$sa3_home <- as.integer(substr(workers$SA1_MAINCODE_2016,1,5))
  workLocationsSA1 <- fread(paste0(outputDir, "/workLocationsSA1.csv"))[, .(sa1_maincode_2016, work_location_pr = sa3_pr)]
  setkey(workLocationsSA1, sa1_maincode_2016)

  log_info("Evaluate distances")
  distanceMatrixWork <- readRDS(file=paste0(outputDir,"/distanceMatrixWork.rds"))
  distanceMatrixIndex <- fread(paste0(outputDir, "/distanceMatrixIndex.csv"))
  setkey(distanceMatrixIndex, sa1_maincode_2016)
  distanceMatrixIndexWork <- fread(paste0(outputDir,"/distanceMatrixIndexWork.csv"))
  setkey(distanceMatrixIndexWork, sa1_maincode_2016)

  work_hist_sa3_wide <- work_hist_sa3 %>%
    pivot_wider(id_cols=c(sa3_home,sa3_work),
                names_from=range_value,
                values_from=pr) %>%
    data.table()

  home_count_sa3 <- workers %>%
    mutate(sa3_home=as.integer(substr(workers$SA1_MAINCODE_2016,1,5))) %>%
    group_by(sa3_home) %>%
    summarise(home_count=n()) %>%
    ungroup()

  log_info("Calculate home to work sa3 counts")
  work_count_sa3 <- work_sa3_movement %>%
    select(sa3_home,sa3_work,pr_sa3) %>%
    inner_join(home_count_sa3, by=join_by(sa3_home)) %>%
    group_by(sa3_home) %>%
    mutate(work_count=roundPreserveSum(home_count*pr_sa3)) %>%
    ungroup() %>%
    select(sa3_home,sa3_work,work_count)

  set.seed(10000)
  work_sa3 <- work_count_sa3 %>%
    uncount(weights=work_count) %>%
    group_by(sa3_home) %>%
    mutate(sa3_order=sample(1:n())) %>%
    ungroup()
  workers_sa3 <- setDT(workers)[order(PlanId)][
    , sa3_order := seq_len(.N), by = sa3_home
  ][
    work_sa3, on = .(sa3_home, sa3_order)
  ]

  log_info("Assign workers SA1 locations ('Balanced')")
  set.seed(10000)

  # The counters below carry the balancing feedback. They are kept as flat
  # integer structures indexed by position so they can be read (gathered) and
  # incremented in O(1); their VALUES match the original data.frame/array
  # counters exactly. Do not parallelise the worker loop without preserving this
  # sequential feedback (see file header).
  workLocationCounter <- distanceMatrixIndexWork%>%select(sa1_maincode_2016)%>%mutate(count=0)
  sa3DistCounter <- array(data=0L, dim=c(40, 40, 280))
  sa3DistCounterIndex <- data.table(sa3=distanceMatrixIndexWork$sa3%>%unique()%>%sort())%>%mutate(index=row_number())
  setkey(sa3DistCounterIndex, sa3)
  globalDistCounter <- data.table(distance=1:280,count=0)

  workers_sa1 <- workers_sa3[sample(nrow(workers_sa3))][, sa1_work := as.numeric(NA)]

  # ---- Precompute static structures ------------------------------------------
  wlc_sa1 <- workLocationCounter$sa1_maincode_2016          # sa1-sorted, unique
  workLocationCounter_vec <- integer(length(wlc_sa1))       # all zero
  globalDistCounter_vec   <- integer(280)                   # all zero
  # sa3DistCounter stays the [40,40,280] integer array (O(1) indexed updates)

  # Static per-work-SA3 candidate skeleton: sa1 order == keyed (sa1-sorted)
  # distanceMatrixIndexWork[sa3==g], inner-joined to workLocationsSA1 (drops none
  # in practice; replicated to be exact). Columns: sa1, work_idx (dmw column),
  # wl_pos (row in workLocationCounter_vec), wl_pr (work_location_pr).
  global_pr_vec <- work_hist_global$global_dist_pr          # index = distance 1..280
  SK <- new.env(parent = emptyenv())
  for (g in sort(unique(workers_sa1$sa3_work))) {
    cand <- distanceMatrixIndexWork[sa3 == g]               # sa1-sorted
    cand[, work_location_pr := workLocationsSA1[.(cand$sa1_maincode_2016),
                                                work_location_pr,
                                                on = .(sa1_maincode_2016)]]
    cand <- cand[!is.na(work_location_pr)]                  # inner_join semantics
    assign(as.character(g), list(
      sa1      = cand$sa1_maincode_2016,
      work_idx = cand$index,
      wl_pos   = match(cand$sa1_maincode_2016, wlc_sa1),
      wl_pr    = cand$work_location_pr,
      n        = nrow(cand)
    ), envir = SK)
  }

  # Static histogram lookups keyed by (sa3_home, sa3_work); columns 3..282 of the
  # wide table correspond to distance bins 1..280 (as the original assumed).
  HSkey <- paste(work_hist_sa3_wide$sa3_home, work_hist_sa3_wide$sa3_work)
  HSmat <- as.matrix(work_hist_sa3_wide[, 3:282])

  # Per-worker precomputed lookups (vectorised once).
  n <- if (is.infinite(max_workers)) nrow(workers_sa1) else min(max_workers, nrow(workers_sa1))
  sa1_home_v   <- workers_sa1$SA1_MAINCODE_2016
  sa3_home_v   <- as.integer(substr(sa1_home_v, 1, 5))
  sa3_work_v   <- workers_sa1$sa3_work
  home_index_v <- distanceMatrixIndex[.(as.integer64(sa1_home_v)), index,
                                      on = .(sa1_maincode_2016)]
  i1_v <- sa3DistCounterIndex[.(sa3_home_v), index, on = .(sa3)]
  i2_v <- sa3DistCounterIndex[.(as.integer(sa3_work_v)), index, on = .(sa3)]
  hsrow_v <- match(paste(sa3_home_v, sa3_work_v), HSkey)
  skkey_v <- as.character(sa3_work_v)

  sa1_work_out <- rep(NA_real_, nrow(workers_sa1))

  if (!is.null(oracle)) {
    oracle <- as.data.table(oracle)
    stopifnot(all(oracle$PlanId == workers_sa1$PlanId))     # same processing order
    oracle_sa1 <- as.double(oracle$sa1_work)
  }

  pb <- progress_bar$new(
    format = "  [:bar] :current/:total (:percent; eta: :eta; :rate)",
    total = n, clear = FALSE, width = 78
  )

  start_time <- Sys.time()
  for (i in seq_len(n)) {
    sk <- get(skkey_v[i], envir = SK)
    home_index <- home_index_v[i]
    i1 <- i1_v[i]; i2 <- i2_v[i]

    dist_all <- distanceMatrixWork[home_index, sk$work_idx]   # bins 1..327
    # distance_proportion computed over ALL candidates (matches original)
    cnt <- tabulate(dist_all)
    dp_all <- 1 / cnt[dist_all]

    keep <- dist_all <= 280                                   # hist joins drop >280
    d      <- dist_all[keep]
    sa1    <- sk$sa1[keep]
    wl_pos <- sk$wl_pos[keep]
    wl_pr  <- sk$wl_pr[keep]
    dp     <- dp_all[keep]

    sd_pr <- HSmat[hsrow_v[i], d]
    if (sum(sd_pr) > 0) sd_pr <- sd_pr / sum(sd_pr)
    gd_pr <- global_pr_vec[d]
    gd_pr <- gd_pr / sum(gd_pr, na.rm = TRUE)

    wl_act <- workLocationCounter_vec[wl_pos]
    sd_act <- sa3DistCounter[i1, i2, d]
    gd_act <- globalDistCounter_vec[d]

    wl_adj <- adjustPr(wl_pr, wl_act)
    sd_adj <- adjustPr(sd_pr, sd_act)
    gd_adj <- adjustPr(gd_pr, gd_act)

    overal <- (wl_adj + 2 * sd_adj + 2 * gd_adj) / dp
    overal <- overal / sum(overal, na.rm = TRUE)

    sel <- sample.int(length(sa1), size = 1, prob = overal)
    destinationSA1 <- sa1[sel]
    distanceDestination <- d[sel]

    workLocationCounter_vec[wl_pos[sel]] <- workLocationCounter_vec[wl_pos[sel]] + 1L
    sa3DistCounter[i1, i2, distanceDestination] <- sa3DistCounter[i1, i2, distanceDestination] + 1L
    globalDistCounter_vec[distanceDestination] <- globalDistCounter_vec[distanceDestination] + 1L

    sa1_work_out[i] <- as.double(destinationSA1)

    if (!is.null(oracle)) {
      if (!identical(sa1_work_out[i], oracle_sa1[i])) {
        cat(sprintf("\nDIVERGENCE at worker i=%d (PlanId=%s): fast=%.0f oracle=%.0f\n",
                    i, format(workers_sa1$PlanId[i]), sa1_work_out[i], oracle_sa1[i]))
        return(invisible(list(diverged_at = i)))
      }
    }
    pb$tick()
  }
  log_info(paste0("Processed ", n, " workers in ",
                  round(difftime(Sys.time(), start_time, units = "mins"), 2), " min"))

  if (!is.null(oracle)) {
    cat(sprintf("\nVERIFIED: first %d assignments bit-identical to oracle.\n", n))
  }

  workers_sa1[, sa1_work := sa1_work_out]

  if (save_outputs && is.infinite(max_workers)) {
    log_info("Saving datasets")
    workLocationCounter$count <- workLocationCounter_vec
    globalDistCounter$count   <- globalDistCounter_vec
    saveRDS(workLocationCounter, paste0(outputDir,"/workLocationCounter_balanced.rds"))
    saveRDS(sa3DistCounter     , paste0(outputDir,"/sa3DistCounter_balanced.rds"     ))
    saveRDS(globalDistCounter  , paste0(outputDir,"/globalDistCounter_balanced.rds"  ))
    saveRDS(workers_sa1        , paste0(outputDir,"/workers_sa1_balance1.rds"        ))
  }
  return(invisible(workers_sa1))
}
