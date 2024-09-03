# libraries and functions -------------------------------------------------
library(dplyr)
library(tidyr)
library(data.table)

assignWorkLocations <- function(outputDir) {
  
  # import data -------------------------------------------------------------
  # outputDir="output"
  work_hist_global <- readRDS(paste0(outputDir,"/work_hist_global.rds")) %>%
    mutate(distance=row_number()) %>%
    select(distance,global_dist_pr=pr) %>%
    data.table()
  
  work_hist_sa3 <- readRDS(paste0(outputDir,"/work_hist_sa3.rds"))
  work_sa3_movement <- readRDS(paste0(outputDir,"/work_sa3_movement.rds"))
  workers <- readRDS(paste0(outputDir,'/populationEmployed.rds')) %>%
    ungroup() %>%
    data.frame() %>%
    filter(is_employed) %>%
    mutate(PlanId=AgentId) %>%
    dplyr::select(PlanId,SA1_MAINCODE_2016)
  workers$sa3_home <- as.integer(substr(workers$SA1_MAINCODE_2016,1,5))
  workLocationsSA1 <- read.csv(paste0(outputDir,"/workLocationsSA1.csv")) %>%
    select(sa1_maincode_2016,work_location_pr=sa3_pr)
  workLocationsSA1<-data.table(workLocationsSA1)
  setkey(workLocationsSA1, sa1_maincode_2016)
  
  
  
  
  #distances
  # distanceMatrix <<- readRDS(file="data/distanceMatrix.rds") # note '<<' to make it global
  distanceMatrixWork <<- readRDS(file=paste0(outputDir,"/distanceMatrixWork.rds")) # note '<<' to make it global
  # Some SA1s ended up snapping their centroid to the same node in the road
  # network so we need to use an index.
  distanceMatrixIndex <- read.csv(paste0(outputDir,"/distanceMatrixIndex.csv"))
  distanceMatrixIndex<-data.table(distanceMatrixIndex)
  setkey(distanceMatrixIndex, sa1_maincode_2016)
  distanceMatrixIndexWork <- read.csv(paste0(outputDir,"/distanceMatrixIndexWork.csv"))
  distanceMatrixIndexWork<-data.table(distanceMatrixIndexWork)
  setkey(distanceMatrixIndexWork, sa1_maincode_2016)
  
  # assign work SA3 regions -------------------------------------------------
  work_hist_sa3_wide <- work_hist_sa3 %>%
    pivot_wider(id_cols=c(sa3_home,sa3_work),
                names_from=range_value,
                values_from=pr) %>%
    data.table()
  
  # tmp<-work_hist_sa3 %>%
  #   filter(is.nan(pr))
  
  home_count_sa3 <- workers %>%
    mutate(sa3_home=as.integer(substr(workers$SA1_MAINCODE_2016,1,5))) %>%
    group_by(sa3_home) %>%
    summarise(home_count=n()) %>%
    ungroup()
  
  
  # calculate home to work sa3 counts
  work_count_sa3 <- work_sa3_movement %>%
    select(sa3_home,sa3_work,pr_sa3) %>%
    inner_join(home_count_sa3) %>%
    # filter(sa3_home==20601) %>%
    group_by(sa3_home) %>%
    mutate(work_count=roundPreserveSum(home_count*pr_sa3)) %>%
    ungroup() %>%
    select(sa3_home,sa3_work,work_count)
  # summarise(home_count=max(home_count),
  #           work_count=sum(work_count))
  
  set.seed(10000)
  work_sa3 <- work_count_sa3 %>%
    uncount(weights=work_count) %>%
    group_by(sa3_home) %>%
    mutate(sa3_order=sample(1:n())) %>%
    ungroup()
  
  workers_sa3 <- workers %>%
    arrange(PlanId) %>%
    group_by(sa3_home) %>%
    mutate(sa3_order=row_number()) %>%
    ungroup() %>%
    left_join(work_sa3, by=c("sa3_home","sa3_order"))
  
  
  
  
  # balanced ----------------------------------------------------------------
  
  set.seed(10000)
  
  workLocationCounter <<- distanceMatrixIndexWork%>%select(sa1_maincode_2016)%>%mutate(count=0)
  sa3DistCounter <<- array(data=0, dim=c(40, 40, 280))
  sa3DistCounterIndex <<- data.table(sa3=distanceMatrixIndexWork$sa3%>%unique()%>%sort())%>%mutate(index=row_number())
  setkey(sa3DistCounterIndex, sa3)
  globalDistCounter <<- data.table(distance=1:280,count=0)
  
  # workers_sa1 <- workers_sa3[1:1000,] %>% mutate(sa1_work=NA)
  workers_sa1 <- workers_sa3[sample(nrow(workers_sa3)),] %>% mutate(sa1_work=as.numeric(NA))
  
  i<-0
  start_time <- Sys.time()
  
  while(i<nrow(workers_sa1)) {
    i<-i+1
    SA1_id <- workers_sa1$SA1_MAINCODE_2016[i]
    SA3_id <- workers_sa1$sa3_work[i]
    workPr <- getWorkPr(SA1_id,SA3_id) %>%
      mutate(overal_pr=(work_location_adj+2*sa3_dist_adj+2*global_dist_adj)/distance_proportion) %>%
      mutate(overal_pr=overal_pr/sum(overal_pr,na.rm=T)) %>%
      select(sa1_maincode_2016,distance,overal_pr)
    destinationSA1 <- sample(workPr$sa1_maincode_2016, size=1, prob=workPr$overal_pr)
    distanceDestination <- workPr[sa1_maincode_2016==destinationSA1]$distance
    setWorkCounters(SA1_id,destinationSA1,distanceDestination)
    workers_sa1[i,]$sa1_work <- destinationSA1
    if(i%%1000==0) cat(paste0("balanced ",i," at ",Sys.time(),"\n"))
  }
  end_time <- Sys.time()
  end_time - start_time
  
  saveRDS(workLocationCounter,paste0(outputDir,"/workLocationCounter_balanced.rds"))
  saveRDS(sa3DistCounter     ,paste0(outputDir,"/sa3DistCounter_balanced.rds"     ))
  saveRDS(globalDistCounter  ,paste0(outputDir,"/globalDistCounter_balanced.rds"  ))
  saveRDS(workers_sa1        ,paste0(outputDir,"/workers_sa1_balance1.rds"        ))
  
}

roundPreserveSum <- function(x) {
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y
}
# expected=tableAdjusted$global_dist_pr;  actual=tableAdjusted$global_dist_actual
# expected=distanceTableCounts$work_location_pr;actual=distanceTableCounts$work_location_actual
# expected=distanceTableCounts$sa3_dist_pr;actual=distanceTableCounts$sa3_dist_actual
# tmp<-data_frame(expected=expected,actual=actual,actual_normalised=actual_normalised,delta=delta)
adjustPr <- function(expected,actual) {
  # expected=c(0.1,0.3,0.5,0.1);actual=c(0,1,1,0)
  if (sum(expected)==0) expected[]<-1/length(expected)
  if (sum(expected)!=1) expected <- expected/sum(expected)
  if (sum(actual)==0) return(expected)
  actual_normalised <- actual/sum(actual)
  delta <- expected-actual_normalised
  delta[delta<0] <- 0
  # in case it fits perfectly
  if (sum(delta)==0) delta<-expected
  
  delta_normalised <- delta/sum(delta)
  return(delta_normalised)
}

getDistCountSA3 <- function(sa3_1,sa3_2) {
  # sa3_1=SA3_home;sa3_2=SA3_id
  index_1=sa3DistCounterIndex[.(as.numeric(sa3_1))] %>% pull(index)
  index_2=sa3DistCounterIndex[.(as.numeric(sa3_2))] %>% pull(index)
  distanceCount <- data.table(distance=1:280,count=sa3DistCounter[index_1,index_2,])
  return(distanceCount)
}
getWorkPr <- function(SA1_id,SA3_id) {
  # SA1_id=20607113908;SA3_id=21304
  
  SA3_home <- as.integer(substr(SA1_id,1,5))
  
  # calculating distances
  index <- distanceMatrixIndex[.(as.numeric(SA1_id))] %>%
    pull(index)
  distanceTable <- distanceMatrixIndexWork[sa3 == as.numeric(SA3_id)]
  distanceTable$distance <- distanceMatrixWork[index,distanceTable$index]
  distanceTally <- distanceTable %>%
    group_by(distance) %>%
    summarise(distance_proportion=1/n())
  # adding distance_proportion (some distances are more common than others)
  distanceTable <- distanceTable %>%
    inner_join(distanceTally,by="distance")
  # adding work location Pr
  distanceTable <- distanceTable %>%
    inner_join(workLocationsSA1,by="sa1_maincode_2016")
  
  # adding local/SA3 distance Pr
  hist_sa3 <- work_hist_sa3_wide %>%
    filter(sa3_home==SA3_home,sa3_work==SA3_id)
  hist_sa3 <- data.table(distance=1:280,sa3_dist_pr=as.numeric(hist_sa3[1,3:282]))
  distanceTable <- distanceTable %>%
    inner_join(hist_sa3,by="distance") 
  if(sum(distanceTable$sa3_dist_pr)>0) {
    distanceTable %>%
      mutate(sa3_dist_pr=sa3_dist_pr/sum(sa3_dist_pr,na.rm=T))
  }
  
  #adding global distance Pr
  distanceTable <- distanceTable %>%
    inner_join(work_hist_global,by="distance") %>%
    mutate(global_dist_pr=global_dist_pr/sum(global_dist_pr,na.rm=T))
  
  
  
  # now have the raw probabilities 
  # sa1_maincode_2016, distance, distance_proportion, work_location_pr, sa3_dist_pr, global_dist_pr
  distanceTable <- distanceTable[, .(sa1_maincode_2016, distance, distance_proportion, work_location_pr, sa3_dist_pr, global_dist_pr)]
  
  #adding the counts
  distanceTableCounts <- distanceTable %>%
    # work location counter
    inner_join(workLocationCounter%>%rename(work_location_actual=count),by="sa1_maincode_2016") %>%
    # sa3 distance counter
    inner_join(getDistCountSA3(SA3_home,SA3_id)%>%rename(sa3_dist_actual=count),by="distance") %>%
    # global distance counter
    inner_join(globalDistCounter%>%rename(global_dist_actual=count),by="distance")
  
  # adjust for actual counts
  tableAdjusted <- distanceTableCounts
  
  tableAdjusted$work_location_adj<-adjustPr(tableAdjusted$work_location_pr,tableAdjusted$work_location_actual)
  tableAdjusted$sa3_dist_adj     <-adjustPr(tableAdjusted$sa3_dist_pr,     tableAdjusted$sa3_dist_actual)
  tableAdjusted$global_dist_adj  <-adjustPr(tableAdjusted$global_dist_pr,  tableAdjusted$global_dist_actual)
  tableAdjusted <- tableAdjusted[, .(sa1_maincode_2016, distance, distance_proportion, work_location_adj, sa3_dist_adj, global_dist_adj)]
  
  return(tableAdjusted)
}

selectWorkSA1 <- function(dataTable) {
  dataTable<-workPr
  # select SA1
  destinationSA1 <- sample(dataTable$sa1_maincode_2016, size=1, prob=dataTable$overal_pr)
  
  # add to counters
  
  
  
  
  return(destinationSA1)
}

setWorkCounters <- function(home_sa1,work_sa1,distanceDestination) {
  # home_sa1=SA1_id;work_sa1=destinationSA1
  
  # work location counter
  currentRow <- which(workLocationCounter$sa1_maincode_2016==work_sa1)
  workLocationCounter[currentRow,2] <<- workLocationCounter[currentRow,2]+1
  # sa3 distance counter
  index_1=sa3DistCounterIndex[.(as.integer(substr(home_sa1,1,5)))] %>% pull(index)
  index_2=sa3DistCounterIndex[.(as.integer(substr(work_sa1,1,5)))] %>% pull(index)
  sa3DistCounter[index_1,index_2,distanceDestination] <<- sa3DistCounter[index_1,index_2,distanceDestination]+1
  # global distance counter
  globalDistCounter[distanceDestination,2] <<- globalDistCounter[distanceDestination,2]+1
}






