suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(logger))
# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

prepWorkData <- function(outputDir) {
  
  log_info("Prepare mode choice by SA3")
  sa3_work_mode <- read.csv("abs/sa3_work_mode.csv") %>%
    select(sa3,age_group="MTWP.Method.of.Travel.to.Work",wfh=WFH,walk=WALK,bike=BIKE,pt=PT,car=CAR) %>%
    fill(sa3)
  
  log_info("Evaluate distance distributions between SA3 pairs")
  sa3_work_dist <- read.csv("abs/sa3_to_sa3_work_distances.csv") %>%
    fill(sa3_home)
  
  colnames(sa3_work_dist) <- 
    c("sa3_home","sa3_work","nil","0-0.5","0.5-1","1-1.5","1.5-2","2-2.5","2.5-3","3-4",
      "4-5","5-6","6-7","7-8","8-9","9-10","10-11","11-12","12-13","13-14",
      "14-15","15-16","16-17","17-18","18-19","19-20","20-21","21-22","22-23",
      "23-24","24-25","25-26","26-27","27-28","28-29","29-30","30-32","32-34",
      "34-36","36-38","38-40","40-42","42-44","44-46","46-48","48-50","50-52",
      "52-54","54-56","56-58","58-60","60-62","62-64","64-66","66-68","68-70",
      "70-72","72-74","74-76","76-78","78-80","80-85","85-90","90-95","95-100",
      "100-110","110-120","120-130","130-140","140-150","150-200","200-250",
      "250-300","300-350","350-400","400-600","600-800","800-1000",
      "1000-3000","3000-inf","na")
  # max distance is 164km, we will ignore everything above that.
  
  ranges_df <- data.frame(range=c(
    "0-0.5","0.5-1","1-1.5","1.5-2","2-2.5","2.5-3","3-4","4-5","5-6","6-7","7-8",
    "8-9","9-10","10-11","11-12","12-13","13-14","14-15","15-16","16-17","17-18",
    "18-19","19-20","20-21","21-22","22-23","23-24","24-25","25-26","26-27",
    "27-28","28-29","29-30","30-32","32-34","34-36","36-38","38-40","40-42",
    "42-44","44-46","46-48","48-50","50-52","52-54","54-56","56-58","58-60",
    "60-62","62-64","64-66","66-68","68-70","70-72","72-74","74-76","76-78",
    "78-80","80-85","85-90","90-95","95-100","100-110","110-120","120-130",
    "130-140","140-150","150-200"
  ),range_value=c(
    0.25,0.75,1.25,1.75,2.25,2.75,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,
    13.5,14.5,15.5,16.5,17.5,18.5,19.5,20.5,21.5,22.5,23.5,24.5,25.5,26.5,27.5,
    28.5,29.5,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73,
    75,77,79,82.5,87.5,92.5,97.5,105,115,125,135,145,175    
  ))
  
  log_info("Prepare distance histograms")
  log_info("... distributes values evenly across narrower ranges.")
  # This makes the histograms more even
  expandRange <- function(df,range_min,range_max,source_spacing,target_spacing=1) {
    offset_value <- (source_spacing/2) - (target_spacing/2)
    df %>%
      dplyr::filter(range_value>=range_min & range_value<=range_max) %>%
      inner_join(tidyr::crossing(
        data.frame(range_value=seq(range_min,range_max,source_spacing)),
        data.frame(offset=seq(offset_value*-1,offset_value,target_spacing))),
        by="range_value",
        relationship = "many-to-many") %>%
      mutate(range_value=range_value+offset) %>%
      mutate(count=count/(source_spacing/target_spacing)) %>%
      dplyr::select(-offset)
  }
  
  log_info("... remove distances that are too large")
  sa3_work_dist2 <- sa3_work_dist %>%
    dplyr::select(-"nil",-"na",-"200-250",-"250-300",-"300-350",-"350-400",-"400-600",
                  -"600-800",-"800-1000",-"1000-3000",-"3000-inf") %>%
    pivot_longer(cols="0-0.5":"150-200",names_to="range",values_to="count") %>%
    left_join(ranges_df, by="range")
  
  log_info("... work distance histogram with even bin widths and in long format")
  sa3_work_dist_hist <- bind_rows(
    expandRange(sa3_work_dist2,  0.25,  2.75, 0.5,0.5),
    expandRange(sa3_work_dist2,  3.5 , 29.5 , 1  ,0.5),
    expandRange(sa3_work_dist2, 31   , 79   , 2  ,0.5),
    expandRange(sa3_work_dist2, 82.5 , 97.5 , 5  ,0.5),
    expandRange(sa3_work_dist2,105   ,145   ,10  ,0.5),
    expandRange(sa3_work_dist2,175   ,275   ,50  ,0.5)
    # anything above 300km isn't considered
  ) %>%
    filter(range_value<=140) %>% # no result over 140km
    arrange(sa3_home,sa3_work,range_value) %>%
    select(-range)
  
  log_info("... the distance histogram for the entire study region")
  work_hist_global <- sa3_work_dist_hist %>%
    group_by(range_value) %>%
    summarise(count=sum(count,na.rm=T)) %>%
    mutate(pr=count/sum(count,na.rm=T)) %>%
    mutate(pr=ifelse(is.nan(pr),0,pr))
  
  log_info("... adding a distance probability for the regional movement")
  work_hist_sa3 <- sa3_work_dist_hist %>%
    group_by(sa3_home,sa3_work) %>%
    mutate(pr=count/sum(count,na.rm=T)) %>%
    mutate(pr=ifelse(is.nan(pr),0,pr)) %>%
    ungroup()
  
  log_info("... the probability of going from one region to another")
  work_sa3_movement <- sa3_work_dist_hist %>%
    group_by(sa3_home,sa3_work) %>%
    summarise(count=sum(count,na.rm=T)) %>%
    mutate(pr_sa3=count/sum(count,na.rm=T)) %>%
    mutate(pr_sa3=ifelse(is.nan(pr_sa3),0,pr_sa3)) %>%
    ungroup() %>%
    mutate(pr_global=count/sum(count,na.rm=T)) %>%
    mutate(pr_global=ifelse(is.nan(pr_global),0,pr_global))
  
  
  log_info("... saving the files")
  saveRDS(work_hist_global, paste0(outputDir,"/work_hist_global.rds"))
  saveRDS(work_hist_sa3,    paste0(outputDir,"/work_hist_sa3.rds"))
  saveRDS(work_sa3_movement,paste0(outputDir,"/work_sa3_movement.rds"))
  
  log_info("Reading, transforming and writing SA1 work locations")  
  workLocationsSA1 <- read.csv(file="data/SA1attributed.csv.gz") %>%
    select(sa1_maincode_2016,global_pr=work) %>%
    filter(!is.na(global_pr))
  workLocationsSA1$sa3 <- as.integer(substr(workLocationsSA1$sa1_maincode_2016,1,5))
  workLocationsSA1 <- workLocationsSA1 %>%
    group_by(sa3) %>%
    mutate(sa3_pr=global_pr/sum(global_pr,na.rm=T)) %>%
    ungroup() %>%
    select(sa1_maincode_2016,sa3,global_pr,sa3_pr)
  write.csv(workLocationsSA1,paste0(outputDir,"/workLocationsSA1.csv"),row.names=F)
  
  log_info("Reading, transforming and writing SA1-SA3 distance matrix")
  distanceMatrixIndex <<- read.csv(file="data/distanceMatrixIndex.csv")
  distanceMatrixIndex$sa3 <- as.integer(substr(distanceMatrixIndex$sa1_maincode_2016,1,5))
  write.csv(distanceMatrixIndex,paste0(outputDir,"/distanceMatrixIndex.csv"),row.names=F)
  
  log_info("Preparing and writing SA1 distance matrix for work")
  distanceMatrixIndexWork <- distanceMatrixIndex %>%
    filter(sa1_maincode_2016%in%workLocationsSA1$sa1_maincode_2016)
  
  distanceMatrixIndexWorkIndicies <- distanceMatrixIndexWork$index %>% unique() %>% sort()
  indiciesTable <- data.table(index=distanceMatrixIndexWorkIndicies,index_new=seq(1,length(distanceMatrixIndexWorkIndicies)))
  
  distanceMatrixIndexWork <- distanceMatrixIndexWork %>%
    inner_join(indiciesTable, by=join_by(index)) %>%
    select(sa1_maincode_2016,index=index_new,sa3)
  write.csv(distanceMatrixIndexWork,paste0(outputDir,"/distanceMatrixIndexWork.csv"),row.names=F)
  
  
  
  log_info("Select the number of work locations for each sa3->sa3.")
  # Make sure they're equal to the number of homes!
  
  distanceMatrix <<- readRDS(file="data/distanceMatrix.rds") # note '<<' to make it global
  
  # Convert the matrix to a data.table in long format
  distanceMatrixDT <- as.data.table(as.table(distanceMatrix)) # Convert matrix to long format

  # Perform the interval calculation in a vectorized way
  distanceMatrixDT[, V3 := findInterval(N, vec = seq(0, 163500, 500))]

  # Convert back to a matrix
  distanceMatrixInt <- matrix(
    distanceMatrixDT$V3, 
    nrow = nrow(distanceMatrix), 
    ncol = ncol(distanceMatrix), 
    dimnames = dimnames(distanceMatrix)
  )

  # Save the result
  saveRDS(distanceMatrixInt, paste0(outputDir, "/distanceMatrixBins.rds"))
  
  distanceMatrixWork <- distanceMatrixInt[,distanceMatrixIndexWorkIndicies]
  saveRDS(distanceMatrixWork,paste0(outputDir,"/distanceMatrixWork.rds"))
  
}
