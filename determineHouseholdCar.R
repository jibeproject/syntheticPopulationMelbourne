suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(logger))

determineHouseholdCar <- function(population) {
 
  log_info("Commencing household size and car attribute assignment")
  
  population <- population%>%
    left_join(read.csv("abs/melb_sa1_IRSAD_2016.csv")%>%
                select(SA1_7DIGCODE,IRSAD))%>%
    mutate(IRSAD = as.numeric(IRSAD))
  
  household <- population %>%
    group_by(HouseholdId) %>%
    summarise(SA2_MAINCODE=unique(SA2_MAINCODE),
              hhSize=n())
  
  household_count <- household%>%
    group_by(SA2_MAINCODE,hhSize)%>%
    summarise(hh_count=n())
  

    
    householdCar_status <- read.csv("abs/melb_sa2_hhSize_hhCar_2016.csv")%>%
      select(SA2_MAINCODE=SA2,hhSize,car_0,car_1,car_2,car_3,car_4)%>%
      mutate(tot=car_0+car_1+car_2+car_3+car_4) %>%
      mutate(car_0_percent=ifelse(is.nan(car_0/tot), 0, car_0/tot),
             car_1_percent=ifelse(is.nan(car_1/tot), 0, car_1/tot),
             car_2_percent=ifelse(is.nan(car_2/tot), 0, car_2/tot),
             car_3_percent=ifelse(is.nan(car_3/tot), 0, car_3/tot),
             car_4_percent=ifelse(is.nan(car_4/tot), 0, car_4/tot)) %>%
      mutate(SA2_MAINCODE=ifelse(SA2_MAINCODE=="",NA,as.integer(SA2_MAINCODE))) %>%
      fill(SA2_MAINCODE, .direction="down") %>%
      fill(hhSize, .direction="down") %>%
      filter(hhSize!="Not applicable") %>%
      mutate(hhSize_cat = case_when(hhSize == "One person"   ~  1,
                                    hhSize == "Two persons"   ~  2,
                                    hhSize == "Three persons" ~  3,
                                    hhSize == "Four persons" ~  4,
                                    hhSize == "Five persons" ~  5,
                                    hhSize == "Six persons" ~  6,
                                    hhSize == "Seven persons" ~  7,
                                    hhSize == "Eight or more persons" ~  8)) %>%
      select(SA2_MAINCODE,hhSize_cat,car_0_percent,car_1_percent,car_2_percent,car_3_percent,car_4_percent)
    
   
    household_count_joined <- household_count %>%
      left_join(householdCar_status, by=c("SA2_MAINCODE","hhSize"="hhSize_cat"))%>%
      mutate(car_0_count=round(hh_count*car_0_percent),
           car_1_count=pmin(hh_count-car_0_percent,round(hh_count*car_1_percent)),
           car_2_count=pmin(hh_count-car_0_percent-car_1_percent,round(hh_count*car_2_percent)),
           car_3_count=pmin(hh_count-car_0_percent-car_1_percent-car_2_percent,round(hh_count*car_3_percent)),
           car_4_count=pmin(hh_count-car_0_percent-car_1_percent-car_2_percent-car_3_percent,round(hh_count*car_4_percent))) %>%
    select(SA2_MAINCODE,hhSize,hh_count,car_0_count,car_1_count,car_2_count,car_3_count,car_4_count)
  
  
  
  set.seed(12)
  
  log_info(paste0("Performing join on ", nrow(household), " sampled persons"))
  
  household_hhCar_joined <- household %>%
    inner_join(household_count_joined, by=c("SA2_MAINCODE","hhSize")) %>%
    arrange(SA2_MAINCODE,hhSize) %>%
    group_by(SA2_MAINCODE,hhSize) %>%
    rowwise() %>%
    mutate(random_sample=round(runif(1, min=1, max=hh_count)))%>%
    mutate(hhCar = case_when(car_0_count>=random_sample ~ 0,
                             car_0_count+car_1_count>=random_sample ~ 1,
                             car_0_count+car_1_count+car_2_count>=random_sample ~ 2,
                             car_0_count+car_1_count+car_2_count+car_3_count>=random_sample ~ 3,
                             car_0_count+car_1_count+car_2_count+car_3_count+car_4_count>=random_sample  ~ 4)) %>%
    arrange(SA2_MAINCODE,hhSize,random_sample) %>%
    select(-hh_count,-car_0_count,-car_1_count,-car_2_count,-car_3_count,-car_4_count,-random_sample)
  
  ## when hh car distribution isn't availbale at SA2 by hh size, then assign number of cars by hh size only (273 household has NA cars).
  householdCar_status_bySize <- read.csv("abs/melb_hhSize_hhCar_2016.csv")%>%
    select(hhSize,car_0,car_1,car_2,car_3,car_4)%>%
    mutate(tot=car_0+car_1+car_2+car_3+car_4) %>%
    mutate(car_0_percent=ifelse(is.nan(car_0/tot), 0, car_0/tot),
           car_1_percent=ifelse(is.nan(car_1/tot), 0, car_1/tot),
           car_2_percent=ifelse(is.nan(car_2/tot), 0, car_2/tot),
           car_3_percent=ifelse(is.nan(car_3/tot), 0, car_3/tot),
           car_4_percent=ifelse(is.nan(car_4/tot), 0, car_4/tot)) %>%
    fill(hhSize, .direction="down") %>%
    filter(hhSize!="Not applicable") %>%
    mutate(hhSize_cat = case_when(hhSize == "One person"   ~  1,
                                  hhSize == "Two persons"   ~  2,
                                  hhSize == "Three persons" ~  3,
                                  hhSize == "Four persons" ~  4,
                                  hhSize == "Five persons" ~  5,
                                  hhSize == "Six persons" ~  6,
                                  hhSize == "Seven persons" ~  7,
                                  hhSize == "Eight or more persons" ~  8)) %>%
    select(hhSize_cat,car_0_percent,car_1_percent,car_2_percent,car_3_percent,car_4_percent)
  
  household_hhCar_joined_NA <- household_hhCar_joined%>%
    filter(is.na(hhCar))%>%
    left_join(householdCar_status_bySize,by=c("hhSize"="hhSize_cat"))%>%
    mutate(random_sample=runif(1, min=0, max=1))%>%
    mutate(hhCar = case_when(car_0_percent>=random_sample ~ 0,
                             car_0_percent+car_1_percent>=random_sample ~ 1,
                             car_0_percent+car_1_percent+car_2_percent>=random_sample ~ 2,
                             car_0_percent+car_1_percent+car_2_percent+car_3_percent>=random_sample ~ 3,
                             car_0_percent+car_1_percent+car_2_percent+car_3_percent+car_4_percent>=random_sample  ~ 4))%>%
    arrange(SA2_MAINCODE,hhSize,random_sample) %>%
    select(c(1:4))
  
  household_hhCar_joined <-household_hhCar_joined%>%
    filter(!is.na(hhCar))%>%
    bind_rows(household_hhCar_joined_NA)
  
  population_hhCar_joined <- population%>%
    left_join(household_hhCar_joined%>%
                select(HouseholdId,hhCar))
  
  log_info(paste0("Wrote ", nrow(population_hhCar_joined), " sampled persons to DataFrame"))
  return(population_hhCar_joined)
  
}