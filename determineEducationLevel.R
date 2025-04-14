suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(logger))

determineEducationLevel <- function(population) {
 
  log_info("Commencing education level assignment")
  
  population_education_count <- population %>%
    group_by(SA2_MAINCODE,Gender,age_cat) %>%
    summarise(pop_count=n())
  
  education_status <- read.csv("abs/melb_sa2_eduHEAP_age_gender_2016.csv")%>%
    select(SA2_MAINCODE=SA2,Age,Gender=Sex,low,medium,high)%>%
    mutate(tot=low+medium+high) %>%
    mutate(low_percent=ifelse(is.nan(low/tot), 0, low/tot),
           medium_percent=ifelse(is.nan(medium/tot), 0, medium/tot),
           high_percent=ifelse(is.nan(high/tot), 0, high/tot),) %>%
    fill(SA2_MAINCODE, .direction="down") %>%
    mutate(Age=ifelse(Age=="",NA,Age)) %>%
    fill(Age, .direction="down") %>%
    mutate(age_cat = case_when(Age == "0-4 years"   ~  1,
                                      Age == "5-9 years"   ~  2,
                                      Age == "10-14 years" ~  3,
                                      Age == "15-19 years" ~  4,
                                      Age == "20-24 years" ~  5,
                                      Age == "25-29 years" ~  6,
                                      Age == "30-34 years" ~  7,
                                      Age == "35-39 years" ~  8,
                                      Age == "40-44 years" ~  9,
                                      Age == "45-49 years" ~ 10,
                                      Age == "50-54 years" ~ 11,
                                      Age == "55-59 years" ~ 12,
                                      Age == "60-64 years" ~ 13,
                                      Age == "65-69 years" ~ 14,
                                      Age == "70-74 years" ~ 15,
                                      Age == "75-79 years" ~ 16,
                                      Age == "80-84 years" ~ 17,
                                      Age == "85-89 years" ~ 18,
                                      Age == "90-94 years" ~ 19,
                                      Age == "95-99 years" ~ 20,
                                      Age == "100 years and over" ~ 21)) %>%
    select(SA2_MAINCODE,Gender,age_cat,low_percent,medium_percent,high_percent)
  
  education_count <- population_education_count %>%
    left_join(education_status, by=c("SA2_MAINCODE","Gender","age_cat")) %>%
    mutate(low_count=round(pop_count*low_percent),
           medium_count=pmin(pop_count-low_count,round(pop_count*medium_percent)),
           high_count=pmin(pop_count-low_count-medium_count,round(pop_count*high_percent))) %>%
    select(SA2_MAINCODE,Gender,age_cat,pop_count,low_count,medium_count,high_count)
  
  
  set.seed(12)
  
  log_info(paste0("Performing join on ", nrow(population), " sampled persons"))
  
  population_education_joined <- population %>%
    inner_join(education_count, by=c("SA2_MAINCODE","Gender","age_cat")) %>%
    arrange(SA2_MAINCODE,Gender,age_cat) %>%
    group_by(SA2_MAINCODE,Gender,age_cat) %>%
    rowwise() %>%
    mutate(random_sample=round(runif(1, min=1, max=pop_count)))%>%
    mutate(education = ifelse(low_count>=random_sample, "low",
                              ifelse(medium_count+low_count>=random_sample,"medium","high"))) %>%
    arrange(SA2_MAINCODE,Gender,age_cat,random_sample) %>%
    select(-pop_count,-low_count,-medium_count,-high_count,-random_sample)
  
  log_info(paste0("Wrote ", nrow(population_education_joined), " sampled persons to DataFrame"))
  return(population_education_joined)
  
}