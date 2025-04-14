suppressPackageStartupMessages(library(dplyr))
# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

# Probabilistically selects an index from the vector of probabilities
selectIndexFromProbabilities <-function(vv) {
  if(is.null(vv) || is.na(vv)) return(vv)
  if(length(vv)==0 || length(vv)==1) return(length(vv))
  vv<-vv/sum(vv) # normalise to 1
  v<-cumsum(vv) # cumulative sum to 1
  roll<-runif(1)
  select<-match(TRUE,v>roll) # pick the first col that is higher than the dice roll
  return(select)
}

# Timestamped console output
echo<- function(msg) {
  cat(paste0(as.character(Sys.time()), ' | ', msg))
}

# Progress bar
printProgress<-function(row, char, majorInterval=100, minorInterval=10) {
  if(is.null(row) || is.na(row) || !is.numeric(row)) return()
  if((row-1)%%majorInterval==0) echo('')
  cat(char)
  if(row%%minorInterval==0) cat('|')
  if(row%%majorInterval==0) cat(paste0(' ', row,'\n'))
}

getGroupIds<-function(filterCsv) {
  groups<- getGroups(filterCsv)
  groupIds <- unique(groups$cluster_id_5)
  return(groupIds)
}

getGroups<-function(filterCsv) {
  gz1 <- gzfile(filterCsv,'rt')
  data<-read.csv(gz1,header = T,sep=',',stringsAsFactors = F,strip.white = T)
  close(gz1)
  
  datacols<-c("sex",
              "min_age",
              "max_age",
              "cluster_id_5")
  
  filters <- data[,datacols] %>%
    group_by(cluster_id_5,sex) %>%
    summarise(age_start=min(min_age), age_end=max(max_age)) 
  
  return(filters)
}

get_age_cat_int_from_str <- function(age_str) {
    age_mapping <- list(
        "0-4 years"   = 1,
        "5-9 years"   = 2,
        "10-14 years" = 3,
        "15-19 years" = 4,
        "20-24 years" = 5,
        "25-29 years" = 6,
        "30-34 years" = 7,
        "35-39 years" = 8,
        "40-44 years" = 9,
        "45-49 years" = 10,
        "50-54 years" = 11,
        "55-59 years" = 12,
        "60-64 years" = 13,
        "65-69 years" = 14,
        "70-74 years" = 15,
        "75-79 years" = 16,
        "80-84 years" = 17,
        "85-89 years" = 18,
        "90-94 years" = 19,
        "95-99 years" = 20,
        "100 years and over" = 21
    )
    return(age_mapping[[age_str]])
}

get_age_cat_labels <- function() {
    age_labels <- c(
        "0-4 years",
        "5-9 years",
        "10-14 years",
        "15-19 years",
        "20-24 years",
        "25-29 years",
        "30-34 years",
        "35-39 years",
        "40-44 years",
         "45-49 years",
         "50-54 years",
         "55-59 years",
         "60-64 years",
         "65-69 years",
         "70-74 years",
         "75-79 years",
         "80-84 years",
         "85-89 years",
         "90-94 years",
         "95-99 years",
         "100 years and over"
    )
    return(age_labels)
}

get_age_cat_from_age <- function(Age) {
    # Determine the age category based on the age of the person
    age_category = case_when(
                Age <   5             ~  1,
                Age >=  5 & Age <=  9 ~  2,
                Age >= 10 & Age <= 14 ~  3,
                Age >= 15 & Age <= 19 ~  4, 
                Age >= 20 & Age <= 24 ~  5,
                Age >= 25 & Age <= 29 ~  6, 
                Age >= 30 & Age <= 34 ~  7, 
                Age >= 35 & Age <= 39 ~  8, 
                Age >= 40 & Age <= 44 ~  9,
                Age >= 45 & Age <= 49 ~ 10, 
                Age >= 50 & Age <= 54 ~ 11, 
                Age >= 55 & Age <= 59 ~ 12, 
                Age >= 60 & Age <= 64 ~ 13, 
                Age >= 65 & Age <= 69 ~ 14,
                Age >= 70 & Age <= 74 ~ 15, 
                Age >= 75 & Age <= 79 ~ 16,
                Age >= 80 & Age <= 84 ~ 17,
                Age >= 85 & Age <= 89 ~ 18,
                Age >= 90 & Age <= 94 ~ 19,
                Age >= 95 & Age <= 99 ~ 20,
                Age >= 100            ~ 21)
    return(age_category)
}
