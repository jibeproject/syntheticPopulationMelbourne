library(tidyverse)
library(data.table)
library(sf)

# For testing
# test<- population[sample(nrow(population),20),]

# load general purpose utility functions
source("util.R")

# Load project properties (includes coordinate reference system)
properties_file <- "../project.properties"
source(properties_file)

# Set seed for reproducibility
set.seed(12)

# Load the zone system
zonesShapeile = "../melbourne/input/zonesShapefile/SA1_2016_AUST_MEL.shp"
zoneSystem <- st_read(zonesShapeile) %>% st_transform(crs = crs)

# ABS 2016 Census fulltime/parttime student status by SA2, Age group and gender
census_data_path = "abs/Melb 2016 - Student status by SA2 (UR) AGE5P SEXP/Melb 2016 - SA2 (UR), AGE5P - Age in Five Year Groups and SEXP cleaned.csv"

prepare_census_data <- function(census_data_path) {
    # ABS 2016 Census fulltime/parttime student status by SA2, Age group and gender
    # The three categories are: "SA2..UR.", "AGE5P", "SEXP"
    # The three relevant count columns are "Not.attending", "Full.time.student", "Part.time.student"
    # The full time and part time counts are summed to get the total student count
    # The sum of the three count columns is the total population denominator (not stated are excluded)
    # The probability of being a student in an SA2 given age and gender is calculated as the student count divided by the total population
    # Note that this is only used for persions aged 15 and older, persons aged 5 to 14 are assumed to be students
    census_data <- fread(census_data_path, check.names = TRUE)
    
    # Calculate total student count, total population, and student probability
    census_data[, total_student_count := Full.time.student + Part.time.student]
    census_data[, total_population := Not.attending + Full.time.student + Part.time.student]
    census_data[, student_probability := total_student_count / total_population]
    census_data[is.na(student_probability), student_probability := 0]
    
    # Convert age categories to integer
    census_data[, age_cat := get_age_cat_int_from_str(AGE5P)]
    
    # Select and rename columns
    census_data <- census_data[, .(SA2_MAINCODE = SA2..UR., AGE5P, age_cat, Gender = SEXP, total_student_count, total_population, student_probability)]
    
    return(census_data)
}

# ABS 2016 Census fulltime/parttime student status by SA2, Age group and gender
census_data <- prepare_census_data(census_data_path)

# 2019 Primary and secondary data have grade and gender-specific enrolment totals
enrolments_primary_secondary <- (read.csv(
    "data/schools/primary and secondary schools/primary_secondary_school_enrolments_locations.csv"
    ) %>% 
    st_as_sf(
    coords = c("X", "Y"),
    crs = 4326
    )) %>% st_transform(crs = crs) %>% 
    st_join(zoneSystem, join = st_intersects)

# 2018 Tertiary and technical higher education
enrolments_higher_education <- (read.csv(
        "data/schools/higher education/Melbourne_cleaned_higher_education_enrolments_2018_with_location.csv"
    ) %>% 
    st_as_sf(
        coords = c("longitude_4326", "latitude_4326"),
        crs = 4326
    )) %>% st_transform(crs = crs) %>% 
    st_join(zoneSystem, join = st_intersects)

get_grade_given_age <- function(age) {
    # The law in Victoria states that children must attend school from the age of 6. 
    # We will assume that children aged 5 are in Prep, children aged 6 are in Grade 1, and so on.
    # Our primary/secondary enrolment data have overall and gender stratified enrolment totals for each grade,
    # as follows: Year.1.Males.Total	Year.1.Females.Total	Year.1.Total
    # The following lookup table will be used to determine the grade given the age of the student.
    age_grade_lookup <- c(
         "5" = "Prep",
         "6" = "Year.1",
         "7" = "Year.2",
         "8" = "Year.3",
         "9" = "Year.4",
        "10" = "Year.5",
        "11" = "Year.6",
        "12" = "Year.7",
        "13" = "Year.8",
        "14" = "Year.9",
        "15" = "Year.10",
        "16" = "Year.11",
        "17" = "Year.12"
    )
    return(age_grade_lookup[as.character(age)])
}

allocateSchools <- function(population) {
    population_schools <- copy(population)
    setDT(population_schools)
    
    # for persons aged 5 to 14, assign student status, grade and school type based on age
    # for persons aged 15 and higher, assign a school type based on probability of being a student
    # given age and gender according to the census data
    # the school types are 1 (primary), 2 (secondary), and 3 (higher education)
    
    population_schools[, age_cat := get_age_cat_from_age(Age)]
    
    # Perform a join to get student_probability
    population_schools <- merge(
        population_schools, 
        census_data, 
        by.x = c("SA2_MAINCODE", "age_cat", "Gender"), 
        by.y = c("SA2_MAINCODE", "age_cat", "Gender"), 
        all.x = TRUE
    )
    population_schools[, student_status := fifelse(Age < 5, FALSE,
                                        fifelse(Age >= 5 & Age <= 17, TRUE,
                                                fifelse(is.na(student_probability), FALSE, runif(.N) < student_probability)))]
    
    population_schools[, school_grade := fifelse(Age < 5, NA_character_,
                                        fifelse(Age > 17, NA_character_,
                                    get_grade_given_age(Age)))]
    
    population_schools[, school_type := fifelse(student_status & Age < 12, 1,
                                    fifelse(student_status & Age <= 17, 2, 
                                fifelse(student_status, 3, NA_integer_)))]

    # Return the data
    return(population_schools)
}

population_schools <-allocateSchools(population)   
population_schools %>%
    group_by(student_status,school_grade,school_type) %>%
    summarise(
        total = n(),
        age_mean = mean(Age,na.rm=TRUE),
        age_sd = sd(Age,na.rm=TRUE),
        age_min = min(Age,na.rm=TRUE),
        age_max = max(Age,na.rm=TRUE)
    ) %>%
  arrange(age_min)


clean_primary_secondary <- function(primary_secondary) {
    
    return(primary_secondary)
}

clean_schools_higher_education <- function(schools_higher_education) {

    return(schools_higher_education)
}

join_zoneID <- function(education_data, zoneSystem) {
    # read in zone system shapefile
    zoneSystem <- readOGR(zoneSystem)

    # reproject education_data to match zoneSystem crs
    education_data <- spTransform(education_data, crs(zoneSystem))

    # join zoneID to education_data based on spatial join
    education_data <- over(education_data, zoneSystem)

    # set x and y coordinates to transformed coordinates
    education_data$x <- coordinates(education_data)[,1]
    education_data$y <- coordinates(education_data)[,2]

    # Return the data
    return(education_data)
}