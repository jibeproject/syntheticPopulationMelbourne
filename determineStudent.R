library(tidyverse)
library(data.table)

# For testing
# test<- population[sample(nrow(population),20),]

# load general purpose utility functions
source("util.R")

# Set seed for reproducibility
set.seed(12)

# 2019 Primary and secondary data have grade and gender-specific enrolment totals
enrolments_primary_secondary <- read.csv("data/schools/primary and secondary schools/primary_secondary_school_enrolments_locations.csv")

# 2018 Tertiary and technical higher education
enrolments_higher_education <- read.csv("data/schools/higher education/Melbourne_cleaned_higher_education_enrolments_2018_with_location.csv")

# ABS 2016 Census fulltime/parttime student status by SA2, Age group and gender
census_data_path = "abs/Melb 2016 - Student status by SA2 (UR) AGE5P SEXP/Melb 2016 - SA2 (UR), AGE5P - Age in Five Year Groups and SEXP cleaned.csv"
census_data <- prepare_census_data(census_data_path)

# set data.table
setDT(census_data)

get_grade_given_age <- function(age) {
    # The law in Victoria states that children must attend school from the age of 6. 
    # We will assume that children aged 5 are in Prep, children aged 6 are in Grade 1, and so on.
    # Our primary/secondary enrolment data have overall and gender stratified enrolment totals for each grade,
    # as follows: Year.1.Males.Total	Year.1.Females.Total	Year.1.Total
    # The following lookup table will be used to determine the grade given the age of the student.
    if (age < 5 || age > 17) {
        return(NA)
    }
    age_grade_lookup <- c(
        "5" = "Prep",
        "6" = "Grade.1",
        "7" = "Grade.2",
        "8" = "Grade.3",
        "9" = "Grade.4",
        "10" = "Grade.5",
        "11" = "Grade.6",
        "12" = "Year.7",
        "13" = "Year.8",
        "14" = "Year.9",
        "15" = "Year.10",
        "16" = "Year.11",
        "17" = "Year.12"
    )
    return(age_grade_lookup[as.character(age)])
}

student_status_given_area_age_gender <- function(area, age, gender) {
    # Using census data, the given SA2 area code, age_cat and gender 
    age_cat = get_age_cat_from_age(age)
    # the probability of being a student is identified and applied
    student_probability <- census_data[
        SA2_MAINCODE == area & 
        age_cat == age_cat & 
        gender == gender, 
        student_probability
    ]
    student_status <- ifelse(runif(length(student_probability)) < student_probability, TRUE, FALSE)
    return(student_status)
}

allocateSchools <- function(population) {
    population_schools <- copy(population)
    setDT(population_schools)
    
    # for persons aged 5 to 14, assign student status, grade and school type based on age
    # for persons aged 15 and higher, assign a school type based on probability of being a student
    # given age and gender according to the census data
    # the school types are 1 (primary), 2 (secondary), and 3 (higher education)
    
    population_schools[, age_cat := get_age_cat_from_age(Age)]
    
    population_schools[, 
        student_status := 
            fifelse(Age < 5, FALSE,
            fifelse(Age < 15, TRUE,
            student_status_given_area_age_gender(SA2_MAINCODE, Age, gender)))
        ]
    
    population_schools[, school_grade := get_grade_given_age(Age)]
    
    population_schools[, school_type := fifelse(Age < 5, NA_integer_,
                                        fifelse(Age < 12, 1,
                                                fifelse(student_status & Age <= 17, 2, 3)))]
    
    # Return the data
    return(population_schools)
}

prepare_census_data <- function(census_data_path) {
    # ABS 2016 Census fulltime/parttime student status by SA2, Age group and gender
    # The three categories are: "SA2..UR.", "AGE5P", "SEXP"
    # The three relevant count columns are "Not.attending", "Full.time.student", "Part.time.student"
    # The full time and part time counts are summed to get the total student count
    # The sum of the three count columns is the total population denominator (not stated are excluded)
    # The probability of being a student in an SA2 given age and gender is calculated as the student count divided by the total population
    # Note that this is only used for persions aged 15 and older, persons aged 5 to 14 are assumed to be students
    census_data <- read.csv(census_data_path)
    census_data <- census_data %>%
        mutate(
            total_student_count = Full.time.student + Part.time.student,
            total_population = Not.attending + Full.time.student + Part.time.student,
            student_probability = total_student_count / total_population,
            age_cat = get_age_cat_int_from_str(AGE5P)
        ) %>%
        select(SA2..UR., age_cat, SEXP, student_probability) %>%
        rename(
            SA2_MAINCODE=SA2..UR.,
            gender=SEXP
        )

    return(census_data)
}

# ABS 2016 Census fulltime/parttime student status by SA2, Age group and gender
census_data <- prepare_census_data()

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