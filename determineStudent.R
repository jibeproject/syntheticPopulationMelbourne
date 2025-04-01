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
## NOTE: Currently the provided shapefile only encompasses Greater Melbourne.
## This means, schools cannot be linked to SA1 areas surrounding Greater Melbourne, 
## but only those within the Greater Melbourne area.
## This is a limitation of the current model, that should be noted. 
## This may be addressed at a later stage, should we have capacity.
zonesShapefile = "../input/zonesShapefile/SA1_2016_AUST_MEL.shp"
zoneSystem <- st_read(zonesShapefile) %>% st_transform(crs = crs)

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
    census_data[, age_cat := sapply(AGE5P, get_age_cat_int_from_str)]
    
    # Select and rename columns
    census_data <- census_data[, .(SA2_MAINCODE = SA2..UR., AGE5P, age_cat, Gender = SEXP, total_student_count, total_population, student_probability)]
    
    return(census_data)
}

# ABS 2016 Census fulltime/parttime student status by SA2, Age group and gender
census_data <- prepare_census_data(census_data_path)


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

allocateStudentStatus <- function(population) {
    population_students <- copy(population)
    setDT(population_students)
    
    # for persons aged 5 to 14, assign student status, grade and school type based on age
    # for persons aged 15 and higher, assign a school type based on probability of being a student
    # given age and gender according to the census data
    # the school types are 1 (primary), 2 (secondary), and 3 (higher education)
    
    population_students[, age_cat := get_age_cat_from_age(Age)]
    
    # Perform a join to get student_probability
    population_students <- merge(
        population_students, 
        census_data, 
        by.x = c("SA2_MAINCODE", "age_cat", "Gender"), 
        by.y = c("SA2_MAINCODE", "age_cat", "Gender"), 
        all.x = TRUE
    )
    population_students[, student_status := fifelse(Age < 5, FALSE,
                                        fifelse(Age >= 5 & Age <= 17, TRUE,
                                                fifelse(is.na(student_probability), FALSE, runif(.N) < student_probability)))]
    
    population_students[, school_grade := fifelse(Age < 5, NA_character_,
                                        fifelse(Age > 17, NA_character_,
                                    get_grade_given_age(Age)))]
    
    population_students[, school_type := fifelse(student_status & Age < 12, 1,
                                    fifelse(student_status & Age <= 17, 2, 
                                fifelse(student_status, 3, NA_integer_)))]

    # Return the data
    return(population_students)
}

population_students <-allocateStudentStatus(population)   
population_students %>%
    group_by(student_status,school_grade,school_type) %>%
    summarise(
        total = n(),
        age_mean = mean(Age,na.rm=TRUE),
        age_sd = sd(Age,na.rm=TRUE),
        age_min = min(Age,na.rm=TRUE),
        age_max = max(Age,na.rm=TRUE)
    ) %>%
  arrange(age_min)


# 2019 Primary and secondary data have grade and gender-specific enrolment totals 
# with matching zone data
enrolments_primary_secondary <- (read.csv(
    "data/schools/primary and secondary schools/primary_secondary_school_enrolments_locations.csv"
    ) %>% 
    st_as_sf(
    coords = c("X", "Y"),
    crs = 4326
    )) %>% st_transform(crs = crs) %>% 
    st_join(zoneSystem, join = st_intersects) %>%
    filter(!is.na(SA1_MAIN16)) %>%
    mutate(
        jibeSchoolId = row_number()
    ) %>% 
    select(jibeSchoolId, everything())

# 2018 Tertiary and technical higher education 
# with matching zone data
enrolments_higher_education <- (read.csv(
        "data/schools/higher education/Melbourne_cleaned_higher_education_enrolments_2018_with_location.csv"
    ) %>% 
    st_as_sf(
        coords = c("longitude_4326", "latitude_4326"),
        crs = 4326
    )) %>% st_transform(crs = crs) %>% 
    st_join(zoneSystem, join = st_intersects) %>%
    filter(!is.na(SA1_MAIN16)) %>%
    mutate(
        jibeSchoolId = row_number() + max(enrolments_primary_secondary$jibeSchoolId, na.rm = TRUE)
    ) %>% 
    select(jibeSchoolId, everything())

allocateSchool <- function(population_students, enrolments_primary_secondary, enrolments_higher_education) {
    # For each student, assign a school accounting for school enrolements
    # based on the school type, gender, grade and distance (closest)
    # The school types are 1 (primary), 2 (secondary), and 3 (higher education)
    # Gender and school grades are used for primary and secondary only 

    primary_secondary <- setDT(enrolments_primary_secondary %>% copy())
    higher_education <- setDT(enrolments_higher_education %>% copy())
    # First for each school, identify students of matching type and grade (where applicable) located within 1600m Euclidean distance (about a 20 minute walk, beyond which distance other modes of transport may be begin to be more likely to be used).  We will use these to determine which students are within the catchment of a single school in the first instance.  Later, continuous distance will be used to prioritise remaining students.
    # Because our synthetic population is only located by SA1, and not for specific 
    # micro-locations, there is little benefit from route based analysis since origin 
    # locations have been meaningfully determined beyond the broader SA1 catchment.
    # Hence, buffer analysis is used in first instance, then Euclidean distance to closest 
    # from SA1 centroid.
    primary_secondary[,'buffer':= st_buffer(geometry, dist = 1600)]
    higher_education[,'buffer':= st_buffer(geometry, dist = 1600)]

    # Population locations are currently determined to SA1 level, so rather than evaluate for each student, we will evaluate for each SA1 against the intersection with the buffer column.  This operation is done at the SA1 multipolygon level, since we don't know where within SA1s specifically individuals live. So we identify the SA1 catchment for schools then link that to individuals, then derive the type that meets their attributes.  There will be two columns created: primary_secondary and higher_education, which will contain the list of SA1s that intersect with the buffer column.
    zoneSystem$primary_secondary_catchment <- sapply(
        st_intersects(zoneSystem, primary_secondary$buffer),
        function(indices) {
            if (length(indices) > 0) {
                primary_secondary$jibeSchoolId[indices]
            } else {
                NA  # No match
            }
        }
    )
    zoneSystem$higher_education_catchment <- sapply(
        st_intersects(zoneSystem, higher_education$buffer),
        function(indices) {
            if (length(indices) > 0) {
                higher_education$jibeSchoolId[indices]
            } else {
                NA  # No match
            }
        }
    )
    # Now we can join the zoneSystem with the population data to get the school catchments for each student.  This only retains the students.
    population_schools <- merge(
        population_students[student_status == TRUE], 
        zoneSystem %>% 
            mutate(
                SA1_MAINCODE_2016 = as.double(SA1_MAIN16)
            ) %>% 
            select(
                SA1_MAINCODE_2016, 
                primary_secondary_catchment, 
                higher_education_catchment
            ),
        by = "SA1_MAINCODE_2016",
        all.x = TRUE
    )

    # For primary and secondary school students, precompute the column name for their enrolment evaluation.  
    population_schools[, primary_secondary_enrolment := 
        fifelse(
            school_type %in% c(1, 2), 
            paste0(school_grade, ".", Gender, "s.Total"), 
            NA_character_
        )
    ]

    # Update the population_schools primary_secondary_catchment list of IDs for those students that have a 
    # non-NA primary_secondary_enrolment, based on the school IDs in the primary_
    # secondary_catchment list having a column name matching the 
    # primary_secondary_enrolment value and with value in that column greater than zero
    # This should return a list of jibeSchoolId values.

    population_schools[school_type %in% c(1, 2), primary_secondary_catchment := 
        lapply(
            seq_len(nrow(population_schools)), 
            function(i) {
                if (!is.na(primary_secondary_catchment[i]) && !is.na(primary_secondary_enrolment[i])) {
                    ids <- unlist(primary_secondary_catchment[i])
                    enrolment_col <- primary_secondary_enrolment[i]
                    if (length(ids) > 0 && !all(is.na(ids)) && !is.na(enrolment_col)) {
                        valid_ids <- primary_secondary[
                            jibeSchoolId %in% ids & 
                            get(enrolment_col) > 0, 
                            jibeSchoolId
                        ]
                        if (length(valid_ids) > 0) valid_ids else NA_complex_
                    } else {
                        list()
                    }
                } else {
                    list()
                }
            }
        )
    ]

    # For primary and secondary school students having no listed school in their catchment, identify the closest school having a non-zero enrolment in the column matching their primary_secondary_enrolment value.  This is done by evaluating the distance from the SA1 centroid to the school location, and returning the jibeSchoolId of the closest school having a non-zero enrolment in that column.  This is done for each student having a non-NA primary_secondary_catchment value.
    population_schools[school_type %in% c(1, 2) & is.na(primary_secondary_catchment), 
        primary_secondary_catchment := 
            lapply(
                seq_len(nrow(population_schools)), 
                function(i) {
                    if (!is.na(primary_secondary_enrolment[i])) {
                        enrolment_col <- primary_secondary_enrolment[i]
                        ids <- primary_secondary[
                            get(enrolment_col) > 0, 
                            jibeSchoolId
                        ]
                        if (length(ids) > 0) {
                            distances <- st_distance(
                                population_schools[i, geometry], 
                                primary_secondary[jibeSchoolId %in% ids, geometry]
                            )
                            closest_school_id <- ids[which.min(distances)]
                            closest_school_id
                        } else {
                            NA_complex_
                        }
                    } else {
                        NA_complex_
                    }
                }
            )
    ]



    # This aligns individuals with their appropriate primary_secondary enrolment data field, 
    # and can be checked with the following: 
    # population_schools[,c('primary_secondary_enrolment','school_type')] %>% table

    # Evaluate students' catchments with respect to school type---and if primary/secondary, gender and grade---within 1600m.  Gender and grade are only relevant for primary and secondary students.  Primary and secondary school data has columns matching the school grade and gender matching the pattern "{school_grade}.{Gender}.Total", with that total being an integer value greater than zero.
    
    population_schools[, school_catchment := 
        fifelse(
            !is.na(primary_secondary_catchment) & school_type %in% c(1, 2),
            {
                # Filter valid rows for primary/secondary schools
                valid_rows <- !is.na(primary_secondary_catchment) & school_type %in% c(1, 2)
                
                # Extract valid IDs and enrolment columns
                ids_list <- primary_secondary_catchment[valid_rows]
                enrolment_cols <- primary_secondary_enrolment[valid_rows]
                
                # Vectorized evaluation of valid IDs
                lapply(seq_along(ids_list), function(i) {
                    ids <- ids_list[[i]]
                    enrolment_col <- enrolment_cols[i]
                    if (!is.null(ids) && length(ids) > 0 && !all(is.na(ids)) && !is.na(enrolment_col)) {
                        valid_ids <- primary_secondary[
                            jibeSchoolId %in% ids & 
                            get(enrolment_col) > 0, 
                            jibeSchoolId
                        ]
                        if (length(valid_ids) > 0) valid_ids else list()
                    } else {
                        list()
                    }
                })
            },
            higher_education_catchment
        )
    ]
}