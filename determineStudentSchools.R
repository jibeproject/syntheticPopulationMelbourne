library(tidyverse)
library(data.table)
library(sf)
library(furrr)
library(logger)
library(ggplot2)

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


determineStudentSchools <- function(population) {
    initial_columns <- colnames(population)
    new_columns <- c("student_status", "school_type", "school_grade", "assigned_school")
    log_info("Setting up census data")
    census_data <<- prepare_census_data(census_data_path)
    log_info("Allocating student status")
    population_students <-allocateStudentStatus(population)   
    # population_students %>%
    #     group_by(student_status,school_grade,school_type) %>%
    #     summarise(
    #         total = n(),
    #         age_mean = mean(Age,na.rm=TRUE),
    #         age_sd = sd(Age,na.rm=TRUE),
    #         age_min = min(Age,na.rm=TRUE),
    #         age_max = max(Age,na.rm=TRUE)
    #     ) %>%
    # arrange(age_min)
    log_info("Setting up primary and secondary schools")
    enrolments_primary_secondary <<- prepare_primary_schools()

    log_info("Setting up higher education schools")
    enrolments_higher_education <<- prepare_higher_education_schools()

    log_info("Preparing combined school enrolments with export to geojson and csv microdata")
    combined_school_enrolments <- prepare_combined_school_enrolments()

    log_info("Allocating schools to students (this is can be a long running process)")
    population_schools <- allocateSchools(population_students[student_status==TRUE,])

    log_info("Joining the students with schools back to the population data")
    population_students <- merge(
        population_students, 
        population_schools[, .(AgentId, assigned_school)], 
        by = "AgentId", 
        all.x = TRUE
    )
    population_students <- population_students %>% as_tibble() %>% select(all_of(c(initial_columns,new_columns)))

    log_info("Saving summary plots and csv: ../output/synthetic_population/synthetic_population_and_census_student_percentage_by_age_gender.jpg")
    prepare_summary_plot(population_students, census_data)
    prepare_spatial_summary_plot(population_students, census_data, zoneSystem) 

    log_info("Returning the population data with schools assigned.")
    prepare_school_microdata(population_students)
    return (population_students)
}


prepare_primary_schools <- function() {
    # 2019 Primary and secondary data have grade and gender-specific enrolment totals and zones
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
    return (enrolments_primary_secondary)
}
prepare_higher_education_schools <- function() {
    # 2018 Tertiary and technical higher education enrolment totals and zones
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
            jibeSchoolId = row_number() + max(enrolments_primary_secondary$jibeSchoolId, na.rm = TRUE),
            Male = as.integer(gsub(",","",Male)),
            Female = as.integer(gsub(",","",Female.or.non.binary)),
            TOTAL = as.integer(gsub(",","",TOTAL))
        ) %>% 
        select(jibeSchoolId, everything())
    return (enrolments_higher_education)
}
prepare_combined_school_enrolments <- function() {
    # Combine enrolments_primary_secondary and enrolments_higher_education
    combined_enrolments <- bind_rows(
        enrolments_primary_secondary %>%
            mutate(
                SA1_MAINCODE_2016 = SA1_MAIN16,
                primary_enrolments = Primary.Total,
                secondary_enrolments = Secondary.Total,
                higher_education_enrolments = 0  # No higher education in primary/secondary data
            ),
        enrolments_higher_education %>%
            mutate(
                SA1_MAINCODE_2016 = SA1_MAIN16,
                primary_enrolments = 0,  # No primary enrolments in higher education data
                secondary_enrolments = 0,  # No secondary enrolments in higher education data
                higher_education_enrolments = TOTAL  # All rows are higher education
            )
    ) %>%
        select(jibeSchoolId, SA1_MAINCODE_2016, primary_enrolments, secondary_enrolments, higher_education_enrolments)
    st_write(combined_enrolments, "../output/synthetic_population/schools_by_type.geojson", delete_dsn = TRUE)
}

prepare_school_microdata <- function(population_students) {
    # Prepare CSV with fields: id, zone, type, enrolmentsMale, enrolmentsFemale, enrolmentsTotal, coordX, coordY 
    # Actual capacity is not known; using 2018 enrolments as a proxy for capacity
    # Occupancy will be completed using the allocations in the synthetic population
    primary_secondary <- enrolments_primary_secondary %>% 
            mutate(
                id = jibeSchoolId,
                zone = SA1_MAIN16,
                type = case_when(
                    coalesce(Primary.Total,0) > 0 & coalesce(Secondary.Total,0)==0 ~ "1",
                    coalesce(Secondary.Total,0) > 0 & coalesce(Primary.Total,0)==0  ~ "2",
                    coalesce(Primary.Total,0)  > 0 & coalesce(Secondary.Total,0)  > 0 ~ "1,2",
                    TRUE ~ NA_character_
                ),
                capacity = coalesce(Primary.Total, 0) + coalesce(Secondary.Total, 0),
                occupancy = NA,
                coordX = st_coordinates(geometry)[1],
                coordY = st_coordinates(geometry)[2]
            )
    higher_education <- enrolments_higher_education %>%
            mutate(
                id = jibeSchoolId,
                zone = SA1_MAIN16,
                type = "3",
                capacity = coalesce(TOTAL,0),
                occupancy = NA,
                coordX = st_coordinates(geometry)[1],
                coordY = st_coordinates(geometry)[2]
            )

    combined_schools <- bind_rows(
        primary_secondary %>% 
            st_drop_geometry() %>% 
            select(id, zone, type, capacity, occupancy, coordX, coordY),
        higher_education %>% 
            st_drop_geometry() %>% 
            select(id, zone, type, capacity, occupancy, coordX, coordY)
    )

    # Calculate occupancy for each school based on assigned students
    school_occupancy <- population_students %>%
        filter(!is.na(assigned_school)) %>%
        group_by(assigned_school) %>%
        summarise(occupancy = n()) %>%
        rename(id = assigned_school)

    # Update the occupancy in the combined_schools data
    combined_schools <- combined_schools %>%
        left_join(school_occupancy, by = "id") %>%
        mutate(occupancy = coalesce(occupancy, 0))


    if (!dir.exists("../microdata")) {
        dir.create("../microdata", recursive = TRUE)
    }
    write.csv(combined_schools, paste0('../microdata/ss_', base_year, '.csv'), row.names = FALSE)
}


prepare_summary_plot <- function(population_students, census_data) {

    population_student_summary<- population_students %>%
    group_by(age_cat, Gender) %>%
    summarise(
        total_students = sum(student_status, na.rm = TRUE),
        total_population = n(),
        student_proportion = (total_students / total_population)
    )

    census_student_summary<- census_data %>%
    group_by(age_cat, Gender) %>%
    summarise(
        total_students = sum(total_student_count, na.rm = TRUE),
        total_population = sum(total_population, na.rm = TRUE),
        student_proportion = (total_students / total_population)
    )
    # Combine the two summaries into one tibble for comparison
    combined_summary <- bind_rows(
    population_student_summary %>% mutate(Source = "Synthetic population"),
    census_student_summary %>% mutate(Source = "Census")
    ) %>% 
    mutate(age_cat = factor(
        age_cat, 
        levels = c(1:21),
        labels = get_age_cat_labels())
    )

    # Plot the data
    ggplot(combined_summary, aes(
        x = age_cat, 
        y = student_proportion, 
        color = Gender, 
        linetype = Source,
        group = interaction(Gender, Source))) +
    geom_line(linewidth = 1) +
    labs(
        title = "Comparison of Census and Synthetic Population Students (%), by Age and Gender",
        x = "Age group (years)",
        y = "Student Percentage",
        color = "Gender",
        linetype = "Source"
    ) +
    theme_minimal() +
    scale_y_continuous(labels = scales::percent_format()) +
    theme(
        legend.position = "bottom",
        text = element_text(size = 12)
    )
    ggsave(
        filename = "../output/synthetic_population/synthetic_population_and_census_student_percentage_by_age_gender.jpg",
        plot = last_plot(),
        device = "jpg",
        width = 10,
        height = 6,
        units = "in"
    )
}

prepare_spatial_summary_plot <- function(population_students, census_data, zoneSystem) {
    # Summarize student percentages by SA2 and age group for population_students
    population_summary <- population_students %>%
        mutate(age_group = case_when(
            age_cat %in% c(1, 2) ~ "<10",
            age_cat %in% c(3, 4) ~ "10-20",
            age_cat >= 5         ~ "20+"
        )) %>%
        group_by(SA2_MAINCODE, age_group) %>%
        summarise(
            total_students = sum(student_status, na.rm = TRUE),
            total_population = n(),
            student_percentage = (total_students / total_population) * 100
        )

    # Summarize student percentages by SA2 and age group for census_data
    census_summary <- census_data %>%
        mutate(age_group = case_when(
            age_cat %in% c(1, 2) ~ "<10",
            age_cat %in% c(3, 4) ~ "10-20",
            age_cat >= 5         ~ "20+"
        )) %>%
        group_by(SA2_MAINCODE, age_group) %>%
        summarise(
            total_students = sum(total_student_count, na.rm = TRUE),
            total_population = sum(total_population, na.rm = TRUE),
            student_percentage = (total_students / total_population) * 100
        )

    # Add a source column to distinguish between Census and Population data
    population_summary <- population_summary %>% mutate(Source = "Population")
    census_summary <- census_summary %>% mutate(Source = "Census")

    # Combine the summaries
    combined_summary <- bind_rows(population_summary, census_summary)

    # Join with zoneSystem geometries
    zoneSystem_dissolved <- zoneSystem %>%
        group_by(SA2_MAIN16, SA2_NAME16) %>%
        summarise(geometry = st_union(geometry)) %>%
        ungroup() %>%
        mutate(
            geometry = st_simplify(geometry, dTolerance = 100),
            SA2_MAIN16 = as.integer(SA2_MAIN16)
        )  

    choropleth_data <- zoneSystem_dissolved %>%
        left_join(
            combined_summary, 
            by = c("SA2_MAIN16" = "SA2_MAINCODE"),
            relationship = "many-to-many"
        )

    # Create the choropleth plot
    choropleth_plot <- ggplot(data = choropleth_data) +
        geom_sf(aes(fill = student_percentage), color = "transparent") +
        scale_fill_viridis_c(option = "plasma", na.value = "grey90", name = "Student %") +
        labs(
            title = "Student Percentage by SA2",
            subtitle = "Comparison of Census and Population Data by Age Group",
            caption = "Source: Synthetic Population and Census Data"
        ) +
        theme_minimal() +
        theme(
            legend.position = "right",
            text = element_text(size = 12),
            axis.title.x = element_blank(),  # Remove x-axis label
            axis.title.y = element_blank()   # Remove y-axis label
        ) +
        facet_grid(age_group ~ Source)

    # Save the plot
    ggsave(
        filename = "../output/synthetic_population/spatial_student_percentage_by_sa2.jpg",
        plot = choropleth_plot,
        device = "jpg",
        width = 12,
        height = 8,
        units = "in"
    )
  # Save the chorpleth data as geojson

    # Transform combined_summary to wide format
    combined_summary_wide <- combined_summary %>%
        pivot_wider(
            id_cols = SA2_MAINCODE,  # Group by SA2_MAINCODE
            names_from = c(Source, age_group),  # Use Source and age_group as prefixes
            values_from = c(total_students, total_population, student_percentage),  # Columns to spread
            names_sep = "_"  # Separator for new column names
        )

    # Join with zoneSystem geometries
    geojson <- zoneSystem_dissolved %>%
        left_join(
            combined_summary_wide, 
            by = c("SA2_MAIN16" = "SA2_MAINCODE"),
            all.x = TRUE,  # Keep all geometries
        )

    st_write(geojson, "../output/synthetic_population/student_percentage_by_population_census_sa2.geojson", delete_dsn = TRUE)
}

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

allocateSchools <- function(population_students) {
    # For each student, assign a school accounting for school enrolements
    # based on the school type, gender, grade and distance (closest)
    # The school types are 1 (primary), 2 (secondary), and 3 (higher education)
    # Gender and school grades are used for primary and secondary only 

    primary_secondary <- setDT(enrolments_primary_secondary %>% copy())
    higher_education <- setDT(enrolments_higher_education %>% copy())
    zone_system <- setDT(zoneSystem %>% copy())
    setkey(zone_system, SA1_MAIN16)
    population_schools <- setDT(
        population_students[
            student_status == TRUE, 
            c("AgentId","SA1_MAINCODE_2016","Age", "Gender", "school_type", "school_grade", "school_type", "school_grade")        
        ] %>% copy()
    )
    population_schools[, sa1_zone_index := match(SA1_MAINCODE_2016, zone_system$SA1_MAIN16)] 
    # First for each school, identify students of matching type and grade (where applicable) located within 1600m Euclidean distance (about a 20 minute walk, beyond which distance other modes of transport may be begin to be more likely to be used).  We will use these to determine which students are within the catchment of a single school in the first instance.  Later, continuous distance will be used to prioritise remaining students.
    # Because our synthetic population is only located by SA1, and not for specific 
    # micro-locations, there is little benefit from route based analysis since origin 
    # locations have been meaningfully determined beyond the broader SA1 catchment.
    # Hence, buffer analysis is used in first instance, rather than Euclidean distance to closest 
    # from SA1 centroid.
    # Population locations are currently determined to SA1 level, so rather than evaluate for each student, we will evaluate for each school which SA1s intersect with the buffer columns.  Since we don't know where within SA1s specifically individuals live, we identify the SA1 catchments for schools then link that to individuals filtering on the type that meets their attributes. This will be done separately for primary/secondary and higher education schools.
    compute_buffers <- function(zone_system, school_data, distance) {
        school_data[, 'sa1_catchment' :=
            st_intersects(
                st_buffer(geometry, dist = distance),
                zone_system$geometry,
                sparse = TRUE
            )
        ]
    }

    primary_secondary <- compute_buffers(zoneSystem, primary_secondary, 1600)
    higher_education <- compute_buffers(zoneSystem, higher_education, 1600)

    primary_secondary[, allocated_enrolments_Male := 0]
    primary_secondary[, allocated_enrolments_Female := 0]
    higher_education[, allocated_enrolments_Male := 0]
    higher_education[, allocated_enrolments_Female := 0]

    setkey(primary_secondary, jibeSchoolId)
    setkey(higher_education, jibeSchoolId)

    locate_school <- function(sa1_zone_index, Gender, school_grade, school_type) {
        # Given an enrolement column, assign a school to the student based on match of SA1_MAINCODE_2016 with either a school in same SA1 zone, or if not possible, from an SA1 matching the 'sa1_catchments' list ids for the school, where 1) the enrolment column is greater than zero and 2) the 'allocated_enrolments' is not greater than the enrolment column.  The first school that meets these criteria is assigned to the student.  If no schools meet these criteria, the student is assigned NA.  School allocation given attributes is incremented.
        # Adding a small delay to avoid overwhelming the system with requests
        # This is a workaround for the issue of too many requests being sent to the server in a short period of time.
        # see https://stackoverflow.com/a/69817842/4636357 
        Sys.sleep(0.000001); 
        # The function returns the jibeSchoolId of the assigned school.
        if (school_type == 3) {
            school_data <- 'higher_education'
            enrolment_column <- Gender
            allocated_enrolment_column <- paste0('allocated_enrolments_',Gender)
            potential_schools <- higher_education[
                get(enrolment_column) > 0 & 
                get(allocated_enrolment_column) < get(enrolment_column)
            ]
        } else if (school_type %in% c(1,2)) {
            school_data <- 'primary_secondary'
            enrolment_column <- paste0(school_grade, ".", Gender, "s.Total")
            allocated_enrolment_column <- paste0('allocated_enrolments_',Gender)
            potential_schools <- primary_secondary[
                get(enrolment_column) > 0 & 
                get(allocated_enrolment_column) < get(enrolment_column)
            ]
        } else {
            stop("Invalid school type")
        }
        sa1_schools <- potential_schools[
            SA1_MAIN16==zone_system[sa1_zone_index, SA1_MAIN16],
        ]
        if (nrow(sa1_schools) > 0) {
            # If there are schools in the SA1 catchment, allocate and return the first one that meets the criteria
            school <- allocate_located_school(sa1_schools[1, jibeSchoolId], school_data, allocated_enrolment_column)
            return(school)
        } 
        catchment_schools <- potential_schools[
            map_lgl(sa1_catchment, ~ sa1_zone_index %in% .x),
        ]
        if (nrow(catchment_schools) > 0) {
            # If there are schools in the SA1 catchment, allocate and return the first one that meets the criteria
            school <- allocate_located_school(catchment_schools[1, jibeSchoolId], school_data, allocated_enrolment_column)
            return(school)
        } 
        # If this student's SA1 is not within 1600m of any school, find the closest matching school with available enrolments to this students SA1
        closest_school <- potential_schools[
            which.min(
                st_distance(
                    potential_schools$geometry,
                    zone_system[sa1_zone_index]$geometry
                )), 
            jibeSchoolId
        ]
        if (!is.na(closest_school) && !(length(closest_school) == 0)) {
            # If there are schools in the SA1 catchment, allocate and return the first one that meets the criteria
            school <- allocate_located_school(closest_school, school_data, allocated_enrolment_column)
            return(school)
        }
        # If no schools meet the criteria, return NA
        return(NA)
    }

    
    allocate_located_school <- function(id, school_dataset, allocated_enrolment_column) {
        # Given a school record, and a list of schools, update the allocated_enrolment_column in the schools data frame and return the jibeSchoolId of the assigned school.

        if (school_dataset=='primary_secondary') {
            enrolments <- primary_secondary[jibeSchoolId==id, get(allocated_enrolment_column)] 
            primary_secondary[jibeSchoolId==id, (allocated_enrolment_column):= enrolments + 1]
        } else if (school_dataset=='higher_education') {
            enrolments <- higher_education[jibeSchoolId==id, get(allocated_enrolment_column)] 
            higher_education[jibeSchoolId==id, (allocated_enrolment_column):= enrolments + 1]
        } else {
            stop("Invalid school dataset")
        }
        return(id)
    }

    # Iterative assignment using a while loop
    iteration <- 1
    population_schools[, assigned_school := NA_integer_]
    while (any(is.na(population_schools$assigned_school))) {
        log_info("Iteration {iteration}: Assigning schools to students...")
        
        # Assign schools to students
        population_schools[is.na(assigned_school), assigned_school := future_pmap_int(
            .(sa1_zone_index, Gender, school_grade, school_type), 
            locate_school
        )]

        # Calculate the percentage of students assigned
        assigned_percentage <- 100 * sum(!is.na(population_schools$assigned_school)) / nrow(population_schools)
        log_info("Iteration {iteration}: {assigned_percentage}% of students have been assigned schools.")

        # Break the loop if all students are assigned
        if (assigned_percentage == 100) {
            log_info("All students have been successfully assigned schools.")
            break
        }

        iteration <- iteration + 1
    }
    
    st_write(
        primary_secondary %>% select(-sa1_catchment), 
        "../output/synthetic_population/primary_secondary_schools_allocated.geojson", 
        delete_dsn = TRUE
    )
    st_write(
        higher_education %>% select(-sa1_catchment), 
        "../output/synthetic_population/higher_education_schools_allocated.geojson", 
        delete_dsn = TRUE
    )
    return(population_schools)
}

