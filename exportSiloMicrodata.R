# See https://github.com/jibeproject/syntheticPopulationMelbourne/issues/4

library(logger)
library(data.table)

# Load project properties (includes coordinate reference system)
properties_file <- "../project.properties"
source(properties_file)


log_info("Loading population data")
population <- readRDS('../output/synthetic_population/population_final.rds')
workers <- readRDS('../output/synthetic_population/workers_sa1_balance1.rds')
schools <- read.csv('../microData/ss_2018.csv')

setDT(population)
setDT(workers)
setDT(schools)
# Set keys for faster joins
setkey(population, SA1_MAINCODE_2016)
setkey(workers, SA1_MAINCODE_2016)
setkey(schools, id)

log_info("Preparing pp_2018 population microdata")
pp <- population[
    , occupation := fifelse(student_status == 1, "student", 
                                 fifelse(is_employed == 1, "employed", "other"))
][
    workers, on = .(AgentId = PlanId), work_SA1 := sa1_work
][
    schools, on = .(assigned_school = id), school_SA1 := zone
][
    , .(
        id = AgentId,
        hhid = HouseholdId,
        age = Age,
        gender = Gender,
        # ethnic = NA_character_,
        relationship = tolower(RelationshipStatus),
        occupation = occupation,
        # driversLicence = NA_integer_,
        education = education,
        home_SA1 = SA1_MAINCODE_2016,
        work_SA1 = work_SA1,
        school_SA1 = school_SA1,
        schoolId = assigned_school
    )
]
log_info(paste0("Writing ../microData/pp_",base_year,".csv"))
write.csv(pp, paste0('../microData/pp_', base_year, '.csv'), row.names = FALSE)

log_info("Preparing hh_2018 household microdata")

## Below checks were run and confirmed that there is no variation in household attributes within households.
## To speed up code, this has been commented out.
# log_info("  - confirming no variation of household attributes within households")
# variation_check <- population[
#     , .(
#         hhSize_unique = uniqueN(hhSize),
#         hhCar_unique = uniqueN(hhCar),
#         SA1_MAINCODE_2016_unique = uniqueN(SA1_MAINCODE_2016)
#     ), by = HouseholdId
# ]

# households_with_variation <- variation_check[
#     hhSize_unique > 1 | hhCar_unique > 1 | SA1_MAINCODE_2016_unique > 1
# ]

# if (nrow(households_with_variation) > 0) {
#     print("Households with variation found:")
#     print(households_with_variation)
# } else {
#     print("No variation found within households for hhSize, hhCar, or SA1_MAINCODE_2016.")
# }

hh <- population[
    , .(
        hhid = HouseholdId,
        hhSize = hhSize,
        zone = SA1_MAINCODE_2016,
        autos = hhCar
    ), by = HouseholdId][
    , .SD[1], by = hhid][
    , .(hhid, hhSize, zone, autos)]

log_info(paste0("Writing ../microData/hh_",base_year,".csv"))
write.csv(hh, paste0('../microData/hh_', base_year, '.csv'), row.names = FALSE)
