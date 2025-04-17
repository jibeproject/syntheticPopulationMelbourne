# See https://github.com/jibeproject/syntheticPopulationMelbourne/issues/4

library(logger)
library(data.table)
library(sf)

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

log_info("loading spatial data")
zoneSystem <- st_read("../input/zonesShapefile/SA1_2016_AUST_MEL.shp") %>% 
    st_transform(crs = crs) %>% 
    rename(zone = SA1_MAIN16)
buildings <- st_read("../input/buildingShapefile/buildings.geojson") %>% 
    st_transform(crs = crs)  %>% 
    st_centroid()
buildings <- st_join(buildings, zoneSystem[buildings, c("zone")], join = st_within) %>%
    mutate(
        coordX = st_coordinates(.)[, 1],
        coordY = st_coordinates(.)[, 2]
    ) %>% 
    filter(!is.na(zone)) %>%
    st_drop_geometry()


setDT(buildings)
setkey(buildings, zone, use)
buildings[, zone := as.double(zone)]

rm(zoneSystem)

log("Preparing jj_2018 jobs microdata")

non_residential_buildings <- buildings[use != "Residential"]

# Allocate random non-residential building to each worker based on zone
jj_2018 <- workers[
        non_residential_buildings, on = .(sa1_work = zone), allow.cartesian = TRUE
    ][
        , .SD[sample(.N, 1)], by = PlanId 
    ][
        , .(
            id = id, 
            zone = sa1_work,
            personId = PlanId,
            microLocationType = "poi",
            microBuildingID = id,
            coordX = coordX,
            coordY = coordY
        )
    ]

# Write the output to a CSV file
write.csv(jj_2018, paste0('../microData/jj_', base_year, '.csv'), row.names = FALSE)

log_info("Preparing pp_2018 population microdata")
pp <- population[
    , occupation := fifelse(student_status == 1, "student", 
                                 fifelse(is_employed == 1, "employed", "other"))
][
    jj_2018, on = .(AgentId = personId), 
    work_SA1 := zone, 
    workplace := fifelse(!is.na(microBuildingID), microBuildingID, -1)
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

log_info("Preparing dd_2018 dwelling microdata, noting:
    - id is set to household ID 
    - zone is set to SA1_MAIN16
    - type is set to 'flat' 
    - hhid is set to household ID
    - bedrooms is set to 3 
    - quality is set to 3 
    - monthlyCost is set to 1640 (median monthly rent in June 2018 for Metropolitan Melbourne, according to https://www.dffh.vic.gov.au/tables-rental-report-june-2018)
    - yearBuilt is set to 0
    - floor is set to 0
    - coordinates are set to a randomly selected building within the SA1 of the household
")

residential_buildings <- buildings[use == "Residential"]

dd <- hh[
    residential_buildings, on = .(zone), allow.cartesian = TRUE
][
    , .SD[sample(.N, min(.N, .GRP))], by = .(zone, hhid)
][
    , .(
        id = hhid,
        zone = zone,
        type = "flat",
        hhid = hhid,
        bedrooms = 3,
        quality = 3,
        monthlyCost = 1640,
        yearBuilt = 0,
        floor = 0,
        coordX = coordX,
        coordY = coordY
    )
]

write.csv(dd, paste0('../microData/dd_', base_year, '.csv'), row.names = FALSE)


