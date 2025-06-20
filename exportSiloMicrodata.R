# See https://github.com/jibeproject/syntheticPopulationMelbourne/issues/4

library(dplyr)
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

log_info("Preparing jj_2018 jobs microdata")

non_residential_buildings <- buildings[use != "Residential"]

# Allocate random non-residential building to each worker based on zone
jj_2018 <- workers[
        non_residential_buildings, on = .(sa1_work = zone), allow.cartesian = TRUE
    ][
        , .SD[sample(.N, 1)], by = PlanId 
    ][
        , .(
            id = .I, 
            zone = sa1_work,
            personId = PlanId,
            type = "tot",
            microLocationType = "poi",
            microBuildingID = id,
            coordX = coordX,
            coordY = coordY
        )
    ]

# If there are zone values in non_residential_buildings that do not match any sa1_work in workers, the resulting joined rows will have NA for all columns from workers, resulting in NA for personId.  Such rows should be omitted.
jj_2018 <- jj_2018[!is.na(personId)]

# building_summary <- jj_2018[
#     , .(
#         unique_building_count = uniqueN(microBuildingID),
#         unique_person_count = uniqueN(personId)
#         ), by = zone
# ]

# building_summary %>% summary()
##       zone           unique_building_count unique_person_count
##  Min.   :2.060e+10   Min.   :  1.00        Min.   :   1.0
##  1st Qu.:2.070e+10   1st Qu.:  7.00        1st Qu.: 378.2
##  Median :2.100e+10   Median : 14.00        Median : 590.0
##  Mean   :2.097e+10   Mean   : 28.26        Mean   : 720.6
##  3rd Qu.:2.120e+10   3rd Qu.: 29.00        3rd Qu.: 811.8
##  Max.   :2.140e+10   Max.   :918.00        Max.   :3708.0

# Write the output to a CSV file
write.csv(jj_2018, paste0('../microData/jj_', base_year, '.csv'), row.names = FALSE)

setDT(jj_2018)
setkey(jj_2018, personId)

log_info("Preparing pp_2018 population microdata")


# Step 1: Add occupation column to population
## Follows numeric typology defined at:
## https://github.com/jibeproject/mito/blob/01ef0b7790273d185d8b0c49da392a37043afc82/mitoCore/src/main/java/de/tum/bgu/msm/data/MitoOccupationStatus.java#L9-L17
## and discussed in https://github.com/jibeproject/mito/issues/18
## 1 = employed (not a student)
## 2 = unemployed (not employed nor a student, and aged < 67)
## 3 = student
## 4 = other (not employed nor a student, and aged >= 67)
population <- population[
    , occupation := fifelse(student_status == 1, 3, 
        fifelse(is_employed == TRUE & student_status != 1, 1,
            fifelse(is_employed != TRUE & student_status != 1 & Age < 67, 2, 4)
        )
    )
]

# Step 2: Join with jj_2018 and derive work-related columns
pp <- merge(
    population,
    jj_2018[, .(personId, work_zone = zone, work_building_id = id)],
    by.x = "AgentId",
    by.y = "personId",
    all.x = TRUE
)

# Step 3: Join with schools and derive school-related columns
pp <- merge(
    pp,
    schools[, .(school_id = id, school_zone = zone)],
    by.x = "assigned_school",
    by.y = "school_id",
    all.x = TRUE
)

# Step 4: Select and compute the final columns
pp <- pp[
    , .(
        id = AgentId,
        hhid = HouseholdId,
        age = Age,
        gender = Gender,
        ethnic = NA_character_,
        relationship = tolower(RelationshipStatus),
        occupation = occupation,
        driversLicence = NA_integer_,
        # education = education,
        workplace = fifelse(!is.na(work_building_id), work_building_id, -1),
        income = 1901*52, # set to 2021 median household income calculated for 2018 https://abs.gov.au/census/find-census-data/quickstats/2021/2GMEL
        schoolId = assigned_school,
        home_SA1 = SA1_MAINCODE_2016,
        school_SA1 = school_zone,
        work_SA1 = work_zone
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
        id = HouseholdId, 
        hhSize = hhSize,
        zone = SA1_MAINCODE_2016,
        autos = hhCar
    ), by = HouseholdId][
    , .SD[1], by = id][
    , .(id, hhSize, zone, autos)]

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

# For each zone, if there are no residential buildings, use non-residential buildings as fallback
zones_with_res <- unique(residential_buildings$zone)
zones_without_res <- setdiff(unique(hh$zone), zones_with_res)

missing_zones <- setdiff(unique(hh$zone), unique(residential_buildings$zone))
n_missing_households <- hh[zone %in% missing_zones, .N]
print(paste("Number of households with no matching zone in residential_buildings:", n_missing_households))
# 2849

# Combine: residential buildings for zones that have them, otherwise non-residential
combined_buildings <- rbind(
    residential_buildings,
    non_residential_buildings[zone %in% zones_without_res]
)

missing_zones <- setdiff(unique(hh$zone), unique(combined_buildings$zone))
n_missing_households <- hh[zone %in% missing_zones, .N]
print(paste("Number of households with no matching zone in combined_buildings:", n_missing_households))
# 612

dd <- hh[
        combined_buildings[, .(zone, building_id = id, coordX, coordY)], on = .(zone), allow.cartesian = TRUE
    ][
        , .SD[sample(.N, 1)], by = id  
    ][
    !is.na(id) 
    ][
    , .(
        id = id,
        zone = zone,
        type = "flat",
        hhid = id,
        bedrooms = 3,
        quality = 3,
        monthlyCost = 1640,
        yearBuilt = 0,
        floor = 0,
        microBuildingID = building_id,
        coordX = coordX,
        coordY = coordY
    )
]

# > dd %>% nrow()
# [1] 1837077
# > nrow(hh) - nrow(dd)           
# [1] 612

missing_hh <- hh[!id %in% dd$id]
zoneSystem$zone <- as.double(zoneSystem$zone)
if (nrow(missing_hh) > 0) {
    # Get polygons for missing zones
    missing_zones_sf <- zoneSystem[zoneSystem$zone %in% missing_hh$zone, ]
    # Generate one random point per zone
    sampled_points <- st_sample(missing_zones_sf, size = nrow(missing_zones_sf), type = "random")
    # Map sampled points to zones
    coords <- st_coordinates(sampled_points)
    zone_coords <- data.table(zone = missing_zones_sf$zone, coordX = coords[,1], coordY = coords[,2])
    # Merge coordinates to missing_hh
    missing_hh <- merge(missing_hh, zone_coords, by = "zone", all.x = TRUE)
    # Add required columns
    missing_hh[, `:=`(
        type = "flat",
        hhid = id,
        bedrooms = 3,
        quality = 3,
        monthlyCost = 1640,
        yearBuilt = 0,
        floor = 0,
        microBuildingID = NA
    )]
    # Bind to dd
    dd <- rbind(dd, missing_hh[, .(id, zone, type, hhid, bedrooms, quality, monthlyCost, yearBuilt, floor, microBuildingID, coordX, coordY)], fill = TRUE)
}

# > dd %>% nrow()
# [1] 1837689
# > nrow(hh) - nrow(dd)           
# [1] 0


# dwelling_zone_summary <- dd[
#    , .(
#        unique_building_count = uniqueN(microBuildingID),
#        unique_household_count = uniqueN(hhid)
#        ), by = zone
# ]

# dwelling_zone_summary %>% summary()
##       zone           unique_building_count unique_household_count
##  Min.   :2.060e+10   Min.   :   1.00       Min.   :   1.0
##  1st Qu.:2.080e+10   1st Qu.:  74.00       1st Qu.: 124.0
##  Median :2.110e+10   Median :  97.00       Median : 165.0
##  Mean   :2.102e+10   Mean   :  99.62       Mean   : 179.9
##  3rd Qu.:2.121e+10   3rd Qu.: 122.00       3rd Qu.: 214.0        
##  Max.   :2.140e+10   Max.   :1167.00       Max.   :3278.0

# dd[
#    , .(
#        unique_household_count = uniqueN(hhid)
#        ), by = microBuildingID
# ] %>% summary()
##Processed 1017399 groups out of 1017399. 100% done. Time elapsed: 44s. ETA: 0s.
## microBuildingID    unique_household_count
## Length:1017399     Min.   :  1.000
## Class :character   1st Qu.:  1.000
## Mode  :character   Median :  1.000
##                    Mean   :  1.806
##                    3rd Qu.:  2.000
##                    Max.   :665.000

write.csv(dd, paste0('../microData/dd_', base_year, '.csv'), row.names = FALSE)


