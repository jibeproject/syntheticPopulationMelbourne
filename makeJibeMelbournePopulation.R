suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

# Increase the maximum allowed size for globals
options(future.globals.maxSize = 1024 * 1024 * 1024) # 1 GB

if (basename(normalizePath("../")) != "melbourne") {
    stop("The base directory is not named 'melbourne'. Current base directory: ", basename(normalizePath("../"), "\nThe synthetic population code is intended to be run from the JIBE Melbourne directory, containing the file 'project.properties'."))
}

if (!file.exists("../project.properties")) {
    stop("The file 'project.properties' does not exist in the expected location: '../project.properties'.")
}

if (!dir.exists("../input/buildingShapefile")) {
    stop("The directory '../input/buildingShapefile' does not exist.")
}

if (!file.exists("../input/buildingShapefile/buildings.geojson")) {
    stop("The file 'buildings.geojson' does not exist in the directory '../input/buildingShapefile'.")
}
# load general purpose utility functions
source("util.R")

outputDir="../output/synthetic_population"

dir.create(outputDir, showWarnings = FALSE, recursive=TRUE)

# collates the population into a single table, adding in household ids
source('collatePopulation.R', local=TRUE); 
population <- collate2016Population()

# using census data determines if agents should be employed or not
source('determineEmployed.R', local=TRUE); 
population <- determineEmployed(population)

# add in additional variables from census data
# 1. add education level (education level is define as high, medium, low, based on paper doi: 10.1093/ije/dyab080 and ASCED - ISCED2011 Level Correspondence Table)
source('determineEducationLevel.R', local=TRUE); 
population <- determineEducationLevel(population)

# 2. add household attributes
source('determineHouseholdAttributes.R', local=TRUE); 
population <- determineHouseholdAttributes(population)

# 3. assign student status and allocate schools
source('determineStudentSchools.R', local=TRUE);
population <- determineStudentSchools(population)

# For historical reasons this naming convention is retained, but this is not the final population data
final_processed_population_data_file <-  paste0(outputDir,'/population_final.rds')
saveRDS(population,final_processed_population_data_file)
echo(
  paste0("Wrote ",
    nrow(population),
    " sampled persons to ",
    final_processed_population_data_file,
    "\n"
  )
)

# Assign work SA1 locations
source('assignWorkLocations.R', local=TRUE); 
assignWorkLocations(
  outputDir,
  population
);

# Prepare and output final population microdata for SILO
source('exportSiloMicrodata.R', local=TRUE);