suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

# Increase the maximum allowed size for globals
options(future.globals.maxSize = 1024 * 1024 * 1024) # 1 GB

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

# 2. add household number of cars 
source('determineHouseholdCar.R', local=TRUE); 
population <- determineHouseholdCar(population)

source('determineStudentSchools.R', local=TRUE);
population <- determineStudentSchools(population)

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

# assigns a work SA1 location for those that work.
# takes about 2 days to run for the entire population
#might want to run this one interactively
source('assignWorkLocations.R', local=TRUE); 
assignWorkLocations(
  outputDir,
  population
);