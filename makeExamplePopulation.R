suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

# load general purpose utility functions
source("util.R")

outputDir="output/test"

dir.create(paste0('../',outputDir), showWarnings = FALSE, recursive=TRUE)



# collates the population into a single table, adding in household ids
source('collatePopulation.R', local=TRUE); 
collate2016Population(
  outputDir
)

# using census data determines if agents should be employed or not
source('determineEmployed.R', local=TRUE); 
determineEmployed(
  outputDir
)

# gets the census and other datasets ready for assigning work locations.
source('prepWorkData.R', local=TRUE); 
prepWorkData(
  outputDir
)

# assigns a work SA1 location for those that work.
# takes about 2 days to run for the entire population
#might want to run this one interactively
source('assignWorkLocations.R', local=TRUE); 
assignWorkLocations(
  outputDir
)

# add in additional variables from census data
# 1. add education level (education level is define as high, medium, low, based on paper doi: 10.1093/ije/dyab080 and ASCED - ISCED2011 Level Correspondence Table)
source('determineEducationLevel.R', local=TRUE); 
determineEducationLevel(
  outputDir
)

# 2. add household number of cars 
source('determineHouseholdCar.R', local=TRUE); 
determineHouseholdCar(
  outputDir
)




# reformat to match Manchester data







