# Export a person-level linkage file of demographic attributes that are derived
# by the synthetic population pipeline but not carried by the SILO/MITO microdata
# written by exportSiloMicrodata.R.
#
# Chief among these is `education` (see the commented-out `education = education`
# in exportSiloMicrodata.R), which SILO's pp format has no field for.
#
# Output joins to ../microData/pp_<base_year>.csv one-to-one on `id`.

library(dplyr)
library(data.table)
library(logger)

# load general purpose utility functions
source("util.R")

# Load project properties (includes base year)
properties_file <- "../project.properties"
sourceProperties(properties_file)

populationFile <- "../output/synthetic_population/population_final.rds"
ppFile <- paste0("../microData/pp_", base_year, ".csv")
outputFile <- paste0("../microData/pp_extended_", base_year, ".csv")

log_info(paste0("Loading ", populationFile))
population <- readRDS(populationFile)
setDT(population)

# Columns already carried by the exports in exportSiloMicrodata.R:
#   pp_<base_year>.csv  AgentId (id), HouseholdId (hhid), Age (age),
#                       Gender (gender), RelationshipStatus (relationship),
#                       assigned_school (schoolId), SA1_MAINCODE_2016 (home_SA1)
#   hh_<base_year>.csv  hhSize, hhCar (autos)
alreadyExported <- c(
    "AgentId", "HouseholdId", "Age", "Gender", "RelationshipStatus",
    "assigned_school", "SA1_MAINCODE_2016", "hhSize", "hhCar"
)

# Nothing is withheld. (RelativeIds was previously excluded because a typo at
# collatePopulation.R:104 assigned it from ChildrenIds, making it byte-identical
# and therefore misleading to publish. That typo was fixed in commit 7047a21, so
# RelativeIds now carries genuine relative links and is exported below.)
excluded <- character(0)

# Attributes to publish, in reporting order.
extendedColumns <- c(
    # provenance
    "AgentIdOld",      # identifier in the source AgentsOz population
    # spatial identifiers other than the SA1 maincode already in pp
    "SA2_MAINCODE",
    "SA1_7DIGCODE",
    # derived demographics
    "age_cat",         # 21-level ABS five-year age group used for census joins
    "education",       # low / medium / high (ASCED-ISCED correspondence)
    "is_employed",     # not recoverable from pp `occupation`: employed students
                       # and non-employed students both code to occupation 3
    "student_status",
    "school_type",
    "school_grade",
    # neighbourhood socioeconomic position (SEIFA deciles, SA1 level)
    "IRSD",
    "IRSAD",
    # household composition links (AgentId references)
    "PartnerId",
    "MotherId",
    "FatherId",
    "ChildrenIds",     # comma-separated AgentIds
    "RelativeIds"      # comma-separated AgentIds (other relatives in household)
)

# Fail loudly if an expected attribute has disappeared upstream.
missingColumns <- setdiff(extendedColumns, names(population))
if (length(missingColumns) > 0) {
    stop(
        "Expected columns absent from ", populationFile, ": ",
        paste(missingColumns, collapse = ", ")
    )
}

# Warn if the pipeline has gained an attribute that is neither exported to the
# SILO microdata nor included here, so this file does not silently fall behind.
unaccountedFor <- setdiff(
    names(population),
    c(alreadyExported, excluded, extendedColumns)
)
if (length(unaccountedFor) > 0) {
    log_warn(paste0(
        "Columns in ", populationFile, " that are neither exported by ",
        "exportSiloMicrodata.R nor included here - review whether they belong ",
        "in the extended demographics: ", paste(unaccountedFor, collapse = ", ")
    ))
}

log_info(paste0("Preparing pp_extended_", base_year, " demographic linkage data"))

extendedDemographics <- population[, c("AgentId", extendedColumns), with = FALSE]
setnames(extendedDemographics, "AgentId", "id")
setkey(extendedDemographics, id)

if (anyDuplicated(extendedDemographics$id) > 0) {
    stop("Duplicate person ids; the linkage file would not join one-to-one to ", ppFile)
}

# Confirm the linkage against the pp microdata, if it has been written.
if (file.exists(ppFile)) {
    ppIds <- fread(ppFile, select = "id")$id
    unmatched <- length(setdiff(ppIds, extendedDemographics$id))
    if (unmatched > 0) {
        log_warn(paste0(
            unmatched, " of ", length(ppIds), " ids in ", ppFile,
            " have no matching record here; ", ppFile,
            " may have been generated from a different population_final.rds"
        ))
    } else {
        log_info(paste0("Linkage check passed: all ", length(ppIds), " ids in ",
                        basename(ppFile), " are present"))
    }
} else {
    log_warn(paste0(ppFile, " not found; skipping linkage check"))
}

log_info(paste0("Writing ", outputFile))
# fwrite rather than write.csv: ~4.2 million rows
fwrite(extendedDemographics, outputFile)

log_info(paste0(
    "Wrote ", nrow(extendedDemographics), " persons and ",
    ncol(extendedDemographics), " columns to ", outputFile
))
