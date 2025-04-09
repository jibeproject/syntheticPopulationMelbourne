# syntheticPopulationMelbourne
Code for generating the synthetic population for the JIBE Melbourne model.

## Requirements
- R 4.0.0 or higher
- R packages: `install.packages(c('tidyverse','data.table','sf','furrr','logger','stringr','igraph','future')`
- for JIBE team, copy content of `data` and `abs` folders from the JIBE Melbourne model repository to the same directory as this repository. 

## Usage
- clone this repository to the `melbourne` folder of the JIBE Melbourne model
- open a terminal to the repository directory
- run the following command to generate the synthetic population:
```bash
Rscript ./makeJibeMelbournePopulation.R
```

## Workflow 
```mermaid
flowchart TD
    subgraph makeJibeMelbournePopulation.R
        direction TB 
        collatePopulation.R -->
        determineEmployed.R -->
        determineEducationLevel.R -->
        determineHouseholdCar.R -->
        determineStudentSchools.R
        determineEmployed.R -->
        assignWorkLocations.R
    end
    input@{ shape: docs, label: "data/*\nabs/*\n"}--> makeJibeMelbournePopulation.R
    Util.R--imported by-->makeJibeMelbournePopulation.R
    makeJibeMelbournePopulation.R -->
    population@{ shape: docs, label: "population_final.rds"}
    makeJibeMelbournePopulation.R -->
    workData@{ shape: docs, label: "workers_sa1_balance1.rds"} -->
    analyseWorkLocations.R
```
