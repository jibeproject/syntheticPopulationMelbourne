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

## Output
Results are output to the following folders:
- `../output/synthetic_population`
- `../microData`

## Workflow 
```mermaid
flowchart TB
subgraph makeJibeMelbournePopulation.R
    direction LR
    subgraph Preprocessing
        direction TB
        subgraph Preparation
            collatePopulation.R
        end
        subgraph Employment ["Employment status"]
            direction LR
            determineEmployed.R
        end
        subgraph Education ["Education level"]
            determineEducationLevel.R
        end
        subgraph Car ["Car ownership"]
            determineHouseholdCar.R
        end
        subgraph Students ["Student status"]
            direction LR
            determineStudentSchools.R
        end
    end
    subgraph Postprocessing
        direction TB
        population@{ shape: docs, label: "../output/synthetic_population/population_final.rds"} -->
        assignWorkLocations.R -->
        workers@{ shape: docs, label: "../output/synthetic_population/workers_sa1_balance1.rds"} -->
        exportSiloMicrodata.R -->
        microdata@{ shape: docs, label: "../microData/pp_2018.csv\n../microData/dd_2018.csv\n../microData/hh_2018.csv\n../microData/jj_2018.csv\n../microData/ss_2018.csv"}
    end
end
Preprocessing --> Postprocessing
Preparation --> Employment --> Education --> Car --> Students
input@{ shape: docs, label: "data/*\nabs/*\n"}--> makeJibeMelbournePopulation.R
Util.R--imported by-->makeJibeMelbournePopulation.R
```
