# syntheticPopulationMelbourne
Code for generating the synthetic population for Melbourne

```mermaid
flowchart TD
    subgraph makeExamplePopulation.R
        direction TB 
        collatePopulation.R -->
        determineEmployed.R -->
        prepWorkData.R -->
        assignWorkLocations.R -->
        determineEducationLevel.R -->
        determineHouseholdCar.R
    end
    i@{ shape: docs, label: "input files"}--> makeExamplePopulation.R
    Util.R--imported by-->makeExamplePopulation.R
    makeExamplePopulation.R -->
    o@{ shape: docs, label: "output files"} -->
    analyseWorkLocations.R
```
