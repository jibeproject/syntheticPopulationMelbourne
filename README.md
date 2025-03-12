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
    Util.R--imported by-->makeExamplePopulation.R
    makeExamplePopulation.R -->
    C@{ shape: docs, label: "output files"} -->
    analyseWorkLocations.R
```
