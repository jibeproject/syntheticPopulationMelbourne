# syntheticPopulationMelbourne
Code for generating the synthetic population for Melbourne

```mermaid
flowchart TD
    subgraph makeExamplePopulation.R
        direction LR 
        collatePopulation.R --> determineEmployed.R --> prepWorkData.R --> assignWorkLocations.R
    end
    makeExamplePopulation.R<-- imports ---Util.R
    makeExamplePopulation.R --> analyseWorkLocations.R
    subgraph "Unused code":
        direction LR
        assignWorkLocationsOld.R
        cleanWorkData.R
        importPopulation.R
    end
```
