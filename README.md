# TREEM(Tracking the Energy and Emissions of MBTA Rapid Transit)

### Folder description

|  name | usage |
|  ----  | ----  |
| bin  | Save scripts with different programming languages.| 
| figure  | Save plots output by script.| 

### Script description

|  name | usage | type |
|  ----  | ----  | ----  |
| vehicle-trajectory-extraction  | Extract train location data from MBTA database. | jupyter notebook | 
| ridership-extraction  | Extract tap-in ridership data from MBTA database.  | jupyter notebook |  
| vehicle-trajectory-computation | Calculate vehicle distance, speed and acceleration for each line. | jupyter notebook |  
| variable-binning-aggregation | Aggregate the vehicle computation table in hour level. | jupyter notebook | 
| trajectory-analysis | Generate statistical table for each train and plot train trajectory.  | jupyter notebook |
| energy-system-model-estimation | Estimate system energy consumption model( linear regression, ridge regression and lasso). | jupyter notebook |  
