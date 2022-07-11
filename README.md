# clusterMLD_ShinyApp
The shiny app implementing package `clusterMLD` to visualize the process and results of clustering longitudinal data


## Usage
Website exhibit:
https://junyzhou.shinyapps.io/clusterMLD_ShinyApp/

Run in R: 
```r
shiny::runGitHub("clusterMLD_ShinyApp", "junyzhou10", ref = "main")
```

Certain R-packages are required: `shiny`, `dplyr`, `DT`, `shinydashboard`, `doMC` (or `doParallel` for windows), `plotrix`, `plotly`, `stats`, `shinyhelper`, and `clusterMLD`. So make sure the packages have already been installed beforehand. 

(Notice that package clusterMLD is not from CRAN. For installation, please refer to [clusterMLD](https://github.com/junyzhou10/clusterMLD))

## Sample data
We provide two sample datasets in the file to play with, namely `LongDat` and `LongDat2`. In each dataset, there are 8 variables: `obs` for observational time, `y_1`,...,`y_5` for 5 outcomes, `id` indicates the corresponding subject id, and `label` for true labels. All data are in long format.

