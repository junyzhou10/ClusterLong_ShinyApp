# ClusterLong_ShinyApp
The shiny app implementing package ClusterLong to visualize the process and results of clustering longitudinal data


## Usage
Directly run in R: shiny::runGitHub("ClusterLong_ShinyApp", "junyzhou10", ref = "main")

Certain R-packages are required: shiny, dplyr, DT, shinydashboard, doMC, plotrix, ClusterLong. So make sure the packages have already been installed beforehand.

(Notice that package ClusterLong is not from CRAN. For installation, please refer to https://github.com/junyzhou10/ClusterLong)

## Sample data
We provide two sample datasets in the file, namely LongDat and LongDat2, to play with. In each dataset, there are 8 variables: obs for observational time, y_1,...,y_5 for 5 outcomes, id indicates the corresponding subject id, and label for true label. All data is stored in long format.

