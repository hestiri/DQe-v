####################################
########### Reading the source file

####  Install and load the required packages
packages <- c("data.table", "shiny", "ggplot2", "gridExtra", "treemap", 
              "dplyr", "shinydashboard", "shinythemes","plotly")
# install.packages(packages, dependencies=TRUE) ## comment out this line after you run the application one time.
library(data.table);library(shiny);library(ggplot2);
library(gridExtra);require(treemap);require(dplyr);
library(shinydashboard);library(shinythemes);library(plotly)



# Reading the source data.
## set the source location:
src <- "~/Desktop/testdata.csv"
## read the data from source
srcdt <- fread(src)
## look at data structure
str(srcdt)
## create a new factor variable out of the time unit 
srcdt$factor <- as.factor(srcdt$u_Time) 




