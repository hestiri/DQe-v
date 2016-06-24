####################################
########### Reading the source file

####  Install and load the required packages
if (!require("data.table")) install.packages('data.table')
if (!require("shiny")) install.packages('shiny')
if (!require("ggplot2")) install.packages('ggplot2')
if (!require("gridExtra")) install.packages('gridExtra')
if (!require("treemap")) install.packages('treemap')
if (!require("dplyr")) install.packages('dplyr')
if (!require("shinydashboard")) install.packages('shinydashboard')
if (!require("shinythemes")) install.packages('shinythemes')
if (!require("plotly")) install.packages('plotly')




# Reading the source data.
## set the source data location:
src <- "testdata.csv"
## read the data from source
srcdt <- fread(src)
## look at data structure
str(srcdt)
## create a new factor variable out of the time unit 
srcdt$factor <- as.factor(srcdt$u_Time) 




