#install.packages(c("data.table", "lubridate", "magrittr"), dependencies=TRUE)
require(data.table);library(dplyr)

# Read  the source data.
srcdt <- fread("~/OneDrive UW/OneDriveBusiness/Variability2/VET_II/database.csv")
srcdt$factor <- as.factor(srcdt$u_Time) 

names(srcdt)



