#install.packages(c("data.table"), dependencies=TRUE)
require(data.table)

# Reading the source data.
## set the source location:
src <- "~/Desktop/database.csv"
## read the data from source
srcdt <- fread(src)
## look at data structure
str(srcdt)
## create a new factor variable out of the time unit 
srcdt$factor <- as.factor(srcdt$u_Time) 




