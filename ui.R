####################################
########### Reading the source file
pdf(NULL)
####  Install and load the required packages
if (!require("data.table")) install.packages('data.table',repos = "http://cran.us.r-project.org")
if (!require("shiny")) install.packages('shiny',repos = "http://cran.us.r-project.org")
if (!require("ggplot2")) install.packages('ggplot2',repos = "http://cran.us.r-project.org")
if (!require("gridExtra")) install.packages('gridExtra',repos = "http://cran.us.r-project.org")
if (!require("dplyr")) install.packages('dplyr',repos = "http://cran.us.r-project.org")
if (!require("shinydashboard")) install.packages('shinydashboard',repos = "http://cran.us.r-project.org")
if (!require("shinythemes")) install.packages('shinythemes',repos = "http://cran.us.r-project.org")
if (!require("plotly")) install.packages('plotly',repos = "http://cran.us.r-project.org")
if (!require("DT")) install.packages('DT',repos = "http://cran.us.r-project.org")

path <- getwd()
setwd(paste0(path))

# Reading the source data.
## set the source data location:
src <- "testdata.csv"
## read the data from source
myfile <- file.path(path, "testdata.csv") 
## read the data from source
srcdt <- read.csv(myfile, header=T)#fread(src)
## look at data structure
str(srcdt)
## create a new factor variable out of the time unit 
srcdt$factor <- as.factor(srcdt$u_Time) 

#setting the range date for UI to after 1980
datUI <- subset(srcdt, srcdt$u_Time >= 1980)


shinyUI(navbarPage(title = "Variability Explorer Tool", 
                 theme = shinytheme("journal"),
                 tabPanel("  Variability Preview  ",
                          sidebarLayout(
                            sidebarPanel(
                              helpText("Select the condition(s) of interest"),
                              selectizeInput(
                                'var0', label = "Select Data", choices = unique(datUI$u_Cond), options = list(placeholder = 'select condition(s) of interest'),
                                selected = unique(datUI$u_Cond)[1], 
                                multiple = T
                              ),
                              
                              sliderInput("slider0", "Select a Time Unit Range",
                                          min = 1980, max = 2014, value = c(2004, 2013), step = 5
                              ),
                              
                              helpText("Time units are set to change in 5 unit
                                       intervals for speed.")
                              
                              ),
                            mainPanel(
                              plotlyOutput("myplot0", height = 400),
                              helpText("Hover over the Box plot to see the actual values for Max, 3rd quantile, mean (in red), 
                                       1 standard error over mean (in blue), median, 1st quantile, and Min.", height=100),
                              br(),
                              plotlyOutput("myplot00", height = 400),
                              helpText("Size of the poits on Scatter plot represent log of population at each location unit and time unit.
                                       Hover over to see values for the prevalence and log of population size for each location unit.", height=100),
                              br()
                              
                              )
                            )
                 ),
                 tabPanel("Exploratory Analysis",
                          sidebarLayout(
                            sidebarPanel(
                              helpText("Select the condition(s) of interest"),
                              selectizeInput(
                                'var', label = "Select Data", choices = unique(datUI$u_Cond), options = list(placeholder = 'select condition(s) of interest'),
                                selected = unique(datUI$u_Cond)[1], 
                                multiple = T
                              ),
                              
                              sliderInput("slider", "Select a Time Unit Range",
                                          min = 1980, max = 2014, value = c(2004, 2013)),
                              br(),
                              helpText("Select IQR/SD ranges that you would consider as high variability"),
                              sliderInput("slider2", "Select Interquartile Range (IQR)",
                                          min =0, max = 10, value = c(1,6), step = 0.5),
                              sliderInput("slider3", "Select Deviation Range (SD)",
                                          min =0, max = 10, value = c(1,6), step = 0.5),
                              HTML("<I>Outliers are marked in red on Box plots.</I>")
                              
                            ),
                            mainPanel(
                              plotOutput("myplot", height = 1200)
                            )
                          )
                 ),
                 tabPanel("       Density Plots      ",
                          sidebarLayout(
                            sidebarPanel(
                              helpText("Select the condition(s) of interest"),
                              selectizeInput(
                                'varden', label = "Select Condition", choices = unique(datUI$u_Cond), options = list(placeholder = 'select condition(s) of interest'),
                                selected = unique(datUI$u_Cond)[1], 
                                multiple = T
                              ),
                              br(),
                              
                              helpText("Select the variable you wish to observe its density distribution."),
                              selectizeInput(
                                'varden2', label = "Select Variable", choices=names(datUI)[4:6], options = list(placeholder = 'select variable of interest'),
                                selected = names(datUI)[6], 
                                multiple = F
                              ),
                              br(),
                              sliderInput("sliderden", "Select Time Unit Range",
                                          min = 1980, max = 2014, value = c(2004, 2013)),
                              br(),
                              HTML("<I>Density plots are complimentory to the visualizations 
                                   in the Exploratory Analysis tab.</I>")
                              
                              ),
                            mainPanel(
                              plotlyOutput("myplot3", height = 600)
                            )
                          )
                          ),
                 
                 tabPanel("Regression-Based Analysis",
                          sidebarLayout(
                            sidebarPanel(
                              helpText("Select the condition(s) of interest"),
                              selectizeInput(
                                'varREG', label = "Select Condition", choices = unique(datUI$u_Cond), options = list(placeholder = 'select condition(s) of interest'),
                                selected = unique(datUI$u_Cond)[1], 
                                multiple = T
                              ),
                              br(),
                              
                              sliderInput("sliderREG", "Select Time Unit Range",
                                          min = 1980, max = 2014, value = c(2004, 2013)),
                              br(),
                              
                              helpText("Select the smoothing degree for the regression model 1."),
                              sliderInput("slider50", "Select Polynomial Degree",
                                          min =1, max = 5, value = 1, step = 1),
                              br(),
                              
                              
                              helpText("Select the smoothing degree for the regression model 2."),
                              sliderInput("slider5", "Select Polynomial Degree",
                                          min =1, max = 5, value = 1, step = 1),
                              br(),
                              helpText("The table highlights location and time units in which there is consensus betweeb
                                       the results obtained from the two polynomial models.")
                              
                              ),
                            mainPanel(
                              plotOutput("myplot4", height = 600),
                              DT::dataTableOutput("mytable")
                            )
                          )
                          )
                 
                 
                 
                 
)
)