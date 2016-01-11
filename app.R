################################### # # # # ##
####### Running the Application #######

# Sourcing Read.R to read the source file.
source("read.R")

#preparing data for the UI
# calculating and adding Interquartile Range ratio
z <- aggregate (srcdt$prevalence, by=list(srcdt$u_Time), FUN=IQR,na.rm = TRUE)
z[is.na(z)] <- 0
names(z)[1]<-paste("u_Time")
names(z)[2]<-paste("iqr")
datUI <- merge(srcdt, z, by="u_Time")
mean <- mean(datUI$iqr,na.rm = TRUE)
datUI$iqr <- (datUI$iqr)/mean
# calculating and adding Standard Deviation ratio
z2 <- aggregate (srcdt$prevalence, by=list(srcdt$u_Time), FUN=sd,na.rm = TRUE)
z2[is.na(z2)] <- 0
names(z2)[1]<-paste("u_Time")
names(z2)[2]<-paste("std")
datUI <- merge(datUI, z2, by="u_Time")
mean2 <- mean(datUI$std,na.rm = TRUE)
datUI$std <- (datUI$std)/mean2
#setting the range date after 1980
datUI <- subset(datUI, datUI$u_Time >= 1980)
rm(z,z2)

ui <- navbarPage(title = "Variability Explorere Tool", 
                 theme = shinytheme("Journal"),
                        tabPanel("Exploratory Analysis",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectizeInput(
                                       'var', label = "Select Data", choices = unique(datUI$u_Cond), options = list(placeholder = 'select a condition/group')
                                     ),

                                     helpText("text will go here"),
                                     sliderInput("slider", "Select a Time Unit Range",
                                                 min = 1980, max = 2014, value = c(1999, 2014)),

                                     helpText("text will go here"),
                                     sliderInput("slider2", "Select Interquartile Range",
                                                 min =min(datUI$iqr), max = 10, value = c(min(datUI$iqr),max(datUI$iqr)), step = 0.1),
   
                                     helpText("text will go here"),
                                     sliderInput("slider3", "Select Deviation Range",
                                                 min =min(datUI$std), max = 10, value = c(min(datUI$std),max(datUI$std)), step = 0.1)
                                     ),
                                   mainPanel(
                                     plotOutput("myplot", height = 900)
                                   )
                                 )
                        ),
                 tabPanel("Regression-Based Analysis",
                          sidebarLayout(
                            sidebarPanel(
                              selectizeInput(
                                'varREG', label = "BB", choices = unique(datUI$u_Cond), options = list(placeholder = 'select a condition')
                              ),
                              
                              helpText("text will go here"),
                              sliderInput("sliderREG", "Select Time Unit Range",
                                          min = 1980, max = 2014, value = c(1999, 2014)),
                              

                              helpText("text will go here"),
                              sliderInput("slider5", "Select Polynomial Degree",
                                          min =1, max = 5, value = 2, step = 1)
                            ),
                            mainPanel(
                              plotOutput("myplot4", height = 600),
                              dataTableOutput("mytable")
                            )
                          )
                 ),
                 
                 tabPanel("Treemap",
                          sidebarLayout(
                            sidebarPanel(
                              selectizeInput(
                                'var', label = "Select Condition", choices = unique(datUI$u_Cond), options = list(placeholder = 'select a u_Cond of variables')
                              ),
                              
                              helpText("text will go here"),
                              sliderInput("slider", "Select Time Unit Range",
                                          min = 1980, max = 2014, value = c(1999, 2014)),
                              
                              helpText("text will go here"),
                              sliderInput("slider4", "Select Standard Deviation for Treemap",
                                          min =0, max = 3, value = 1, step = 0.1)
                            ),
                            mainPanel(
                              plotOutput("myplot3", height = 600)
                            )
                          )
                 )
                 
                        
)


server <- function(input, output) { 
  dat <- reactive({
    subset(srcdt, srcdt$u_Time >= min(input$slider) & srcdt$u_Time <= max(input$slider) & srcdt$u_Cond ==input$var)
  })
  datREG <- reactive({
    subset(srcdt, srcdt$u_Time >= min(input$sliderREG) & srcdt$u_Time <= max(input$sliderREG) & srcdt$u_Cond ==input$varREG)
  })
  output$myplot <- renderPlot({
    ##do a bunch of processes on data to calculate interquartile range (iqr), standard deviation (sd), mean of total population (mean)
    ### and mean of patient within each u_Time (meanP)
    
    ##calculate interquartile range of weighted patient size
    z <- aggregate (dat()$prevalence, by=list(dat()$u_Time), FUN=IQR,na.rm = TRUE)
    z[is.na(z)] <- 0
    names(z)[1]<-paste("u_Time")
    names(z)[2]<-paste("iqr")
    dat2 <- merge(dat(), z, by="u_Time")
    ##interquartile range is now added as a new column and saved into a new table, dat2
    #calculate mean of iqr
    mean <- mean(dat2$iqr,na.rm = TRUE)
    #re-calculate iqr to store a ratio for iqr at a given u_Time over the mean of iqr over all u_Times
    dat2$iqr <- (dat2$iqr)/mean
    ##calculate standard deviation of the weighted patient size
    z2 <- aggregate (dat()$prevalence, by=list(dat()$u_Time), FUN=sd,na.rm = TRUE)
    z2[is.na(z2)] <- 0
    names(z2)[1]<-paste("u_Time")
    names(z2)[2]<-paste("std")
    dat3 <- merge(dat2, z2, by="u_Time")
    ##standard deviation is now added as a new column and saved into a new table, dat3
    #calculate mean of std
    mean2 <- mean(dat3$std,na.rm = TRUE)
    #re-calculate std to store a ratio for std at a given u_Time over the mean of std over all u_Times
    dat3$std <- (dat3$std)/mean2
    
    ##calculate mean of weighted patient size by u_Time
    z3 <- aggregate (dat()$prevalence, by=list(dat()$u_Time), FUN=mean,na.rm = TRUE)
    z3[is.na(z3)] <- 0
    names(z3)[1]<-paste("u_Time")
    names(z3)[2]<-paste("mean")
    dat4 <- merge(dat3, z3, by="u_Time")
    
    ##remove not needed objects
    rm(dat2,dat3,z,z2,z3)
    
    ##mean of wieghten patient size is now added as a new column and saved into a new table, dat4
    #calculate mean of actual patient size
    #    z4 <- aggregate (dat()$patient, by=list(dat()$factor), FUN=mean,na.rm = TRUE)
    #    z4[is.na(z4)] <- 0
    #    names(z4)[1]<-paste("factor")
    #    names(z4)[2]<-paste("meanP")
    #    dat5 <- merge(dat4, z4, by="factor")
    # ##mean of actual patient size is now added as a new column and saved into a new table, dat5
    
    ##setting values for plots from the UI    
    mn1 <- min(input$slider2,na.rm = TRUE)
    mx1 <- max(input$slider2,na.rm = TRUE)
    mn2 <- min(input$slider3,na.rm = TRUE)
    mx2 <- max(input$slider3,na.rm = TRUE)
    year_m <- min(input$slider)
    
    ##plotting: 
    ## first plot, d1, is a box plot with jittered points in the background. Users can change the Interquartile Range ratio to choose their range to select u_Times with high variability
    d1 <-   qplot(as.factor(u_Time), prevalence, data=dat4, geom=c("boxplot", "jitter"), 
                  fill=iqr, main="Prevalence by Location-Time -- Based on Interquartile Range",   
                  alpha=I(1/2), aes(color=u_Time),
                  xlab="Time Unit", ylab="Prevalence") + 
      scale_fill_continuous(low="gold", high="red", limits=c(mn1,mx1)) 
    
    ## second plot, d2, is a box plot with jittered points in the background. Users can change the Standard Deviation ratio to choose their range to select u_Times with high variability
    d2 <-   qplot(as.factor(u_Time), prevalence, data=dat4, geom=c("boxplot", "jitter"), 
                  fill=std, main="Prevalence by Location-Time -- Based on Standard Deviation Range",   
                  alpha=I(1/2), aes(color=u_Time),
                  xlab="Time Unit", ylab="Prevalence") +
      scale_fill_continuous(low="gold", high="red", limits=c(mn2,mx2))
    
    ## third plot, d3, is a scatter plot of  weighted patient size (prevalence) with jittered points in the background. 
    ##A smoothed regression line shows the overall trend in prevalence of selected cohort of patients over time.
    d3 <- qplot(u_Time, prevalence, data=dat4, main="Prevalence Over Time",
                xlab="Time Unit", ylab="Prevalence") + 
      stat_smooth(level=0.99) + 
      geom_point(alpha = 0.3) + 
      scale_x_continuous(breaks=year_m:2014)
    
    ## fourth plot, d4, is a scatter plot of  total patient size (population) with jittered points in the background. 
    ##A smoothed regression line shows the overall patient size from each clinic over time.
    d4 <- qplot(u_Time, population, data=dat4, main="Overall Patient Population Over Time",
                xlab="Time Unit", ylab="Overall Patient Population") + 
      stat_smooth(colour = "red",level=0.99) + 
      geom_point(alpha = 0.3) + 
      scale_x_continuous(breaks=year_m:2014)
    
    ##arranging the plots using grid.arrange
    grid.arrange(d1, d2, d3, d4, nrow=4)
    
  })
  
  
  output$myplot3 <- renderPlot({
    ##the treemap in this plot shows u_Times in which standard deviation ratio (u_Time's std/all u_Times' std) is bigger than or equal to an entry from the UI 
    #add std to a new table, dat2
    z <- aggregate (dat()$prevalence, by=list(dat()$u_Time), FUN=sd,na.rm = TRUE)
    z[is.na(z)] <- 0
    names(z)[1]<-paste("u_Time")
    names(z)[2]<-paste("std")
    dat2 <- merge(dat(), z, by="u_Time")
    
    #calculate std ratio
    meanstd <- mean(dat2$std,na.rm = TRUE)
    dat2$std <- (dat2$std)/meanstd
    
    
    #calculate index for when std is bigger than the average selected
    dat2$index <- ifelse(dat2$std>=(input$slider4), 1, 0)
    
    #generate 1 cell for counting number of data points within each u_Time
    dat2$numb <- 1
    
    #plot the treemap
    
    treemap(dat2, index=c('index','u_Time'), vSize='numb', title = 'number of records with high variability in each time unit',
            fontsize.title = 30)
    
  })
  
  
  output$myplot4 <- renderPlot({
    ##ignore the subsets
    ##training set
    #data2 <- subset(data, u_Time <2012 & u_Time > 1999) ##this should be data-1 u_Time
    #data3 <- subset(data, u_Time == 2013) ##this is the last u_Time requested for if we want to show the last u_Time data in a different color...data2+data3=data4
    #names(data)
    
    dat2 <- datREG()
    xdata <- datREG()
    dat2$prd <- 0
    dat2$lowSE <- 0
    dat2$highSE <- 0
    ##for loop begins to calculate linear regression models for each u_Loc
    for(i in 1:length(unique(dat2$u_Loc))){
      x <- unique(dat2$u_Loc)[i]
      idX <-  which(dat2$u_Loc == x)
      xdata <- dat2[idX,]
      fit <- lm(xdata$patient~xdata$u_Time,data=xdata)
      predObj <- predict(fit,newdata=xdata,interval="confidence"
                         , level = 0.95,type="response")
      dat2$prd[idX] <- predObj[,1]
      dat2$lowSE[idX] <- predObj[,2]
      dat2$highSE[idX] <- predObj[,3]
    }
    
    
    dat2$anom <- ifelse(dat2$patient>dat2$highSE | dat2$patient<dat2$lowSE, 1, 0)
    datanom <- subset(dat2, dat2$anom == 1)
    
    
    ###polynomia model
    #   dat2 <- dat()
    #   xdata <- dat()
    dat2$prd2 <- 0
    dat2$lowSE2 <- 0
    dat2$highSE2 <- 0
    for(i in 1:length(unique(dat2$u_Loc))){
      x <- unique(dat2$u_Loc)[i]
      idX <-  which(dat2$u_Loc == x)
      xdata <- dat2[idX,]
      fit2 <- lm(patient~poly(u_Time,input$slider5, raw=TRUE),data=xdata)
      predObj2 <- data.frame(predict(fit2,newdata=xdata,interval="confidence",
                                     level = 0.95,type="response",se = TRUE))
      dat2$prd2[idX] <- predObj2[,1]
      dat2$lowSE2[idX] <- predObj2[,2]
      dat2$highSE2[idX] <- predObj2[,3]
    }
    
    dat2$anom2 <- ifelse(dat2$patient>dat2$highSE2 | dat2$patient<dat2$lowSE2, 1, 0)
    datanom2 <- subset(dat2, dat2$anom2 == 1)
    
    par(mfrow=c(2,1), oma = c(1,0,1,0) ,mar = c(1.5,0,2,0))

    ##regular plot of data with linear regression method to show outliers
    plot(dat2$patient~dat2$u_Time,  xlab="", ylab="Patient", type = "b" , main = "Outliers with Linear Regression", 
         xlim=c(min(input$sliderREG),max(input$sliderREG)), pch=0) ##
    points(datanom$patient~datanom$u_Time, col = "red", pch=15)

    ##regular plot of data with polynomial regression method to show outliers
    plot(dat2$patient~dat2$u_Time,  xlab="u_Time", ylab="Patient", type = "b" , main = "Outliers with Polynomial Regression", 
         xlim=c(min(input$sliderREG),max(input$sliderREG)), pch=1) ##add x and y mins later-- 
    points(datanom2$patient~datanom2$u_Time, col = "red", pch=19)

  })
  
  output$mytable <- renderDataTable({
    datTB <- datREG()
    xdataTB <- datREG()
    datTB$prdTB <- 0
    datTB$lowSETB <- 0
    datTB$highSETB <- 0
    ##for loop begins to calculate linear regression models for each u_Loc
    for(i in 1:length(unique(datTB$u_Loc))){
      x <- unique(datTB$u_Loc)[i]
      idX <-  which(datTB$u_Loc == x)
      xdataTB <- datTB[idX,]
      fitTB <- lm(xdataTB$patient~xdataTB$u_Time,data=xdataTB)
      predObjTB <- predict(fitTB,newdata=xdataTB,interval="confidence"
                         , level = 0.95,type="response")
      datTB$prdTB[idX] <- predObjTB[,1]
      datTB$lowSETB[idX] <- predObjTB[,2]
      datTB$highSETB[idX] <- predObjTB[,3]
    }
    
##polynomial model
    datTB$prd2TB <- 0
    datTB$lowSE2TB <- 0
    datTB$highSE2TB <- 0
    for(i in 1:length(unique(datTB$u_Loc))){
      x <- unique(datTB$u_Loc)[i]
      idX <-  which(datTB$u_Loc == x)
      xdataTB <- datTB[idX,]
      fitTB2 <- lm(patient~poly(u_Time,input$slider5, raw=TRUE),data=xdataTB)
      predObjTB2 <- data.frame(predict(fitTB2,newdata=xdataTB,interval="confidence",
                                     level = 0.95,type="response",se = TRUE))
      datTB$prdTB2[idX] <- predObjTB2[,1]
      datTB$lowSETB2[idX] <- predObjTB2[,2]
      datTB$highSETB2[idX] <- predObjTB2[,3]
    }
    
    datTB$LINEAR <- ifelse(datTB$patient>datTB$highSETB | datTB$patient<datTB$lowSETB, 1, 0)

    datTB$POLY <- ifelse(datTB$patient>datTB$highSETB2 | datTB$patient<datTB$lowSETB2, 1, 0)

    #datTB
    diff<- subset(datTB,datTB$LINEAR != datTB$POLY)
    diff[,.(u_Loc, u_Time, LINEAR, POLY)]

  }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  # })
  
  
  
  output$sum <- renderPrint({
    data <- read.table("database.csv", header=T, sep=',')
    data$factor = as.factor(data$factor) 
    data$patient <- data$prevalence
    data$prevalence <- data$prevalence/data$population
    data4 <- subset(data, data$u_Time >= min(input$slider) & data$u_Time <= max(input$slider) & data$u_Cond ==input$var)    
    u_Timem <- min(input$slider)  
    
    
    summary(dat())})
  
  
  }

shinyApp(ui, server)


