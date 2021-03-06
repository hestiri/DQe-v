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
myfile <- file.path(path, "testdata.csv") 
## read the data from source
srcdt <- read.csv(myfile, header=T)#fread(src)
## look at data structure
str(srcdt)
## create a new factor variable out of the time unit 
srcdt$factor <- as.factor(srcdt$u_Time) 


shinyServer(
  
  function(input, output) { 
  dat0 <- reactive({
    subset(srcdt, srcdt$u_Time >= min(input$slider0) & srcdt$u_Time <= max(input$slider0) & srcdt$u_Cond ==input$var0)
  })
  dat <- reactive({
    subset(srcdt, srcdt$u_Time >= min(input$slider) & srcdt$u_Time <= max(input$slider) & srcdt$u_Cond ==input$var)
  })
  datden <- reactive({
    subset(srcdt, srcdt$u_Time >= min(input$sliderden) & srcdt$u_Time <= max(input$sliderden) & srcdt$u_Cond ==input$varden)
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
    
    
    ##setting values for plots from the UI    
    mn1 <- min(input$slider2,na.rm = TRUE)
    mx1 <- max(input$slider2,na.rm = TRUE)
    mn2 <- min(input$slider3,na.rm = TRUE)
    mx2 <- max(input$slider3,na.rm = TRUE)
    year_m <- min(input$slider)
    
    ##plotting: 
    ## first plot, d1, is a box plot with jittered points in the background. Users can change the Interquartile Range ratio to choose their range to select u_Times with high variability
    d1 <-   ggplot(dat4, aes(x=as.factor(u_Time), y=prevalence)) + 
      geom_jitter(width = 0.2, alpha = 0.3) +
      geom_boxplot(aes(fill=iqr), outlier.colour = "red", outlier.shape = 1, alpha = 0.5) +
      scale_fill_continuous(low="gold", high="red", limits=c(mn1,mx1), name="Interquartile Range Ratio",guide = FALSE) +
      theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", hjust=0)) +
      labs(title = "Prevalence by Location-Time -- Based on Interquartile Range") + labs(x = "Time Unit") + labs(y = "Prevalence")
    
    
    
    ## second plot, d2, is a box plot with jittered points in the background. Users can change the Standard Deviation ratio to choose their range to select u_Times with high variability
    d2 <-   ggplot(dat4, aes(x=as.factor(u_Time), y=prevalence)) + 
      geom_jitter(width = 0.2, alpha = 0.3) +
      geom_boxplot(aes(fill=std), outlier.colour = "red", outlier.shape = 1, alpha = 0.5) +
      scale_fill_continuous(low="gold", high="red", limits=c(mn2,mx2), name="Interquartile Range Ratio",guide = FALSE) +
      theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", hjust=0)) +
      labs(title = "Prevalence by Location-Time -- Based on Interquartile Range") + labs(x = "Time Unit") + labs(y = "Prevalence")
    
    
    
    ## third plot, d3, is a scatter plot of  weighted patient size (prevalence) with jittered points in the background. 
    ##A smoothed regression line shows the overall trend in prevalence of selected cohort of patients over time.
    d3 <- qplot(u_Time, prevalence, data=dat4, main="Prevalence Over Time", #geom = "jitter",
                xlab="Time Unit", ylab="Prevalence") + 
      stat_smooth(level=0.99) + 
      geom_point(alpha = 0.2) + 
      theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", hjust=0)) +
      scale_x_continuous(breaks=year_m:2014)
    
    ## fourth plot, d4, is a scatter plot of  total patient size (population) with jittered points in the background. 
    ##A smoothed regression line shows the overall patient size from each clinic over time.
    d4 <- qplot(u_Time, population, data=dat4, main="Overall Patient Population Over Time",
                xlab="Time Unit", ylab="Overall Patient Population") + 
      stat_smooth(colour = "red",level=0.99) + 
      geom_point(alpha = 0.3) + 
      theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", hjust=0)) +
      scale_x_continuous(breaks=year_m:2014)
    
    ##arranging the plots using grid.arrange
    grid.arrange(d1, d2, d3, d4, nrow=4)
    
  })
  
  output$myplot0 <- renderPlotly({
    
    z3 <- aggregate (dat0()$prevalence, by=list(dat0()$u_Time), FUN=mean,na.rm = TRUE)
    z3[is.na(z3)] <- 0
    names(z3)[1]<-paste("u_Time")
    names(z3)[2]<-paste("mean")
    datt <- merge(dat0(), z3, by="u_Time")
    
    st.err <- function(x, na.rm=FALSE) {
      if(na.rm==TRUE) x <- na.omit(x)
      sd(x)/sqrt(length(x))
    }
    z2 <- aggregate (dat0()$prevalence, by=list(dat0()$u_Time), FUN=st.err,na.rm = TRUE)
    z2[is.na(z2)] <- 0
    names(z2)[1]<-paste("u_Time")
    names(z2)[2]<-paste("st.err")
    datt <- merge(datt, z2, by="u_Time")
    
    p <- ggplot(datt, aes(x=as.factor(u_Time), y=prevalence)) + 
      geom_jitter(width = 0.2, alpha = 0.3) +
      geom_boxplot(aes(), outlier.shape = 1, alpha = 0.5) +
      scale_fill_continuous(low="gold", high="red",guide = FALSE) +
      geom_ribbon(aes(ymin = (mean - st.err), ymax = (mean + st.err)), fill = "blue") +
      geom_point(aes(y = mean), col = "red", shape = 95, size = 3, alpha = 0.5)+
      facet_wrap(~factor, scale="free_x",nrow = 1, switch = "x") +
      theme(axis.text.x=element_text(colour="white", size = 0.1)) +
      theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", hjust=0)) +
      labs(title = "Box Plot of Prevalence by Location-Time") + labs(x = "")+labs(y = "Prevalence")
    
    (gg <- ggplotly(p))
  })
  
  
  
  output$myplot00 <- renderPlotly({
    
    q <- ggplot(dat0(), aes(u_Time)) +
      geom_point(data = dat0(), aes(x = u_Time, y = prevalence, col = u_Loc, size = log(population), legend = F), alpha = 0.5)+ 
      scale_colour_hue(guide = FALSE)+guides(fill=FALSE)+
      scale_size(guide = FALSE)+ theme(legend.position="none") +
      facet_wrap(~factor, scale="free_x",nrow = 1, switch = "x") +
      theme(axis.text.x=element_text(colour="white", size = 0.1)) +
      theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", hjust=0)) +
      labs(title = "Scatter Plot of Prevalence by Location-Time") + labs(x = "Time Unit") + labs(y = "Prevalence")
    (gg <- ggplotly(q))
    
  })
  
  
  
  output$myplot3 <- renderPlotly({
    ## Density plots of the selected variable. 
    
    #to be able to pass the variable name to ggplot, you'll have to write it as a function.
    myplot = function(col) {
      ggplot(datden(), aes_string(x = col, fill = "factor", colour = "factor")) + 
        geom_density(alpha = 0.4, show.legend = FALSE) +
        theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", hjust=0)) +
        xlab(paste0(input$varden2," of ",input$varden, sep ="")) + ylab("Density") + 
        ggtitle(paste0("Density distribution of ",input$varden2," by year", sep = "")) +
        facet_wrap(~factor)
    }
    p <- myplot(input$varden2)
    (gg <- ggplotly(p))
  })
  
  #   output$text1 <- renderText({ 
  #     "* These visualizations are complimentory to the the visualizations in the Exploratory Analysis tab."
  #   })
  
  output$myplot4 <- renderPlot({
    
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
      fit <- lm(patient~poly(u_Time, input$slider50, raw=T)+poly(population, input$slider50, raw=T),data=xdata)
      predObj <- data.frame(predict(fit,newdata=xdata,interval="confidence",
                                    level = 0.95,type="response",se = TRUE))
      dat2$prd[idX] <- predObj[,1]
      dat2$lowSE[idX] <- predObj[,2]
      dat2$highSE[idX] <- predObj[,3]
    }
    
    
    dat2$anom <- ifelse(dat2$patient>dat2$highSE | dat2$patient<dat2$lowSE, 1, 0)
    datanom <- subset(dat2, dat2$anom == 1)
    
    
    ###polynomia model
    dat2$prd2 <- 0
    dat2$lowSE2 <- 0
    dat2$highSE2 <- 0
    for(i in 1:length(unique(dat2$u_Loc))){
      x <- unique(dat2$u_Loc)[i]
      idX <-  which(dat2$u_Loc == x)
      xdata <- dat2[idX,]
      fit2 <- lm(patient~poly(u_Time, input$slider5, raw=T)+poly(population, input$slider5, raw=T),data=xdata)
      predObj2 <- data.frame(predict(fit2,newdata=xdata,interval="confidence",
                                     level = 0.95,type="response",se = TRUE))
      dat2$prd2[idX] <- predObj2[,1]
      dat2$lowSE2[idX] <- predObj2[,2]
      dat2$highSE2[idX] <- predObj2[,3]
    }
    
    dat2$anom2 <- ifelse(dat2$patient>dat2$highSE2 | dat2$patient<dat2$lowSE2, 1, 0)
    datanom2 <- subset(dat2, dat2$anom2 == 1)
    
    p1 <-  ggplot(dat2, aes(u_Time,log10(patient), col = u_Loc,size = log10(population))) +
      geom_point(alpha = 0.7, show.legend = FALSE) + 
      geom_point(data = datanom, aes(u_Time, log10(patient)), shape = 21, colour = "black", fill = "white", size = 7, stroke = 1, alpha = 0.8) +
      geom_point(data = datanom2, aes(u_Time, log10(patient)), colour="red", size = 5.5, alpha = 0.8) +
      theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", hjust=0)) +
      xlab("") + ylab(paste0("LOG Number of Patient with ",input$varREG, sep ="")) + 
      ggtitle("Regression-based Anomaly Detection") +
      facet_wrap(~u_Time, scale="free_x", nrow = 1, switch = "x") +
      theme(axis.text.x=element_text(colour="white", size = 0.1)) 
    
    p2 <-  ggplot(dat2, aes(u_Time,log10(patient), label = u_Loc)) +
      geom_label(aes(fill = factor(u_Loc)), colour = "white", fontface = "plain", size = 3, show.legend = FALSE, alpha = 0.2)+
      geom_label(data = datanom2, aes(fill = factor(u_Loc)), colour = "white", fontface = "bold", size = 3.5, show.legend = FALSE, alpha = 0.95)+
      theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", hjust=0)) +
      xlab("Time Unit") + ylab(paste0("LOG Number of Patient with ",input$varREG, sep ="")) + 
      facet_wrap(~u_Time, scale="free_x", nrow = 1, switch = "x")  +
      theme(axis.text.x=element_text(colour="white", size = 0.1)) 
    
    grid.arrange(p1, p2, nrow=2)
    
    
  })
  
  output$mytable <- DT::renderDataTable({
    dat3 <- datREG()
    xdata3 <- datREG()
    dat3$prd <- 0
    dat3$lowSE <- 0
    dat3$highSE <- 0
    ##for loop begins to calculate linear regression models for each u_Loc
    for(i in 1:length(unique(dat3$u_Loc))){
      x <- unique(dat3$u_Loc)[i]
      idX <-  which(dat3$u_Loc == x)
      xdata3 <- dat3[idX,]
      fit <- lm(patient~poly(u_Time, input$slider50, raw=T)+poly(population, input$slider50, raw=T),data=xdata3)
      predObj1 <- data.frame(predict(fit,newdata=xdata3,interval="confidence",
                                    level = 0.95,type="response",se = TRUE))
      dat3$prd[idX] <- predObj1[,1]
      dat3$lowSE[idX] <- predObj1[,2]
      dat3$highSE[idX] <- predObj1[,3]
    }
    
    
    dat3$anom1 <- ifelse(dat3$patient>dat3$highSE | dat3$patient<dat3$lowSE, 1, 0)
    # datanom <- subset(dat2, dat2$anom == 1)
    
    
    ###polynomia model
    dat3$prd2 <- 0
    dat3$lowSE2 <- 0
    dat3$highSE2 <- 0
    for(i in 1:length(unique(dat3$u_Loc))){
      x2 <- unique(dat3$u_Loc)[i]
      idX2 <-  which(dat3$u_Loc == x2)
      xdata3 <- dat3[idX2,]
      fit2 <- lm(patient~poly(u_Time, input$slider5, raw=T)+poly(population, input$slider5, raw=T),data=xdata3)
      predObj2 <- data.frame(predict(fit2,newdata=xdata3,interval="confidence",
                                     level = 0.95,type="response",se = TRUE))
      dat3$prd2[idX2] <- predObj2[,1]
      dat3$lowSE2[idX2] <- predObj2[,2]
      dat3$highSE2[idX2] <- predObj2[,3]

    }
    
    dat3$anom2 <- ifelse(dat3$patient>dat3$highSE2 | dat3$patient<dat3$lowSE2, 1, 0)
    

dat3$POLY1 <- ifelse(dat3$anom1 == 1, "Anomaly", "-")

dat3$POLY2 <- ifelse(dat3$anom2 == 1, "Anomaly", "-")

# #datTB
cons <- subset(dat3,dat3$POLY1 == "Anomaly" & dat3$POLY2 == "Anomaly")
DT::datatable(select(cons,u_Loc, u_Time, POLY1, POLY2),
              options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })
  
  
  }
)
  
  # output$sum <- renderPrint({
  #   data <- read.table("database.csv", header=T, sep=',')
  #   data$factor = as.factor(data$factor)
  #   data$patient <- data$prevalence
  #   data$prevalence <- data$prevalence/data$population
  #   data4 <- subset(data, data$u_Time >= min(input$slider) & data$u_Time <= max(input$slider) & data$u_Cond ==input$var)
  #   u_Timem <- min(input$slider)
  # 
  # 
  #   summary(dat())})
  
#   
#   }
# )
