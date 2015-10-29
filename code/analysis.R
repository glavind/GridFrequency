
################
### ANALYSIS ###
################
#  #library(data.table)
  library(xts)
  library(lubridate) # convert date
  library(pryr) # report file size
  
  library(grid)
  library(latticeExtra)

  library(ggplot2)  # graph
  library(reshape2) # transform
  library(scales) # graph

  
  setwd("C:/Users/Glavind/git/GridFrequency")
  if(!file.exists("figs")){dir.create("figs", recursive = T)}

## Read data
  # Read raw 1/10 second data
  FC <- readRDS("data/freq_continental_processed.rds") # XTS
  FN <- readRDS("data/freq_nordic_processed.rds") # XTS
  
  ## Processed to 1 min data
  mFC <- readRDS("data/mFC.rds") # XTS - mean, max, min, std.dev
  mFN <- readRDS("data/mFN.rds") # XTS - mean, max, min, std.dev
  
  ## Processed to 5 min data
  m5FC <- readRDS("data/m5FC.rds") # XTS - mean, max, min, std.dev
  m5FN <- readRDS("data/m5FN.rds") # XTS - mean, max, min, std.dev
  
  ## Processed to 1 hour data
  hFC <- readRDS("data/hFC.rds") # XTS - mean, max, min, std.dev
  hFN <- readRDS("data/hFN.rds") # XTS - mean, max, min, std.dev
  
### A) Determine how many observations are missing.
### B) Plot series
### C) Calulate frequency drift. Plot accumulated drift.    
### D) Determine number of minutes with max above thresholds. Histogram of max.
### E) Determine number of minutes with min belov thresholds. Histogram of min.
### F) Level plot. To see patterns intraday and between month. Cut in weekday/sat/sun to check for transday effects if anything is found.

### A) Missing observations ###
  # Continental:
    cat("Continental: missing", sum(is.na(FC[,1])), "observations (", sprintf("%.4f",sum(is.na(FC[,1]))/nrow(FC)*100),"%)")
  # Nordic:
    cat("Nordic: missing", sum(is.na(FN[,1])), "observations (", sprintf("%.4f",sum(is.na(FN[,1]))/nrow(FN)*100),"%)")
  

### B) Plot hourly series ###
  #### Graph Settings #### 
    # From https://github.com/oscarperpinan/spacetime-vis
    xscale.components.custom <- function(...){
      ans <- xscale.components.default(...)
      ans$top=FALSE
      ans}
    yscale.components.custom <- function(...){
      ans <- yscale.components.default(...)
      ans$right=FALSE
      ans}
    myArgs <- list(as.table=TRUE,
                   between=list(x=0.5, y=0.2),
                   xscale.components = xscale.components.custom,
                   yscale.components = yscale.components.custom)
    defaultArgs <- lattice.options()$default.args
   
    ####
    # Basically the ggplot2like() theme function with some alterations
    myTheme <- trellis.par.get()
    myTheme <- modifyList(myTheme, list(axis.line = list(col = "black", lwd=1), 
                                      axis.text = list(cex = 0.8, lineheight = 0.9, col = "black"), 
                                      panel.background = list(col = "transparent"), 
                                      reference.line = list(col = "white"), 
                                      strip.background = list(col = "transparent"), 
                                      strip.shingle = list(col = "transparent"), 
                                      strip.border = list(col = "transparent"), 
                                      add.text = list(cex = 1.1, font=2),
                                      plot.symbol =  list(col = "black", pch = 19, cex = 0.6), 
                                      plot.line = list(col = "black"), 
                                      plot.polygon = list(col = "black", border = "transparent"), 
                                      dot.line = list(col = "black"), 
                                      dot.symbol = list(col = "black", pch = 19)))
    
    lattice.options(default.theme = myTheme, default.args = modifyList(defaultArgs, myArgs))
  #############################################################
  
  #### Continental ####
  dat<-hFC
  # Plot in 2 frames with std dev 
    freqPlotRaw <- xyplot(cbind(dat,50),
                     col=c("black","red","midnightblue","black", "black"),
                     superpose=TRUE, main="Continent",
                     auto.key=list(space="bottom", columns=4, title="", cex.title=1),
                     lwd=0.5, alpha=0.3, layout = c(1, 2),
                     screens=c(rep('Frequency - Continental',3),'Std.Dev','Frequency - Continental'))
    
    freqPlot <- update(freqPlotRaw, par.settings = myTheme, between = list(x = 0.3, y = 1))
    
    pdf(file="figs/hFC-freq-stddev.pdf")
    freqPlot
    dev.off()
    
  # Plot series cut in 3 frames
    freqPlotRaw <- xyplot(cbind(dat[,1:3],50),
                          col=c("black","red","midnightblue", "black"), main="Continent",
                          superpose=TRUE, strip=FALSE,
                          auto.key=list(space="bottom", text=c("mean","max","min","50Hz"), columns=4, title="", cex=0.8, cex.title=0.3),
                          lwd=0.5, alpha=0.3, layout = c(1, 3),
                          cut=list(n=3, overlap=0.1))
    freqPlotRaw
                          
    freqPlot <- update(freqPlotRaw, par.settings = myTheme, between = list(x = 0.3, y = 1))
    
    pdf(file="figs/hFC-freq-cut.pdf")
    freqPlot
    dev.off()
    
  ###### Nordic ######
  dat<-hFN
  # Plot in 2 frames with std dev 
    freqPlotRaw <- xyplot(cbind(dat,50),
                          col=c("black","red","midnightblue","black", "black"),
                          superpose=TRUE, auto.key=F,
                          lwd=0.5, alpha=0.3,
                          screens=c(rep('Frequency - Nordic',3),'Std.Dev','Frequency - Nordic'))
    
    freqPlot <- update(freqPlotRaw, par.settings = myTheme, layout = c(1, 2),
                       between = list(x = 0.3, y = 1))
    
    pdf(file="figs/hFN-freq-stddev.pdf")
    freqPlot
    dev.off()
    
  # Plot series cut in 3 frames
    freqPlotRaw <- xyplot(cbind(dat[,1:3],50),
                          col=c("black","red","midnightblue", "black"),
                          superpose=TRUE, auto.key=F, strip=FALSE,
                          lwd=0.5, alpha=0.3, layout = c(1, 3),
                          cut=list(n=3, overlap=0.1))
    
    freqPlot <- update(freqPlotRaw, par.settings = myTheme, between = list(x = 0.3, y = 1))
    
    pdf(file="figs/hFN-freq-cut.pdf")
    freqPlot
    dev.off()

### C) Frequency drift ###
    ## Continental
    dFC <- apply.daily(FC, mean, na.rm=T)
    index(dFC)<-as.Date(index(dFC))
    dFC<-cbind(dFC,(dFC[,1]-50)*3600*24/50)
    miss <- is.na(dFC)
    dFC[miss] <- 0
    dFC<-cbind(dFC,cumsum(dFC[,2]))
    names(dFC)<-c("FC.frequency", "FC.drift", "FC.cum.drift")
    
    ## Nordic
    dFN <- apply.daily(FN, mean, na.rm=T)
    index(dFN)<-as.Date(index(dFN))
    dFN<-cbind(dFN,(dFN[,1]-50)*3600*24/50) # drift
    miss <- is.na(dFN)
    dFN[miss] <- 0
    dFN<-cbind(dFN,cumsum(dFN[,2])) # cumm.drift
    names(dFN)<-c("FN.frequency", "FN.drift", "FN.cum.drift")
    
    ## Both
    dat<-fortify(cbind(dFC[,3],dFN[,3]))
    
    ## Plot drift of both series
    g <- ggplot(data=dat, aes(x=Index)) +
      geom_line(data=dat, aes(y=FN.cum.drift, color="Nordic")) +
      geom_line(data=dat, aes(y=FC.cum.drift, color="Continent")) +
      ggtitle("Cumulative drift") +
      labs(color="Series") +
      xlab("Time") + 
      ylab("Seconds") +
      theme_bw() +
      theme(legend.key = element_blank()) + #Remove box around legend
      scale_x_date(labels=date_format("%b",tz="Europe/Paris"), expand=c(0,0), breaks=date_breaks(width="1 month"))  + # Change x-axis
      #scale_colour_discrete(name="Frequency series", breaks=c("FN","FC"), labels=c("Nordic","Continental")) + #Change labels
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) # Rotate x-axis
    g
    
    g1 <- g + stat_smooth(method = "lm", data=dat, aes(y=FN.cum.drift, color="Nordic")) +
              stat_smooth(method = "lm", data=dat, aes(y=FC.cum.drift, color="Continent")) +
              theme(legend.position="none", legend.key=element_blank())
    g1
    ## Frequency setpoint corrections:
    (49.990 - 50)*3600*24/50
    (50.010 - 50)*3600*24/50
    # https://www.swissgrid.ch/swissgrid/en/home/experts/topics/frequency.html
    
    
### D/E) Number of extreme events
  ## Create histogram
    ## Continental
    dat <- coredata(mFC[,1])
    hist(dat, prob=TRUE, col="grey80", breaks = 60, main="hFC", prob=T)# prob=TRUE for probabilities not counts
    lines(density(dat, na.rm = T), col="midnightblue", lwd=2) 
    rug(dat) # Make a rug of observations underneath
    abline(v=49.90, lwd=2, col="red")
    abline(v=50.10, lwd=2, col="red")

### B) Plot averages across day ###
    ## Nordic
    dat <- mFN[,1]
    avFN <- aggregate(dat, by=format(index(dat), format = "%H:%M"), mean, na.rm=T)
    index(avFN)<- as.POSIXct(strptime(index(avFN),format="%H:%M"))
    colnames(avFN)<-c("freq")

    gplot<- ggplot(data=avFN, aes(x=index(avFN), y=freq)) +geom_line() + ggtitle("Average Frequency")
    gplot<- gplot + xlab("Time") + ylab("Frequency (Hz)")
    gplot<- gplot + theme_bw() + geom_line(y=50) 
    gplot<- gplot + scale_x_datetime(labels=date_format("%H:%M",tz="Europe/Paris"), expand=c(0,0), breaks=date_breaks(width="1 hour")) 
    gplot
    ggsave("figs/FN_avgAcrossDay.pdf",gplot, height=4, width=12)
    
    # Continental
    dat <- mFC[,1]
    avFC <- aggregate(dat, by=format(index(dat), format = "%H:%M"), mean, na.rm=T)
    index(avFC)<- as.POSIXct(strptime(index(avFC),format="%H:%M"))
    colnames(avFC)<-c("freq")
    
    gplot<- ggplot(data=avFC, aes(x=index(avFC), y=freq)) + geom_line() + ggtitle("Average Frequency")
    gplot<- gplot + xlab("Time") + ylab("Frequency (Hz)")
    gplot<- gplot + theme_bw() + geom_line(y=50) 
    gplot<- gplot + scale_x_datetime(labels=date_format("%H:%M",tz="Europe/Paris"), expand=c(0,0), breaks=date_breaks(width="1 hour")) 
    gplot
    ggsave("figs/FC_avgAcrossDay.pdf",gplot, height=4, width=12)
    

    # Both
    da <- cbind(as.data.frame(cbind(avFN, avFC)), index(avFC))
    names(da)<-c("FN", "FC", "index")
    da.melt<- melt(da, id.vars = "index")
      
    g <- ggplot(data=da.melt, aes(x=index, y=value, col=variable)) + 
      geom_line() + 
      geom_line(y=50, col="black") +
      ggtitle("Average Frequency") +
      xlab("Time") + 
      ylab("Frequency (Hz)") +
      theme_bw() +
      theme(legend.key = element_blank()) + #Remove box around legend
      scale_x_datetime(labels=date_format("%H:%M",tz="Europe/Paris"), expand=c(0,0), breaks=date_breaks(width="1 hour"))  + # Change x-axis
      scale_colour_discrete(name="Frequency series", breaks=c("FN","FC"), labels=c("Nordic","Continental")) + #Change labels
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) # Rotate x-axis
      
    g
    
    ggsave("figs/FCFN_avgAcrossDay.pdf",g, height=4, width=12)
    
    # To display functions in package: ls(pos = "package:packagename") and search() to get correct string

### F) Level plots ####
    
    # General helper functions
    fiveMin <- function(x){as.numeric(format(x, '%M'))%/%5}
    week <- function(x)as.numeric(format(x, '%W')) # Week of the year as decimal number (00-53) using Monday as the first day of week 
    weekday <- function(x)as.numeric(format(x, '%u')) # 1-7, 1=monday
    wday <- function(x){ #Sunday=3, Saturday=2, Other=1
      day<-as.numeric(format(x, '%u')) # 1-7, 1=monday
      if(day==7) 3
      else if (day==6) 2
      else 1
    }
    vWeekday<-Vectorize(wday)
    hour <- function(x)as.numeric(format(x, '%H'))
    month <- function(x)as.numeric(format(x, '%m'))
    
    # Theme
    myTheme <- modifyList(custom.theme(region=brewer.pal(9, 'RdBu')),
                          list(strip.background=list(col='gray'),panel.background=list(col='gray')))
    
    # Plotting function
    lvlPlotMonth <- function(x){
      maxZ <- max(abs(x), na.rm = T)
      
      lvlplot<-levelplot(x ~ hour(index(x)) * fiveMin(index(x)) | factor(month(index(x))),
                        at=pretty(c(-maxZ, maxZ), n=8),
                        colorkey=list(height=0.3),
                        layout=c(1, 9), strip=FALSE, strip.left=TRUE,
                        xlab='Hour', ylab='Month', 
                        par.settings=myTheme)
      lvlplot
    }
    
    lvlPlotWeekday <- function(x){
      maxZ <- max(abs(x), na.rm = T)
      
      lvlplot<-levelplot(x ~ hour(index(x)) * fiveMin(index(x)) | factor(weekday(index(x))),
                         at=pretty(c(-maxZ, maxZ), n=8),
                         colorkey=list(height=0.3),
                         layout=c(1, 9), strip=FALSE, strip.left=TRUE,
                         xlab='Hour', ylab='Month', 
                         par.settings=myTheme)
      lvlplot
    }
## Levelplot with Month ##
  ## Continental ##
  # Mean
    dat <- m5FC[,1]-50
    fileName <- "figs/FC_lvlMean.pdf"
    pdf(fileName, width=6, height=8)
    lvlPlotMonth(dat)
    dev.off()
    
  # Max
    dat <- m5FC[,2]-50
    fileName <- "figs/FC_lvlMax.pdf"
    pdf(fileName, width=6, height=8)
    lvlPlotMonth(dat)
    dev.off()
    
  # Min
    dat <- m5FC[,3]-50
    fileName <- "figs/FC_lvlMin.pdf"
    pdf(fileName, width=6, height=8)
    lvlPlotMonth(dat)
    dev.off()
    
    
  ## Nordic ##
    # Mean
    dat <- m5FN[,1]-50
    fileName <- "figs/FN_lvlMean.pdf"
    pdf(fileName, width=6, height=8)
    lvlPlotMonth(dat)
    dev.off()
    
    # Max
    dat <- m5FN[,2]-50
    fileName <- "figs/FN_lvlMax.pdf"
    pdf(fileName, width=6, height=8)
    lvlPlotMonth(dat)
    dev.off()
    
    # Min
    dat <- m5FN[,3]-50
    fileName <- "figs/FN_lvlMin.pdf"
    pdf(fileName, width=6, height=8)
    lvlPlotMonth(dat)
    dev.off()
    
## Levelplot with Weekday ##
    # Mean
    dat <- m5FN[,1]-50
    fileName <- "figs/FN_lvlMean-weekday.pdf"
    pdf(fileName, width=6, height=8)
    lvlPlotWeekday(dat)
    dev.off()
    
    # Max
    dat <- m5FN[,2]-50
    fileName <- "figs/FN_lvlMax-weekday.pdf"
    pdf(fileName, width=6, height=8)
    lvlPlotWeekday(dat)
    dev.off()
    
    # Min
    dat <- m5FN[,3]-50
    fileName <- "figs/FN_lvlMin-weekday.pdf"
    pdf(fileName, width=6, height=8)
    lvlPlotWeekday(dat)
    dev.off()
  