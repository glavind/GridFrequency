
################
### ANALYSIS ###
################
  #library(data.table)
  library(xts)
  library(lubridate) # convert date
  library(pryr) # report file size
  
  library(grid)
  library(latticeExtra)
  
  setwd("C:/Users/Glavind/git/GridFrequency")

## Read data
  # Read raw 1/10 second data
  FN <- readRDS("data/freq_nordic_processed.rds") # XTS
  FC <- readRDS("data/freq_continental_processed.rds") # XTS
  
  ## Processed to 1 min data
  mFC <- readRDS("data/mFC.rds") # XTS - mean, max, min, std.dev
  mFN <- readRDS("data/mFN.rds") # XTS - mean, max, min, std.dev
  
  ## Processed to 1 hour data
  hFC <- readRDS("data/hCN.rds") # XTS - mean, max, min, std.dev
  hFN <- readRDS("data/hFN.rds") # XTS - mean, max, min, std.dev
  
## Missing observations
# Continental:
  cat("Continental: missing", sum(is.na(FC[,1])), "observations (", sprintf("%.4f",sum(is.na(FC[,1]))/nrow(FC)*100),"%)")
# Nordic:
  cat("Nordic: missing", sum(is.na(FN[,1])), "observations (", sprintf("%.4f",sum(is.na(FN[,1]))/nrow(FN)*100),"%)")
  
## Basic plots
  dat<-FC["2015-01::2015-03"]
  xyplot(dat,
         superpose=TRUE, auto.key=FALSE,
         lwd=0.5, alpha=0.3, col="midnightblue")
  
## Frequency drift
  dFC <- apply.daily(FC, mean, na.rm=T)
  index(dFC)<-as.Date(index(dFC))
  dFC<-cbind(dFC,(dFC[,1]-50)*3600*24/50)
  
  miss <- is.na(dFC)
  x[miss] <- 0
  cs <- cumsum(x)
  cs[miss] <- NA
  
  dFC<-cbind(dFC,cumsum(dFC[,2]))
## Plot drift 
  plot(dFC[,2])
## Plot accumulated drift
  plot(dFC[,3])
  
## Create histogram
  dat <- coredata(mFC[,1])
  hist(dat, prob=TRUE, col="grey80", breaks = 60, main="hFC", prob=T)# prob=TRUE for probabilities not counts
  lines(density(dat, na.rm = T), col="midnightblue", lwd=2) 
  rug(dat) # Make a rug of observations underneath
  abline(v=49.90, lwd=2, col="red")
  abline(v=50.10, lwd=2, col="red")
##
  
## Create plot
dat <- coredata(hFC[,1])
plot(dat)
lines(density(dat, na.rm = T), col="midnightblue", lwd=2) 
##
  
  
  dFN <- apply.daily(FN, mean, na.rm=T)
  index(dFN)<-as.Date(index(dFN))
  dFN<-cbind(dFN,(dFN[,1]-50)*3600*24/50) # drift
  dFN<-cbind(dFN,cumsum(dFN[,2])) # cumm.drift
  names(dFN)<-c("frequency", "drift", "cumm.drift")
  plot(dFN[,"cumm.drift"])
  
  
