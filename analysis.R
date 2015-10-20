library(data.table)
library(lubridate) # convert date
library(pryr) # report file size

setwd("C:/Users/Glavind/git/GridFrequency")

dtFN <- readRDS("freq_nordic_dt_1-9.rds")
dfFC <- readRDS("continental.rds") # Load
dfFC <- dfFC[dfFC[,1]>=dmy_hms("1-1-2015 00:00:00") & dfFC[,1]<dmy_hms("1-10-2015 00:00:00"),]

################
### ANALYSIS ###
################