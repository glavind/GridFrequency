library(lubridate) # convert date
library(httr)
library(pryr) # report file size
#library(data.table)
library(xts)

setwd("C:/Users/Glavind/git/GridFrequency")
#Sys.setenv(TZ="Europe/Paris") # Optional: Set Global R timezone to continental

##########################################
### Continental frequency measurements ###
##########################################

  if(!file.exists("data/continental/zip")){dir.create("data/continental/zip", recursive = T)}
  if(!file.exists("data/continental/csv")){dir.create("data/continental/csv", recursive = T)}
  
  ## Download files from RTE. A backlog of one year is available.
  getYear <- 2015 # Set year
  for(i in 1:10){ # Set months
    getMonth <- i
    fileName <- paste0("RTE_Frequence_", getYear, "_", sprintf("%02d", getMonth))
    fileUrl<- paste0("http://clients.rte-france.com/lang/fr/include/download/Frequences/", fileName, ".zip")
    download.file(fileUrl, destfile = paste0("./data/continental/zip/", fileName, ".zip"))
  }
  dateDownloaded <- date()
  
  ## Unzip files
  zipFiles <- dir(path="data/continental/zip") # Find all files in directory
  zipFiles <- grep(zipFiles, pattern = ".zip", value = T) # restrict to zip files
  
  for (zipFile in zipFiles){ # Unzip all files
    unzip(zipfile=paste0("./data/continental/zip/", zipFile), exdir="data/continental/csv")
  }

###########################
### Collect in one file ###
###########################
  
  ## Read files
  xFC <- data.frame()
  csvFiles <- dir(path="data/continental/csv", full.names = T) # Find all files in directory
  csvFiles <- grep(csvFiles, pattern = ".csv", value = T) # restrict to csv
  csvFiles <- grep(csvFiles, pattern = "2015_0[0-9]", value = T) # restrict to month 1-9
  
  ## Combine csv-files in one df
  for (csvFile in csvFiles){
    f <- readLines(csvFile) # Find number of lines
    f.read <- read.table(file=csvFile, na.strings=c("N/A"), sep = ";", dec = ",", header = F, skip = 2, nrows = length(f)-3) #Exclude first two and last line
    f <- NULL; f.read$V3<- NULL #Drop orphan column
    
    names(f.read)<-c("date","frequency")
    f.read$date <- as.POSIXct(f.read$date, format="%d/%m/%Y %H:%M:%S", tz="Europe/Paris") # convert to date
    xFC <- rbind(xFC, f.read)
  }
  
  freq_continental <- xts(xFC[,2],xFC[,1]) # Save as xts
  names(freq_continental) <- c("frequency")
  attr(freq_continental,"downloaded.on") <- file.mtime(csvFiles[1]) # Save download time
  
  saveRDS(freq_continental,"data/freq_continental_raw.rds") # Save
  
  rm(xFC, freq_continental, csvFile, f, csvFiles, f.read)
  
#####################################
### Nordic frequency measurements ###
#####################################
  
  if(!file.exists("data/nordic")){dir.create("data/nordic",recursive = T)}
  
  freq_nordic <- data.frame() # Start with empty data frame
  #freq_nordic <- readRDS("freq_nordic.rds") # Start with loaded data frame

  ## Get from/to as hour, both dates are included
  fromUTC <- dmy_hms("31-12-2014 00:00:00")
  toUTC <- dmy_hms("31-12-2014 23:00:00")
  
  date.start <- Sys.time() # Save starttime
  
  ## Main loop
  for (loopHour in seq(fromUTC, toUTC, by=3600)){ # Loop over each hour
    timer<- Sys.time()
    #loopHour <- dmy_hms("07-01-2015 15:00:00")
    
    restURL <- paste0("http://driftsdata.statnett.no/restapi/Frequency/BySecond?",
                      "FromInTicks=", formatC(as.numeric(loopHour) * 1000, format="fg"),
                      "&ToInTicks=", formatC((as.numeric(loopHour) + 3600) * 1000, format="fg"))
    snData <- GET(restURL) # Get data
    parsedData <- content(snData, "parsed") # parse to list
    #str(parsedData)
    
    if(length(parsedData$Measurements)==3600){ # If all observations present in hour, write result
      startPointLocal <- strptime(parsedData$StartPointUTC/1000, '%s', tz="Europe/Paris")
      endPointLocal <- strptime(parsedData$EndPointUTC/1000, '%s', tz="Europe/Paris")
      result <- data.frame("date"=as.POSIXct(seq(loopHour, loopHour+3599, by=1),origin = "1970-01-01",tz="UTC"),
                           "frequency"=unlist(parsedData$Measurements))
      freq_nordic <- rbind(freq_nordic,result)
      
    } else { # Loop across each minute in hour
      #warning("missing observations in hour ", as.POSIXct(loopHour, origin = "1970-01-01",tz="UTC"))
      for (loopMinute in seq(loopHour, loopHour + 3600, by=60)){ # Loop over each minute
        #loopMinute <- dmy_hms("07-01-2015 15:33:00")

        restURL <- paste0("http://driftsdata.statnett.no/restapi/Frequency/BySecond?",
                          "FromInTicks=", formatC(as.numeric(loopMinute) * 1000, format="fg"),
                          "&ToInTicks=", formatC((as.numeric(loopMinute) + 60) * 1000, format="fg"))
        snData <- GET(restURL) # Get data
        parsedData <- content(snData, "parsed") # parse to list
        #str(parsedData)
        
        if(length(parsedData$Measurements)==60){ # If all observations are present in minute, write results
          startPointLocal <- strptime(parsedData$StartPointUTC/1000, '%s', tz="Europe/Paris")
          endPointLocal <- strptime(parsedData$EndPointUTC/1000, '%s', tz="Europe/Paris")
          result <- data.frame("date"=as.POSIXct(seq(loopMinute, loopMinute+59, by=1),origin = "1970-01-01",tz="UTC"),
                               "frequency"=unlist(parsedData$Measurements))
          freq_nordic <- rbind(freq_nordic,result)
        } else { # loop across seconds in minute
          warning("missing observations in minute: ", as.POSIXct(loopMinute, origin = "1970-01-01",tz="UTC"))
          
          for (loopSecond in seq(loopMinute, loopMinute + 60, by=1)){ # Loop over each second
            #loopSecond <- dmy_hms("07-01-2015 15:30:40")
            
            restURL <- paste0("http://driftsdata.statnett.no/restapi/Frequency/BySecond?",
                              "FromInTicks=", formatC(as.numeric(loopSecond) * 1000, format="fg"),
                              "&ToInTicks=", formatC(as.numeric(loopSecond + 1) * 1000, format="fg")) 
            snData <- GET(restURL) # Get data
            parsedData <- content(snData, "parsed") # parse to list
            #str(parsedData)
            
            if(length(parsedData$Measurements)==1){ # Observation present in second, write result
              startPointLocal <- strptime(parsedData$StartPointUTC/1000, '%s', tz="Europe/Paris")
              endPointLocal <- strptime(parsedData$EndPointUTC/1000, '%s', tz="Europe/Paris")
              result <- data.frame("date"=as.POSIXct(loopSecond, origin = "1970-01-01", tz="UTC"),
                                   "frequency"=unlist(parsedData$Measurements))
            } else { # If no observation then write NA
              #warning("missing observation in second: ",as.POSIXct(loopSecond, origin = "1970-01-01",tz="UTC"))
              result <- data.frame("date"=as.POSIXct(loopSecond, origin = "1970-01-01", tz="UTC"),
                                   "frequency"=NA)
            }
            freq_nordic <- rbind(freq_nordic, result)
          }
        }
      }
    }
    cat("\n", "Hour: ", format(as.POSIXct(loopHour, origin = "1970-01-01",tz="UTC"),"%Y-%m-%d %H:%M:%S")," (", as.numeric(Sys.time() - timer, units='secs'), " seconds)",sep="")
  }
  cat("\n", "Total Runtime: ", as.numeric(Sys.time() - date.start, units='mins'), " minutes",sep="")
  
  ## Diagnostics
  head(freq_nordic)
  tail(freq_nordic)
  object_size(freq_nordic) # Report size
  
  ## Save
  saveRDS(freq_nordic, "data/nordic/freq_nordic_xxx.rds") # Save
  
###########################
### Collect in one file ###
###########################
  ## Read all months that where previously saved to disc   
  r0<-unique(readRDS("data/nordic/freq_nordic_2014-12-31.rds"))
  r1<-unique(readRDS("data/nordic/freq_nordic_2015-1.rds"))
  r2<-unique(readRDS("data/nordic/freq_nordic_2015-2.rds"))
  r3<-unique(readRDS("data/nordic/freq_nordic_2015-3.rds"))
  r4<-unique(readRDS("data/nordic/freq_nordic_2015-4.rds"))
  r5<-unique(readRDS("data/nordic/freq_nordic_2015-5.rds"))
  r6<-unique(readRDS("data/nordic/freq_nordic_2015-6.rds"))
  r7<-unique(readRDS("data/nordic/freq_nordic_2015-7.rds"))
  r8<-unique(readRDS("data/nordic/freq_nordic_2015-8.rds"))
  r9<-unique(readRDS("data/nordic/freq_nordic_2015-9.rds"))
   
  r <- rbind(r0, r1,r2,r3,r4,r5,r6,r7,r8,r9) # collect in df
  rm(r0, r1,r2,r3,r4,r5,r6,r7,r8,r9)
  
  xFN <- xts(r[,2], r[,1], tz="Europe/Paris") # collect in XTS
  names(xFN) <- c("frequency")
  saveRDS(xFN, "data/freq_nordic_raw.rds") # Save unprocessed XTS to disc
  rm(r, xFN)
  
####################
### Process Data ###
####################
  
## Load Nordic data, subset and validate
  FN <- readRDS("data/freq_nordic_raw.rds") # Load raw observations file
  FN <- FN['2015-01-01::2015-09-30 23:59:59'] # Subset
  # Check number of observations are correct
  FNDates<-seq(ymd("2015-01-01",tz="Europe/Paris"), ymd_hms("2015-09-30 23:59:59",tz="Europe/Paris"), by = 1)
  assertthat::are_equal(nrow(FN),length(FNDates))  # Check length
  
  saveRDS(FN, "data/freq_nordic_processed.rds") # Save processed XTS to disc
  rm(FN, FNDates)
  
## Load Continental data, subset and validate
  FC <- readRDS("data/freq_continental_raw.rds") # Load
  # Print number of observations with NA in index, and then remove them
  cat("Removing", sum(is.na(index(FC))), "observations with missing index") 
  FC <- FC[!is.na(index(FC))] # Remove observations with NA in index, these stem from DST.
  # Subset
  FC <- FC['2015-01-01::2015-09-30 23:59:59']
  
  ## Handle missing observations
  FCDates<-seq(ymd("2015-01-01",tz="Europe/Paris"), ymd_hms("2015-09-30 23:59:50",tz="Europe/Paris"), by = 10)
  FCIndex<-xts(NULL,order.by = FCDates) #create empty xts with all dates
  #missObs <- FCDates[!(FCDates %in% index(FC))] # Find missing observations
  
  FC<-cbind(FCIndex,FC) # Populate with observations
  assertthat::are_equal(nrow(FC),length(FCDates)) # Check length
  
  saveRDS(FC, "data/freq_continental_processed.rds") # Save processed XTS to disc
  rm(FC, FCDates, FCIndex)
  
###########################  
### Aggregate to minute ###
###########################
  ## Read data
  # Read raw 1/10 second data
  FN <- readRDS("data/freq_nordic_processed.rds") # XTS
  FC <- readRDS("data/freq_continental_processed.rds") # XTS
  
  my.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
  my.min <- function(x) ifelse( !all(is.na(x)), min(x, na.rm=T), NA)
  
  #############################
  ## Processed to 1 min data ##
  #############################
  # Continental:
  V1 <- aggregate(FC, cut(index(FC), breaks="min"), mean, na.rm = TRUE) # Mean, digress to zoo
  V2 <- aggregate(FC, cut(index(FC), breaks="min"), my.max) # Max
  V3 <- aggregate(FC, cut(index(FC), breaks="min"), my.min) # Min
  V4 <- aggregate(FC, cut(index(FC), breaks="min"), sd, na.rm = TRUE) # Standard Deviation
  mFC <- xts(x=cbind(coredata(V1),coredata(V2),coredata(V3),coredata(V4)),
             order.by=as.POSIXct(index(V1),tz="Europe/Paris"))
  colnames(mFC)<-c("mean", "max", "min","std.dev")
  summary(mFC)
  rm(V1,V2,V3,V4)
  saveRDS(mFC, "data/mFC.rds") # Save
  #mFC <- readRDS("data/mFC.rds") # Load
  
  # Nordic:
  V1 <- aggregate(FN, cut(index(FN), breaks="min"), mean, na.rm = TRUE) # Mean
  V2 <- aggregate(FN, cut(index(FN), breaks="min"), my.max) # Max
  V3 <- aggregate(FN, cut(index(FN), breaks="min"), my.min) # Min
  V4 <- aggregate(FN, cut(index(FN), breaks="min"), sd, na.rm = TRUE) # Standard Deviation
  mFN <- xts(x=cbind(coredata(V1),coredata(V2),coredata(V3),coredata(V4)),
             order.by=as.POSIXct(index(V1),tz="Europe/Paris"))
  colnames(mFN)<-c("mean", "max", "min","std.dev")
  summary(mFN)
  rm(V1,V2,V3,V4)
  saveRDS(mFN, "data/mFN.rds") # Save
  #mFN <- readRDS("data/mFN.rds") # Load
  
  rm(mFC,mFN)
  
  #############################
  ## Processed to 5 min data ##
  #############################
  # Continental:
  V1 <- period.apply(FC, endpoints(FC, "minutes", 5), mean, na.rm=T)
  V2 <- period.apply(FC, endpoints(FC, "minutes", 5), my.max)
  V3 <- period.apply(FC, endpoints(FC, "minutes", 5), my.min)
  V4 <- period.apply(FC, endpoints(FC, "minutes", 5), sd, na.rm=T)
  m5FC <- xts(x=cbind(coredata(V1),coredata(V2),coredata(V3),coredata(V4)),
             order.by=as.POSIXct(index(V1),tz="Europe/Paris"))
  
  colnames(m5FC)<-c("mean", "max", "min","std.dev")
  summary(m5FC)
  index(m5FC)<- index(m5FC) - (60*4+50) #Subtract 00:04:50 to get start instead of end at index
  rm(V1,V2,V3,V4)
  saveRDS(m5FC, "data/m5FC.rds") # Save
  #m5FC <- readRDS("data/m5FC.rds") # Load
  
  # Nordic:
  V1 <- period.apply(FN, endpoints(FN, "minutes", 5), mean, na.rm=T)
  V2 <- period.apply(FN, endpoints(FN, "minutes", 5), my.max)
  V3 <- period.apply(FN, endpoints(FN, "minutes", 5), my.min)
  V4 <- period.apply(FN, endpoints(FN, "minutes", 5), sd, na.rm=T)
  m5FN <- xts(x=cbind(coredata(V1),coredata(V2),coredata(V3),coredata(V4)),
             order.by=as.POSIXct(index(V1),tz="Europe/Paris"))
  
  colnames(m5FN)<-c("mean", "max", "min","std.dev")
  index(m5FN)<- index(m5FN) - (60*4+59) #Subtract 00:04:59 to get start instead of end at index
  summary(m5FN)
  rm(V1,V2,V3,V4)
  saveRDS(m5FN, "data/m5FN.rds") # Save
  #m5FN <- readRDS("data/m5FN.rds") # Load
  
  rm(m5FC,m5FN)
  
  ##############################
  ## Processed to 1 hour data ##
  ##############################
  # Continental:
  V1 <- aggregate(FC, cut(index(FC), breaks="hour"), mean, na.rm = TRUE) # Mean, digress to zoo
  V2 <- aggregate(FC, cut(index(FC), breaks="hour"), my.max) # Max
  V3 <- aggregate(FC, cut(index(FC), breaks="hour"), my.min) # Min
  V4 <- aggregate(FC, cut(index(FC), breaks="hour"), sd, na.rm = TRUE) # Standard Deviation
  hFC <- xts(x=cbind(coredata(V1),coredata(V2),coredata(V3),coredata(V4)),
             order.by=as.POSIXct(index(V1),tz="Europe/Paris"))
  colnames(hFC)<-c("mean", "max", "min","std.dev")
  summary(hFC)
  rm(V1,V2,V3,V4)
  saveRDS(hFC, "data/hFC.rds") # Save
  #hFC <- readRDS("data/hFC.rds") # Load
  
  # Nordic:
  V1 <- aggregate(FN, cut(index(FN), breaks="hour"), mean, na.rm = TRUE) # Mean
  V2 <- aggregate(FN, cut(index(FN), breaks="hour"), my.max) # Max
  V3 <- aggregate(FN, cut(index(FN), breaks="hour"), my.min) # Min
  V4 <- aggregate(FN, cut(index(FN), breaks="hour"), sd, na.rm = TRUE) # Standard Deviation
  hFN <- xts(x=cbind(coredata(V1),coredata(V2),coredata(V3),coredata(V4)),
             order.by=as.POSIXct(index(V1),tz="Europe/Paris"))
  colnames(hFN)<-c("mean", "max", "min","std.dev")
  summary(hFN)
  rm(V1,V2,V3,V4)
  saveRDS(hFN, "data/hFN.rds") # Save
  #hFN <- readRDS("data/hFN.rds") # Load
  
  rm(hFC,hFN)
  rm(my.max, my.min)
  rm(FC, FN)