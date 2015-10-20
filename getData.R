library(lubridate) # convert date
library(pryr) # report file size

setwd("C:/Users/Glavind/git/GridFrequency")

##########################################
### Continental frequency measurements ###
##########################################

  if(!file.exists("data/continental/zip")){dir.create("data/continental/zip",recursive = T)}
  if(!file.exists("data/continental/csv")){dir.create("data/continental/csv",recursive = T)}
  
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

  ## Read files
  freq <- data.frame()
  csvFiles <- dir(path="data/continental/csv", full.names = T) # Find all files in directory
  csvFiles <- grep(csvFiles, pattern = ".csv", value = T) # restrict to csv
  
  ## Combine files in dataframe
  for (csvFile in csvFiles){
    f <- readLines(csvFile)
    f.read <- read.table(file=csvFile, sep = ";", dec = ",", header = T, skip = 1, nrows = length(f)-3); f <- NULL
    names(f.read)<-c("date","frequency","X")
    
    f.read$X<- NULL #Drop orphan column
    f.read$date <- dmy_hms(f.read$date) # convert to date
    freq <- rbind(freq, f.read)
  }
  
  attr(freq,"downloaded.on") <- dateDownloaded
  
  head(freq)
  tail(freq) 
  object_size(freq) # Report size
  saveRDS(freq,"continental.rds") # Save
  #freq <- readRDS("continental.rds") # Load
  
#####################################
### Nordic frequency measurements ###
#####################################
  library(httr)
  library(lubridate)
  library(pryr) # report file size
  if(!file.exists("data/nordic")){dir.create("data/nordic",recursive = T)}
  
  
  freq_nordic <- data.frame()
  #freq_nordic <- readRDS("freq_nordic.rds") # Load

  fromUTC <- dmy_hms("01-01-2015 00:00:00")
  toUTC <- dmy_hms("01-10-2015 20:00:00")
  
  date.start <- Sys.time()
  
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
        #loopMinute <- dmy_hms("07-01-2015 15:30:00")
        
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
  
  
  head(freq_nordic)
  tail(freq_nordic)
  object_size(freq_nordic) # Report size
  saveRDS(freq_nordic, "freq_nordic_2015-9.rds") # Save
  #freq_nordic <- readRDS("freq_nordic.rds") # Load
  #test<-freq_nordic[freq_nordic[,1]>=dmy("01-08-2015")&freq_nordic[,1]<dmy("01-09-2015"),]
  
#   aDate <- parsedData$StartPointUTC/1000
#   aDate <- parsedData$EndPointUTC/1000
#   as.POSIXct(aDate, origin = "1970-01-01",tz="UTC")
  
  r1<-unique(readRDS("freq_nordic_2015-1.rds"))
  r2<-unique(readRDS("freq_nordic_2015-2.rds"))
  r3<-unique(readRDS("freq_nordic_2015-3.rds"))
  r4<-unique(readRDS("freq_nordic_2015-4.rds"))
  r5<-unique(readRDS("freq_nordic_2015-5.rds"))
  r6<-unique(readRDS("freq_nordic_2015-6.rds"))
  r7<-unique(readRDS("freq_nordic_2015-7.rds"))
  r8<-unique(readRDS("freq_nordic_2015-8.rds"))
  r9<-unique(readRDS("freq_nordic_2015-9.rds"))
  
  r <- rbind(r1,r2,r3,r4,r5,r6,r7,r8,r9)
  saveRDS(r, "freq_nordic.rds") # Save
  
  ## Test if number of observations are correct:
#   d <- difftime(dmy_hms("1-10-2015 00:00:00"), dmy_hms("1-1-2015 00:00:00"), units="sec")
#   d
#   nrow(r)
#   f<-nrow(r)-as.integer(d)
#   f
#   f/3600/24
  mem_used()
  
  library(data.table)
  dt<-data.table(r)
  saveRDS(dt,"freq_nordic_dt_1-9.rds")
  
################
### ANALYSIS ###
################