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
  
  fromUTC <- dmy_hms("07-01-2015 15:00:00")
  toUTC <- dmy_hms("07-01-2015 16:00:00")
  
  freq_nordic <- data.frame()
  date.start <- Sys.time()
  
  for (loopHour in seq(fromUTC, toUTC, by=3600)){ # Loop over each hour
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
          #warning("missing observations in minute: ", as.POSIXct(loopMinute, origin = "1970-01-01",tz="UTC"))
          
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
              result <- data.frame("date"=as.POSIXct(seq(loopSecond, loopSecond, by=1),origin = "1970-01-01",tz="UTC"),
                                   "frequency"=unlist(parsedData$Measurements))
            } else { # If no observation then write NA
              warning("missing observation in second: ",as.POSIXct(loopSecond, origin = "1970-01-01",tz="UTC"))
              result <- data.frame("date"=as.POSIXct(seq(loopSecond, loopSecond, by=1),origin = "1970-01-01",tz="UTC"),
                                   "frequency"=NA)
            }
            freq_nordic <- rbind(freq_nordic, result)
          }
        }
      }
    }
  }
  print("Runtime"); Sys.time() - date.start
  
  str(freq_nordic)
  object_size(freq_nordic) # Report size
  saveRDS(freq_nordic, "freq_nordic.rds") # Save
  #freq_nordic <- readRDS("freq_nordic.rds") # Load
  
  aDate <- parsedData$StartPointUTC/1000
  aDate <- parsedData$EndPointUTC/1000
  as.POSIXct(aDate, origin = "1970-01-01",tz="UTC")
  