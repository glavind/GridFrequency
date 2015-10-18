library(lubridate) # convert date
library(pryr) # report file size

setwd("C:/Users/Glavind/git/GridFrequency")

## Nordic frequency measurements
  if(!file.exists("data/nordic")){dir.create("data/nordic",recursive = T)}

## Continental frequency measurements
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
  
  ## Unzip files
  zipFiles <- dir(path="data/continental/zip") # Find all files in directory
  zipFiles <- grep(zipFiles, pattern = ".zip", value = T) # restrict to zip files
  
  for (zipFile in zipFiles){ # Unzip all files
    unzip(zipfile=paste0("./data/continental/zip/", zipFile), exdir="data/continental/csv")
  }

  ## Read files
  freq <- data.frame()
  csvFiles <- dir(path="data/continental/csv",full.names = T) # Find all files in directory
  csvFiles <- grep(csvFiles, pattern = ".csv", value = T) # restrict to csv
  
  for (csvFile in csvFiles){
    f <- readLines(csvFile)
    f.read <- read.table(file=csvFile, sep = ";", dec = ",", header = T, skip = 1, nrows = length(f)-3); f <- NULL
    names(f.read)<-c("date","frequency","X")
    
    f.read$X<- NULL #Drop orphan column
    f.read$date <- dmy_hms(f.read$date) # convert to date
    freq <- rbind(freq, f.read)
  }
  
  
  head(freq)
  tail(freq) 
  object_size(freq) # Report size
  saveRDS(freq,"continental.rds") # Save
  #freq <- readRDS("continental.rds") # Load
  