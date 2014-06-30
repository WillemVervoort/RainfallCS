# Jason Lessels BOM data ripper
# modifications Willem Vervoort June/July 2014
library('RCurl')

bdown=function(url, file){
  f = CFILE(file, mode="wb")
  a = curlPerform(url = url, writedata = f@ref, noprogress=T)
  close(f)
  return(a)
}


bomDailyObs <- function (siteNumber, observation = "temp",...) 
{
  if (class(observation) != "character") 
    stop("observation must be; temp, rain or solar.")
  observation <- tolower(observation)
  if (is.na(match(observation, c("rain", "min_temp","max_temp", "solar")))) 
    stop("observation must be; min_temp, max_temp, rain or solar.")
  if (is.numeric(siteNumber)) {
    theurl <- paste("http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_nccObsCode=DATATYPE&p_display_type=dailyDataFile&p_startYear=&p_c=&p_stn_num=0", 
                    siteNumber, sep = "")
  }  else {
    theurl <- paste("http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_nccObsCode=DATATYPE&p_display_type=dailyDataFile&p_startYear=&p_c=&p_stn_num=", 
                    siteNumber, sep = "")
  }
  if (observation == "rain") {
    theurl <- gsub("DATATYPE", "136", theurl)
    dataCode <- 136
  }
  if (observation == "max_temp") {
    theurl <- gsub("DATATYPE", "122", theurl)
    dataCode <- 122
  }
  if (observation == "min_temp") {
    theurl <- gsub("DATATYPE", "123", theurl)
    dataCode <- 123
  }
  if (observation == "solar") {
    theurl <- gsub("DATATYPE", "193", theurl)
    dataCode <- 193
  }
  raw <- getURLContent(theurl,...)
  if (grepl("Unfortunately there are no data available", raw[[1]])) {
    stop("Unfortunately there are no data available for the site number you have entered.\nThis may be because either the station number is invalid, or the station has\nnot observed the type of data requested.")
  }
  
  split1 <- strsplit(raw[[1]], "1 year of data</a></li><li><a")
  split2 <- strsplit(split1[[1]][[2]], "title=\"Data file for daily rainfall data for all years")
  split3 <- strsplit(split2[[1]][[1]], "p_c=")
  split4 <- strsplit(split3[[1]][[2]], "&amp;p_ncc")
  uniqueID <- split4[[1]][[1]]
  start_year <- substr(strsplit(raw[[1]], "startYear=")[[1]][2],1,4)
  theurl <- paste("http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_display_type=dailyZippedDataFile&p_stn_num=0", 
                  siteNumber, "&p_c=", uniqueID, "&p_nccObsCode=", dataCode, "&p_startYear=", start_year,
                sep = "")
  tmpfile <- tempfile()
  td <- tempdir()
  ret = bdown(theurl, tmpfile)
  unzip(tmpfile, exdir = td)
    
  fileName <- paste("IDCJAC0009_0", siteNumber,"_1800_Data.csv",sep="") 
  dat <- as.data.frame(read.csv(paste(td, fileName, sep = "\\"), as.is = TRUE))
#  unlink(td, recursive = T)
#  unlink(tmpfile, recursive = T)
  if (observation == "rain") {
    names(dat) <- c("productCode", "stationNumber", "year", 
                    "month", "day", "rainfall", "daysRainMeasuredOver", 
                    "quality")
  }
  if (observation == "max_temp") {
    names(dat) <- c("productCode", "stationNumber", "year", 
                    "month", "day", "maxTemp", "daysTempMeasuredOver", 
                    "quality")
  }
  if (observation == "min_temp") {
    names(dat) <- c("productCode", "stationNumber", "year", 
                    "month", "day", "minTemp", "daysTempMeasuredOver", 
                    "quality")
  }
  if (observation == "solar") {
    names(dat) <- c("productCode", "stationNumber", "year", 
                    "month", "day", "dailyExposure")
  }
  dat2  <- cbind(dat,as.Date(paste(dat$year, dat$month, dat$day,sep="-")))
  names(dat2) <- c(names(dat),"Date")
  return(dat2)
}