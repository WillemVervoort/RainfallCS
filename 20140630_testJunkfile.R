
dat <- read.csv(paste(td, "/web/htdocs/tmp/cdio/", fileName, 
                      sep = ""), as.is = TRUE)
unlink(paste(tmpdir, "/web", sep = ""), recursive = T)
unlink(paste(tmpdir, "/tmp.zip", sep = ""), recursive = T)

# thanks to: http://stackoverflow.com/questions/14426359/downloading-large-files-with-r-rcurl-efficiently
bdown=function(url, file){
  library('RCurl')
  f = CFILE(file, mode="wb")
  a = curlPerform(url = url, writedata = f@ref, noprogress=T)
  close(f)
  return(a)
}

## ...and now just give remote and local paths     
ret = bdown("http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_display_type=dailyZippedDataFile&p_stn_num=066062&p_c=-872892826&p_nccObsCode=136&p_startYear=2014", "C:\\Users\\rver4657\\Documents\\R\\test.zip")
unzip("C:\\Users\\rver4657\\Documents\\R\\test.zip", exdir = "C:\\Users\\rver4657\\Documents\\R")
