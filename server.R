library(shiny)
library(RODBC)
library(oz)
# for fancier plotting
library(ggplot2)

# read in ozdata
oz.map <- read.csv("ozdata.csv")

# # connect to data source
# db <- odbcConnect("testwillem", uid="rver4657", pwd="7564revrMySQL")
# 
# # # testing ODBC
# # # find the names of the available tables
# sqlTables(db)
# #
## Drop the different tables
# sqlDrop(db,"main_table")
# sqlDrop(db,"regr_results")
# sqlDrop(db,"regr_stats")
# odbcClose(db)

## Clear the different tables
#  sqlClear(db,"main_table")
#  sqlClear(db,"regr_results")
#  sqlClear(db,"regr_stats")
#  odbcClose(db)

# # load the different tables into a data fram
# df_main <- sqlQuery(db, "select * from main_table",as.is = c(1,2,6,7),
#                     stringsAsFactors=F)
# df_regr_results <- sqlQuery(db, "select * from regr_results")
# df_regr_stats <- sqlQuery(db, "select * from regr_stats")

# here code that runs when app is launched
# This is some old code just to get the BOM stations into a Rdata file
# stations <- read.fwf("20140617_AllBOMstations.txt", 
#                      widths=c(7,6,43,7,7,9,10,16,4,10,9,7),
#                     skip = 3,
#               na.strings = c("..",".....","null"))
# colnames(stations) <-c("Site","Dist","Site_name","Start","End","Lat",
#                        "Lon","Source","STA","Height_m","Bar_ht","WMO")
# save(stations, file="Stations.Rdata")
# this could be moved to a helper script
load("stations.Rdata")
# Source the dataripper script to get the data from the BOM site
# This is adaption of Jason Lessels' bomDailyDataripper
source("dataripper.r")
# # this is using the multiplot function from the R cookbook
# source("multiplot.r")

plot.fun <- function(Dates = DateInput(),
                     Data=StationInput(),
                     d.type=input$type) {
  
  # define labels
  if (d.type=="rain") lab <- "Rainfall"; plot.t <- "h"
  if (d.type=="min_temp") lab <- "Minimum Temperature"; plot.t <- "l"
  if (d.type=="max_temp") lab <- "Maximum Temperature"; plot.t <- "l"
  
  # find begin and end dates from info
  time1 <- match(as.Date(Dates$Startdate),as.Date(Data$Date))
  time2 <- match(as.Date(Dates$Enddate),as.Date(Data$Date))

  plot.df <- data.frame(Dates=Data$Date[time1:time2],
                        values=Data[time1:time2,6])
  # make the plot
t.plot <-  ggplot(plot.df,aes(x=Dates,y=values)) +
         geom_line(colour="blue") +
        geom_smooth(method="lm", formula=y~x, colour="red",
                                       linetype=2, size=2) +
  xlab("Dates") + ylab(lab) + 
  ggtitle(paste(Data$stationNumber[1], "for", as.Date(Dates$Startdate), "to",
                as.Date(Dates$Enddate)))
#         geom_line(x = Data$Date[time1:time2],Dates$lm.line, colour="blue",
#                   linetype=2, size=2)
print(t.plot)
#   plot(Data$Date[time1:time2],
#        Data[time1:time2,6],
#        type=plot.t,col="blue",
#        xlab= "Date", ylab=lab)
#   lines(Data$Date[time1:time2],Dates$lm.line, col="red",lty=2,lwd=3)
}

hist.fun <- function(dat) {
#    plot(dat$slope)
#     plot.df <- dat
    th <- hist(dat$slope, plot=F)  
    nplots <- 1
    dr <- dat[dat$data_type=="rain",]
    if (nrow(dr) > 0 ) {
       rh <- hist(dr$slope, plot=F)  
    nplots <- nplots + 1
    }
    dmt <- dat[dat$data_type=="max_temp",]
    if (nrow(dmt) > 0 ) {
      mth <- hist(dmt$slope, plot=F)  
      nplots <- nplots + 1
    }
    dmit <- dat[dat$data_type=="min_temp",]
    if (nrow(dmit) > 0 ) {
      mith <- hist(dmit$slope, plot=F)  
      nplots <- nplots + 1
    }
    # depending on number of plots split screen
    par(mfrow=c(nplots,1))
    plot(th, xlab ="significant slopes with p-value < 0.05", 
         ylab = "slope or trend value",
         main="slopes of all Analyses")
    if (nrow(dr) > 0) {
      plot(rh, xlab ="significant slopes with p-value < 0.05", 
           ylab = "Rainfall slope (mm/day)",
           main="slopes of Rainfall")
    }
    if (nrow(dmt) > 0) {
      plot(mth, xlab ="significant slopes with p-value < 0.05", 
           ylab = "Max T slope (deg C/day)",
           main="slopes of Maximum Temperature")
    }
    if (nrow(dmit) > 0) {
      plot(mith, xlab ="significant slopes with p-value < 0.05", 
           ylab = "Min T slope (deg C/day)",
           main="slopes of Minimum Temperature")
    }
    #    p1 <- ggplot(dat, aes(x=dat$slope)) +
#        geom_histogram() +
#      xlab("significant slopes with p-value < 0.05") +
#      ylab("slope or trend value")
#   p1
#   dr <- dat[dat$data_type=="rain",]
#   if (nrow(dr) > 0 ) {
#     p2 <- ggplot(dr, aes(x=dr$slope)) +
#       geom_histogram() +
#       xlab("significant slopes with p-value < 0.05") +
#       ylab("Rainfall slope (mm/day)")
#   }
#   dmt <- dat[dat$data_type=="max_temp",]
#   if(nrow(dmt) > 0) {
#     p3 <- ggplot(dmt, aes(x=dmt$slope)) +
#       geom_histogram() +
#       xlab("significant slopes with p-value < 0.05") +
#       ylab("Max Temperature slope (degrees C/day)")
#   }
#   dmit <- dat[dat$data_type=="min_temp",]
#   if (nrow(dmit)>0) {
#     p4 <- ggplot(dmit, aes(x=dmit$slope)) +
#       geom_histogram() +
#       xlab("significant slopes with p-value < 0.05") +
#       ylab("Min Temperature slope (degrees C/day)")
#  }
#   if (exists("p2") & exists("p3") & exists("p4")) multiplot(p1,p2,p3,p4)
#   if (exists("p2") & exists("p3") & !exists("p4")) multiplot(p1,p2,p3)
#   if (exists("p2") & !exists("p4") & !exists("p3")) multiplot(p1,p2)
#   if (!exists("p4") & !exists("p3") & !exists("p2")) p1
#   if (!exists("p2") & exists("p3") & exists("p4")) multiplot(p1,p3,p4)
#   if (!exists("p3") & exists("p2") & exists("p4")) multiplot(p1,p2,p4)
  
}

# This is the start of the server part
# this gives the input and output
shinyServer(function(input, output, session) {
  
  # here code that runs every time the app is visited

  # create storage lists that are reactive
  data <- reactiveValues()
  name <- reactiveValues()
  dates <- reactiveValues()
#  dat <- reactiveValues()
# find the station in the 
  StationOut <- reactive({
    # find the station name in the station data set, now includes finding the state
      name$name <- stations[grep(input$Station,stations$Site_name,
                                 ignore.case=T),c("Site","Site_name","STA","Lat","Lon")]
     # This selects at least a station in the right state, but what to do if I have more than 1 station?
      name$name2 <- name$name[grep(input$state,name$name$STA),]
   # return(name$name2$Site)
  })

  output$choice <- renderUI({
      selectInput('choice', label='Choose a Station',
                  selected=StationOut()$Site[1], StationOut()$Site)
  })

StationInput <- reactive({
      if (input$goButton == 0) 
        return()
      # use dataripper to download data from BOM station
      isolate(
             data <- bomDailyObs(input$choice,observation=input$type))
             
       return(data)
      })
  
DateInput <- reactive({
   if (input$goButton == 0)  
    return()
   # this needs to read the dates and store them somewhere
   begin <- input$dateRange[1]
   end <- input$dateRange[2]
   isolate(
   if (is.character(StationInput())==T) {stop("no data available")})
   isolate(
   if (begin >= min(StationInput()$Date)) {
     dates$Startdate <- begin
     dates$StartMsg <- paste("data analysis will start from",begin)
   } else {
     dates$Startdate <- min(StationInput()$Date) #data$Date)
     dates$StartMsg <- paste("the data only starts at", min(StationInput()$Date))
   })
   
   isolate({
   if (end <= max(StationInput()$Date)) {
     dates$Enddate <- end
     dates$EndMsg <- paste("and the data ends at",end)
   } else {
     dates$Enddate <- max(StationInput()$Date) #max(data$Date)
     dates$EndMsg <- paste("and the data only runs till", max(StationInput()$Date))
   }
   # define the begin and end rows to plot
   temp <- match(as.Date(dates$Startdate),as.Date(StationInput()$Date))
   temp2 <- match(as.Date(dates$Enddate),as.Date(StationInput()$Date))

   ####################################################################
   # Now run the regression using lm and store the data into a database
   # need to work out what database (huge? or just small)
   # current thinking is: just make a database locally, move later
   # Probably can use AERDM from the Uni and therefore use MySQL
   
   
   # define the regression data frame
   df <- data.frame(time=1:nrow(StationInput()[temp:temp2,]), 
                    response=StationInput()[temp:temp2,6])
   # run the regression (start simple with just linear)
   lm.mod <- lm(response~time,df)
   
   # insert results into tables from database
   #splitTime <- strsplit(as.character(Sys.time())," ")
   #df_main[(nrow(df_main)+1),] <- 
   
   # Find latitude and longitude for each station
   Lat <- StationOut()$Lat[grep(input$choice,StationOut()$Site)]
   Lon <- StationOut()$Lon[grep(input$choice,StationOut()$Site)]
   
   # Create input line for database  
   line <- data.frame(station_id = input$choice,
                    lat = Lat,
                    lon = Lon,
                    data_type = input$type,
                    timestamp = Sys.time(),
                    start_date = as.character(dates$Startdate),
                    end_date = as.character(dates$Enddate),
                    comment = "testing")
   # append database
   db <- odbcConnect("testwillem", uid="rver4657", pwd="7564revrMySQL")
   try(sqlSave(db, line, tablename="main_table", 
           rownames=FALSE, append=T))
#   odbcClose(db)
#    # coefficients and stats
    mod.res <-  summary(lm.mod)$coefficients   
    results <- data.frame(station_id = input$choice,intercept = mod.res[1,1],
            se_int = mod.res[1,2], p_value_int = mod.res[1,4],
            slope = mod.res[2,1], se_slope = mod.res[2,2],
            p_value_slope = mod.res[2,4], comment="test",
            data_type=input$type)
  # append database
  try(sqlSave(db, results, tablename="regr_results", 
            rownames=FALSE, append=T))
# model statistics and summary
    mod.sum <- summary(lm.mod)
    fstat<-mod.sum$fstatistic
    pv <-   pf(fstat[1], fstat[2], fstat[3], lower.tail=FALSE) 
    bias <- sum(residuals(lm.mod),na.rm=T)
    stats <- data.frame(station_id = input$choice, rmse = mod.sum$sigma, 
                      r_sq = mod.sum$adj.r.squared, p_value = pv, 
                      bias = bias, stat1 = mod.sum$r.squared, 
                      stat2 = fstat[1], stat3 = fstat[2], 
                      stat4 = fstat[3], stat5 = NA, comment = "test",
                      data_type=input$type)
    # append database
    try(sqlSave(db, stats, tablename="regr_stats", 
            rownames=FALSE, append=T))
    
    #bad <-   
   # predict the regression line
#    lm.line <- predict(lm.mod,
#             new.data=data.frame(time=1:nrow(StationInput()[temp:temp2,]),
#                                           response=rep(0,nrow(df))))
#    dates$lm.line <- vector(length=nrow(df))
#    dates$lm.line[as.numeric(names(lm.line))] <- lm.line
   dates$lm.mod <- lm.mod
    }) # close isolate()
   # in here we need to write the output of the regression to the database
   return(dates)
 })  




  
  # ------------------------------------
  # Start of output creation
  #
  # and create a plot
 output$plot <- renderPlot({
    if (input$goButton == 0)
      return() 
    # grab the data from StationInput
    #data.plot <- StationInput()
    # only run when submit is pushed??
    isolate(plot.fun(Dates=DateInput(),
                     Data=StationInput(),
                     d.type=input$type)
    )
    
    })
output$map <- renderPlot({
  if (input$goButton == 0)
    return() 
  # 
  # only run when submit is pushed??
  isolate({
    p <- ggplot(subset(oz.map, border=="coast"), aes(long, lat, fill=state))
    p <- p + geom_path()
    p <- p + coord_equal()
    p <- p + ggtitle("Coastline of Australia")
    p
  })
})

extractData <- reactive({
  if (input$goButton == 0)
    return() 
    db <- odbcConnect("testwillem", uid="rver4657", pwd="7564revrMySQL")
    test <- sqlQuery(db,paste("SELECT station_id, slope, p_value_slope, data_type FROM regr_results"))
  #    hist(dat$slope)
    test <- test[test$p_value_slope < 0.05,]
   st <- sqlQuery(db,paste("SELECT station_id, lat, lon FROM main_table"))
  #    hist(dat$slope)
  # close data base
  odbcClose(db)
  # select only significant slopes
  test <- test[test$p_value_slope < 0.05,]
  st.out <- cbind(st[st$station_id %in% test$station_id,],test[,2:4])
  return(list(stations = st, sig.st = st.out))
})


output$histogram <- renderPlot({
  if (input$goButton == 0)
    return() 
 # updateTabsetPanel(session, selected="summary")
  hist.fun(dat=as.data.frame(extractData()$sig.st))
  # plot the histograms
  #isolate(hist.fun(dat = as.data.frame(extractData())))
})

# Make the map of Australia with analysed data
output$map <- renderPlot({
  if (input$goButton == 0)
    return() 
  # 
  # only run when submit is pushed??
  isolate({
    p <- ggplot(subset(oz.map, border=="coast"), aes(long, lat, fill=state))
    p <- p + geom_path()
    p <- p + coord_equal()
    p <- p + ggtitle("Coastline of Australia")
    p
  })
})


output$dateMsg <- renderPrint({
  if (input$goButton == 0)
    return()
  # Display the dates that are available or chosen
  isolate(cat(DateInput()$StartMsg,DateInput()$EndMsg, "\n"))
})

output$slope <- renderPrint({
  if (input$goButton == 0) return("")
  isolate({
  cat("The slope of the regression line is",coef(DateInput()$lm.mod)[2],
        " with a p-value of",summary(DateInput()$lm.mod)$coefficients[2,4],".")
  cat(ifelse(summary(DateInput()$lm.mod)$coefficients[2,4]>0.05,
         " Statistically this means there is more than a 5% chance that this slope is similar to 0, 
         or we are not confident there is actually a trend.",
         paste(" Statistically this means there is less than a 5% chance that this slope is similar to 0, 
         meaning we are quite confident there is a",
         ifelse(coef(DateInput()$lm.mod)[2]>=0,"positive","negative"),
         "trend.")))
  })
})

output$fitResults <- renderPrint({
  if (input$goButton == 0) return("")
  isolate(
    cat("The adj. r_squared of the line fit is",summary(DateInput()$lm.mod)$adj.r.squared,
      " with an average residual value (RMSE) of",summary(DateInput()$lm.mod)$sigma,
      ifelse(input$type=="rain","mm","degrees C"),"."
      ," In general terms, the closer the adj. r-squared is to 1 means a better fit of the model to the data.")
  )
})

output$CautionComment <- renderPrint({
  if (input$goButton == 0) return("")
  isolate(
      cat("This analysis makes several assumptions, the most important one: that the trend in the data is linear! 
      Also, we are analysing the real data here, more common is to analyse the anomalies"))
})


  output$testoutput1 <- renderPrint({
     if (input$goButton == 0) ""
      # this is just a test to see if everything works
      "for testing" 
     #  str(extractData())    
      #DateInput()$lm.line[1:100]
   })


 })

#on.exit({
  # first delete the tables (see http://stackoverflow.com/questions/23913616/rodbc-sqlsave-table-creation-problems)
#  try(sqlDrop(db, sqtable="main_table", errors = F), silent=T)
#  try(sqlDrop(db, sqtable="regr_results", errors = F), silent=T)
#  try(sqlDrop(db, sqtable="regr_stats", errors = F), silent=T)
  # write data base tables back
#  sqlSave(db, df_main, tablename="main_table", rownames=FALSE,safer=F)
#  sqlSave(db, df_regr_results, tablename="regr_results", rownames=FALSE)
#  sqlSave(db, df_regr_stats, tablename="regr_stats", rownames=FALSE)
  
  # close the connection
  
 # })


