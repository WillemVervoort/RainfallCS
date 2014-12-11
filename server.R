library(shiny)
library(RODBC)
library(oz)
# for fancier plotting
library(ggplot2)


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


# Source the dataripper script to get the data from the BOM site
# This is adaption of Jason Lessels' bomDailyDataripper
source("dataripper.r")
# # this is using the multiplot function from the R cookbook
source("helper.r")

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
print(t.plot)
}

# mapping plot function
# this function needs to be expanded to include frequnecy of analysis
# in the size of the points.
plot.map <- function(Data_in, background=oz.map, dtype="rain") {
  p1 <- ggplot(subset(background, border=="coast"), aes(long, lat))
  p1 <- p1 + geom_path()
  p1 <- p1 + coord_equal()
  p1 <- p1 + ggtitle("Australia")
  #    p
  #    p1 <- p +  geom_polygon(data=subset(oz.map,border="coast"), aes(fill=state))
  # Calculate the number of times a station is analysed
  st.count <- count(Data_in$stations$station_ID)
  for (i in 1:nrow(st.count)) {
    Data_in$stations[Data_in$stations$station_ID %in% st.count$x[i],"count"] <- st.count$freq[i]
  }
  #neg slopes
  st.count <- count(Data_in$neg.st$station_ID)
  for (i in 1:nrow(st.count)) {
    Data_in$neg.st[Data_in$neg.st$station_ID %in% st.count$x[i],"count"] <- st.count$freq[i]
  }
 # pos slopes
  st.count <- count(Data_in$pos.st$station_ID)
  for (i in 1:nrow(st.count)) {
    Data_in$pos.st[Data_in$pos.st$station_ID %in% st.count$x[i],"count"] <- st.count$freq[i]
  }
  
  p1 <- p1 + geom_point(data=subset(Data_in$stations,Data_in$stations$data_type==dtype),
                        aes(x=lon,y=lat),colour="black",size=count)
  p1 <- p1 + geom_point(data=subset(Data_in$neg.st,Data_in$neg.st$data_type==dtype),
                        aes(x=lon,y=lat),colour="red",size=count)
  p1 <- p1 + geom_point(data=subset(Data_in$pos.st,Data_in$pos.st$data_type==dtype),
                        aes(x=lon,y=lat),colour="blue",size=count)
  return(p1)
  
  
}


# histogram plotting function
hist.fun <- function(Data_in, dtype="rain") {

  label <- ifelse(dtype=="rain","Rainfall",
                  ifelse(dtype=="max_temp","Maximum T","Mimimum T"))
  units <- ifelse(dtype=="rain","mm/day",
                  ifelse(dtype=="max_temp","degree C/day","degree C/day"))
  
  
  p2 <- ggplot(subset(Data_in,Data_in$data_type==dtype),
               aes(x=slope))
  p2 <- p2 + geom_histogram() +
    xlab(paste("Change in", units, "for significant",label,
               "slopes with p-value < 0.05"))
  return(p2)
}
  
# histogram plotting function for times analysed
hist.timefun <- function(Data_in, dtype="rain") {
  
  Data_in$times <- round(as.numeric(difftime(Data_in$end_date,Data_in$start_date))/365,0)
  
  label <- ifelse(dtype=="rain","Rainfall",
                  ifelse(dtype=="max_temp","Maximum T","Mimimum T"))
    
  p3 <- ggplot(subset(Data_in,Data_in$data_type==dtype),
               aes(x=times))
  p3 <- p3 + geom_histogram() +
    xlab(paste("Number of years analysed for significant",label,
               "slopes with p-value < 0.05"))
  return(p3)
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
   Lat <- as.numeric(as.character(StationOut()$Lat[grep(input$choice,StationOut()$Site)]))
   Lon <- as.numeric(as.character(StationOut()$Lon[grep(input$choice,StationOut()$Site)]))
   
   # Create input line for database  
   test <- data.frame(station_ID = as.character(input$choice),
                      Lat = Lat,
                      Lon = Lon,
                      data_type = input$type,
                      timestamp = as.character(Sys.time()),
                      start_date = as.character(dates$Startdate),
                      end_date = as.character(dates$Enddate),
                      comment = "testing",stringsAsFactors=F)
#    main_table$station_ID = 
#    main_table$Lat = Lat;  line$Lon = Lon
#    main_table$data_type = as.character(input$type)
#    main_table$comment = as.character("testing")
#    # append database
   db <- odbcConnect("testwillem", uid="rver4657", pwd="7564revrMySQL", case="nochange")
  sqlSave(db,test,tablename="main_table",
          rownames=F,append=T)
   
   # sqlQuery(db,"DESCRIBE main_table")
#   odbcClose(db)
#    # coefficients and stats
    mod.res <-  summary(lm.mod)$coefficients   
    results <- data.frame(station_ID = as.character(input$choice),intercept = mod.res[1,1],
            se_int = mod.res[1,2], p_value_int = mod.res[1,4],
            slope = mod.res[2,1], se_slope = mod.res[2,2],
            p_value_slope = mod.res[2,4], data_type=input$type,
            comment="test", stringsAsFactors=F)
  # append database
  try(sqlSave(db, results, tablename="regr_results", 
            rownames=FALSE, append=T))
# model statistics and summary
    mod.sum <- summary(lm.mod)
    fstat<-mod.sum$fstatistic
    pv <-   pf(fstat[1], fstat[2], fstat[3], lower.tail=FALSE) 
    bias <- sum(residuals(lm.mod),na.rm=T)
    stats <- data.frame(station_ID = as.character(input$choice), rmse = mod.sum$sigma, 
                      r_sq = mod.sum$adj.r.squared, p_value = pv, 
                      bias = bias, stat1 = mod.sum$r.squared, 
                      stat2 = fstat[1], stat3 = fstat[2], 
                      stat4 = fstat[3], stat5 = -9999, data_type=input$type,
                      comment = "test", stringsAsFactors=F)
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

extractData <- reactive({
  if (input$goButton == 0)
    return() 
    db <- odbcConnect("testwillem", uid="rver4657", pwd="7564revrMySQL")
    test <- sqlQuery(db,paste("SELECT station_ID, slope, p_value_slope, data_type FROM regr_results"))
  #    hist(dat$slope)
  #  test <- test[test$p_value_slope < 0.05,]
   st <- sqlQuery(db,paste("SELECT station_ID, lat, lon, start_date, end_date FROM main_table"))
  #    hist(dat$slope)
  st.all <- cbind(st,test)
  # close data base
  odbcClose(db)
  # select only significant slopes
  st.out <- st.all[st.all$p_value_slope < 0.05,]
  st.neg <- st.out[st.out$slope < 0,]
  st.pos <- st.out[st.out$slope > 0,]
  return(list(stations = st.all, sig.st = st.out, neg.st = st.neg, pos.st = st.pos))
})



# Make the map of Australia with analysed data
# and a histogram
output$rain_map <- renderPlot({
  if (input$goButton == 0)
    return("") 
  # 
  # only run when submit is pushed??
    isolate({ 
      fig1 <- plot.map(Data_in = extractData(), dtype="rain" )
      fig2 <- hist.fun(Data_in=as.data.frame(extractData()$sig.st), dtype="rain")
      fig3 <- hist.timefun(Data_in=as.data.frame(extractData()$sig.st), dtype="rain")
      multiplot(fig1,fig2,fig3,cols=1)
      })
})

# Make a map of Australia with pos and neg slopes
output$maxT_map <- renderPlot({
  if (input$goButton == 0)
    return("") 
  # 
  # only run when submit is pushed??
  
  isolate({ 
    fig1 <-  plot.map(Data_in = extractData(), dtype="max_temp" )
    fig2 <- hist.fun(Data_in=as.data.frame(extractData()$sig.st), dtype="max_temp")
    fig3 <- hist.timefun(Data_in=as.data.frame(extractData()$sig.st), dtype="max_temp")
    multiplot(fig1,fig2,fig3,cols=1)
  })
})

output$minT_map <- renderPlot({
  if (input$goButton == 0)
    return("") 
  # 
  # only run when submit is pushed??
  isolate({ 
    fig1 <- plot.map(Data_in = extractData(), dtype="min_temp" )
    fig2 <- hist.fun(Data_in=as.data.frame(extractData()$sig.st), dtype="min_temp")
    fig3 <- hist.timefun(Data_in=as.data.frame(extractData()$sig.st), dtype="min_temp")
    multiplot(fig1,fig2,fig3,cols=1)
    
  })
})


output$dateMsg <- renderPrint({
  if (input$goButton == 0)
    return("")
  # Display the dates that are available or chosen
  isolate(cat(DateInput()$StartMsg,DateInput()$EndMsg, "\n"))
})

output$slope <- renderPrint({
  if (input$goButton == 0) return("")
  isolate({
  cat("The slope of the regression line is",round(coef(DateInput()$lm.mod)[2],3),
        " with a p-value of",round(summary(DateInput()$lm.mod)$coefficients[2,4],3),".")
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
    cat("The adj. r_squared of the line fit is",round(summary(DateInput()$lm.mod)$adj.r.squared,2),
      " with an average residual value (RMSE) of",round(summary(DateInput()$lm.mod)$sigma,3),
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
      # str(extractData()$sig.st)    
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


