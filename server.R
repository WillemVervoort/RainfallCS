library(shiny)
library(RODBC)

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

# This is the start of the server part
# this gives the input and output
shinyServer(function(input, output) {
  
  # here code that runs every time the app is visited

  # create storage lists that are reactive
  data <- reactiveValues()
  name <- reactiveValues()
  dates <- reactiveValues()
# find the station in the 
  StationOut <- reactive({
    # find the station name in the station data set, now includes finding the state
      name$name <- stations[grep(input$Station,stations$Site_name,
                                 ignore.case=T),c("Site","Site_name","STA")]
     # This selects at least a station in the right state, but what to do if I have more than 1 station?
      name$name2 <- name$name[grep(input$state,name$name$STA),]
    return(name$name2$Site)
  })

  output$choice <- renderUI({
      selectInput('choice', label='Choose a Station',
                  selected=StationOut()[1], StationOut())
  })

StationInput <- reactive({
      if (input$goButton == 0) 
        return()
      # use dataripper to download data from BOM station, now fudged to get 1 station
      data <- bomDailyObs(input$choice,observation=input$type)
      return(data)
      })
  
DateInput <- reactive({
   if (input$goButton == 0)  
    return()
   # this needs to read the dates and store them somewhere
   begin <- input$dateRange[1]
   end <- input$dateRange[2]
   
   if (begin >= min(StationInput()$Date)) {
     dates$Startdate <- begin
     dates$StartMsg <- paste("data analysis will start from",begin)
   } else {
     dates$Startdate <- min(StationInput()$Date)
     dates$StartMsg <- paste("the data only starts at", min(StationInput()$Date))
   }
   
   if (end <= max(StationInput()$Date)) {
     dates$Enddate <- end
     dates$EndMsg <- paste("and the data ends at",end)
   } else {
     dates$Enddate <- max(StationInput()$Date)
     dates$EndMsg <- paste("and the data only runs till", max(StationInput()$Date))
   }
   return(dates)
 })  

 ####################################################################
  # Now run the regression using lm and store the data into a database
  # need to work out what database (huge? or just small)
  # current thinking is: just make a database locally, move later
  # Probably can use AERDM from the Uni and therefore use MySQL
  
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
    # define labels
    if (input$type=="rain") lab <- "Rainfall"; plot.t <- "h"
    if (input$type=="min_temp") lab <- "Minimum Temperature"; plot.t <- "l"
    if (input$type=="max_temp") lab <- "Maximum Temperature"; plot.t <- "l"
    # make the plot
    # define the begin and end rows to plot
    temp <- match(DateInput()$Startdate,StationInput()$Date)
    temp2 <- match(DateInput()$Enddate,StationInput()$Date)
    
    
    # define the regression data frame
    df <- data.frame(time=1:nrow(StationInput()[temp:temp2,]), 
                     response=StationInput()[temp:temp2,6])
    # run the regression (start simple with just linear)
    lm.mod <- lm(response~time,df)
    switch(lm.mod, "lm.mod" = lm.mod)
    
    # predict the regression line
    lm.line <- predict(lm.mod)
    # make the plot
    plot(StationInput()$Date[temp:temp2],
         StationInput()[temp:temp2,6],
         type=plot.t,col="blue",
         xlab= "Date", ylab=lab)
    lines(StationInput()$Date[temp:temp2],lm.line)
    })

output$dateMsg <- renderPrint({
  if (input$goButton == 0)
    return()
  # Display the dates that are available or chosen
  isolate(cat(DateInput()$StartMsg,DateInput()$EndMsg, "\n"))
})


 output$testoutput1 <- renderPrint({
     if (input$goButton == 0)
       return()
     # this is just a test to see if everything works
     summary(lm.mod)
  })

# Now need to run the regression and create the output
# store output into a database
  #output$testoutput1 <- renderText(paste("station selected =",as.character(TypeInput)))
  # on a second tab, extract the database info, summarise and plot
  
 })
