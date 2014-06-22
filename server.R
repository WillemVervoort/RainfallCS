library(shiny)

# here code that runs when app is launched
# stations <- read.fwf("20140617_AllBOMstations.txt", 
#                      widths=c(7,6,43,7,7,9,10,16,4,10,9,7),
#                     skip = 3,
#               na.strings = c("..",".....","null"))
# colnames(stations) <-c("Site","Dist","Site_name","Start","End","Lat",
#                        "Lon","Source","STA","Height_m","Bar_ht","WMO")
# save(stations, file="Stations.Rdata")
load("stations.Rdata")

# This is the server part
shinyServer(function(input, output) {
  
  # here code that runs every time the app is visited
  # source(helper.r) # this has helper functions and is stored in www

# find the station in the 
  StationInput <- reactive({
    # find the station name in the station data set
      SelectStation <- stations[grep(input$Station,
                                     stations$Site_name,ignore.case=T),"Site"]
      # temporary, just use the first one, later work out how to deal with this
      SelectStation <- SelectStation[1]
      temp <- tempfile()
      download.file(paste("http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_display_type=dailyZippedDataFile&p_stn_num=0",SelectStation,"&p_c=-1069999971&p_nccObsCode=136&p_startYear=2014",sep=""),
                    temp, mode="wb")
      raindata <- read.table(unz(temp, paste("IDCJAC0009_0",SelectStation,"_1800.zip",sep=""),"r"))
      unlink(temp)
      
      
  })
  
  TypeInput <- reactive({
      SelectType <- input$type
  })
      
    
    
    
  # In here a function that looks up the rainfall station at the BOM website, downloads and unzips the data
  # is it possible to unzip using R?
  # yes you can, example from stack overflow
  
    # part of an example, use "switch" to use R variables (counties$white)
  #      data <- switch(input$var, 
  #"Percent White" = counties$white,
  #"Percent Black" = counties$black,
  #"Percent Hispanic" = counties$hispanic,
  #"Percent Asian" = counties$asian)
  
  # Now run the regression using lm and store the data into a database
  # need to work out what database (huge? or just small)
  
  # and create a plot
  output$plot <- renderPlot({    
    # this code only runs when renderplot is called
  #  chartSeries(dataInput(), theme = chartTheme("white"), 
   #             type = "line", log.scale = input$log, TA = NULL)
  })
  #output$testoutput1 <- renderText(paste("station selected =",as.character(SelectStation[1])))
  #output$testoutput1 <- renderText(paste("station selected =",as.character(TypeInput)))
  # on a second tab, extract the database info, summarise and plot
  
 })
