library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel(h1("Climate Change Citizen Science", align="center", style = "color:blue")),
  sidebarLayout(position = "left",
    sidebarPanel(h3("Settings"),
                 # first choose the station
                 textInput("Station", label = h4("Enter Station name"), 
                           value = "Observatory hill"),
                 br(),
                 selectInput("state", label = "State or Territory",
                             choices = c("ACT", "NSW", "QLD", "VIC", "WA", "TAS", "NT"),
                             selected ="NSW"),
                 br(),
                 # now choose Temperature or Rainfall
                 selectInput("type", 
                             label = "Choose Rainfall or min/max temperature",
                             choices = c("rain", "min_temp", "max_temp"),
                             selected = "max_temp"),
                 br(),
                 uiOutput('choice'),
                 # now choose the time period
                 br(),
                 dateRangeInput('dateRange',
                                label = 'Date range input: yyyy-mm-dd',
                                start = Sys.Date() - 2, end = Sys.Date() + 2
                 ),
                 
                 actionButton("goButton", "Go!")
                 ),
    mainPanel(h3("Data and analysis"),
              
              textOutput("testoutput1"),
            #  textOutput("testoutput2"),
                
              plotOutput("plot"),
              "Comment:this app is going to allow people to analyse different temperature and rainfall timeseries")
    
  )
  
))
