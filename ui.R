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
                 uiOutput("choice"),
                 # now choose the time period
                 br(),
                 dateRangeInput("dateRange",
                                label = 'Date range input: yyyy-mm-dd',
                                start = "1850-01-01", end =Sys.Date()
                 ),
                 
                 actionButton("goButton", "Go!")
                 ),
    mainPanel(
      tabsetPanel(
        tabPanel(h4("Data and analysis"),             
              textOutput("dateMsg"),
              br(),
              p("The results of the regression analysis are:"),
              textOutput("slope"),
              textOutput("fitResults"),
              textOutput("CautionComment"),
              
                          
              textOutput("testoutput1"),
            #  textOutput("testoutput2"),
                
              plotOutput("plot"),
              "Comment:this app is going to allow people to analyse different temperature and rainfall timeseries"
        ),
        tabPanel(h4("Summary"), "summary",
                 plotOutput("histogram")), 
        tabPanel(h4("Map"), "Map",
                 plotOutput("map"))      
  )
  )
  )
))
