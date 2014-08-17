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
      tabsetPanel(id = "resultstabs",
        tabPanel(title = h4("Data and analysis"), value = "data panel",             
              textOutput("dateMsg"),
              br(),
              p("The results of the regression analysis are:"),
              textOutput("slope"),
              br(),
              textOutput("fitResults"),
              br(),
              plotOutput("plot"),
              textOutput("CautionComment"),
              br(),          
              textOutput("testoutput1"),
            #  textOutput("testoutput2"),
                
              "Comment:this app is going to allow people to analyse different temperature and rainfall timeseries"
        ),
        tabPanel(title = h4("Summary"), value = "summary",
                 "Hit the", strong("GO"), "button to display the histograms.",
                 "This pages displays the distribution of analysed slopes as histograms",
                 br(),
                 plotOutput("histogram", height = 700, width = 400)), 
        tabPanel(title = h4("Map"), value = "Map",
                 "This is going to be a map that displays all the analysed stations",
                 plotOutput("map"))      
  )
  )
  )
))
