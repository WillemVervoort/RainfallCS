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
      tabsetPanel(id = "resultstab",
        tabPanel(title = h4("Data and analysis"),             
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
        tabPanel(title = h4("Rainfall"), value="rain", 
                 "Hit the", strong("GO"), "button to display maps and histograms.",
                 "This pages displays the distribution of analysed slopes for rainfall as maps and as histograms",
                 br(),
                 plotOutput("rain_map"),
                 "Red points are negative significant slopes, blue points are positive significant slopes, black points are non-significant slopes"#,
                 #plotOutput("rain_histogram", height = 700, width = 400)
                 ), 
        tabPanel(title = h4("Maximum T"), value="maxT",
                 "Hit the", strong("GO"), "button to display maps and histograms.",
                 "This pages displays the distribution of analysed slopes for maximum temperature as maps and as histograms",
                 br(),
                 plotOutput("maxT_map"),
                 "Red points are negative significant slopes, blue points are positive significant slopes, black points are non-significant slopes"#,
                 #plotOutput("maxT_histogram", height = 700, width = 400)
                 ), 
        tabPanel(title = h4("Minimum T"), value="minT",
               "Hit the", strong("GO"), "button to display maps and histograms.",
               "This pages displays the distribution of analysed slopes for minimum temperature as maps and as histograms",
               br(),
               plotOutput("minT_map"),
               "Red points are negative significant slopes, blue points are positive significant slopes, black points are non-significant slopes"#,      
               #plotOutput("maxT_histogram", height = 700, width = 400)
        ) 
      )
    )
))
)