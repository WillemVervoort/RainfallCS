library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel(h1("Climate Change Citizen Science", align="center", style = "color:blue")),
  
  sidebarLayout(position = "left",
    sidebarPanel(h3("Settings"),
                 # first choose the station
                 textInput("Station", label = h4("Enter Station name"), 
                           value = "Enter Station..."),
                 submitButton("Submit"),
                 br(),
                 br(),
                 # now choose the time period
                 h4("Choose your time period"),
                 sliderInput("Decades", "number of decades from end:",  
                             min = 1, max = 1000, value = 500),
                 "Comment:these values need to be adjusted depending on the dataset",
                 br(),
                 "alternative is:",
                 dateRangeInput("dates", label = h4("Date range"),
                                start=c("2004-01-01"),end="2014-01-01"),
                 submitButton("Submit")
                 ),
    mainPanel(h3("Data and analysis"),
              renderPlot(),
              "Comment:this app is going to allow people to analyse different temperature and rainfall timeseries")
    
  )
  
))
