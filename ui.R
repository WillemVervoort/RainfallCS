library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel(h1("Climate Change Citizen Science", align="center", style = "color:blue")),
  sidebarLayout(position = "left",
    sidebarPanel(h3("Settings"),
                 # first choose the station
                 textInput("Station", label = h4("Enter Station name"), 
                           value = ""),
                 submitButton("Submit"),
                 br(),
                 # now choose Temperature or Rainfall
                 selectInput("type", 
                             label = "Choose Rainfall or temperature",
                             choices = c("Rainfall", "Temperature"),
                             selected = "Rainfall"),
                 br(),
                 # now choose the time period
                 br(),
                 dateRangeInput("dates", label = h4("Date range"),
                                start=c("2004-01-01"),end="2014-01-01"),
                 submitButton("Submit")
                 ),
    mainPanel(h3("Data and analysis"),
              textOutput("testoutput1"),
              textOutput("testoutput2"),
                
              #renderPlot(),
              "Comment:this app is going to allow people to analyse different temperature and rainfall timeseries")
    
  )
  
))
