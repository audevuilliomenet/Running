library(leaflet)
library(shiny)
library(dplyr)

ui <- fluidPage(
  
  # Create the application title
  titlePanel("Run Route in London"),
  
  # Create the two part of the dashboard. 
  sidebarLayout(position = "right",
    # Create the sidebar Panel
    sidebarPanel(
      # Add a description to know what to do!
      helpText("Choose the LondonWards you would like to start your run from!"),
      
      # Create a Slidebar to select the London Wards with route to run from!
      selectInput("name",
                  label = "Start Run Ward",
                  choices = allrun_route_sf$WardName)
    ),
    # Create the main Panel
    mainPanel(
      # leafletOutput("londonmap", width="100%", height=1000)
    )
  )
)
server <- function(input,output){}
shinyApp(ui,server)  
