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
      helpText("Choose the ward of London you would like to start your run from!"),
      
      # Create a Slidebar to select the London Wards with route to run from!
      selectInput("variable",
                  label = "Start Run Ward",
                  choices = run_dataframe_sp@data$WardName)
    ),
    # Create the main Panel
    mainPanel(
      leafletOutput(outputId = "londonmap", width="100%", height=1000)
    )
  )
)

## 1. Example : sf object!
server <- function(input,output){
  # create the map 
  output$londonmap <- renderLeaflet({
    leaflet(run_dataframe) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addPolylines(data = run_dataframe)
      # addPolylines(run_dataframe[input$variable])
      # ERROR : addPolylines must be called with both lng and lat, or with neither.
    
      # addPopups()
      # ERROR: Need to have lng/lat 
  })
}

## 2. Example : Change it to sp object!
server <- function(input,output){
  # create the map 
  output$londonmap <- renderLeaflet({
    leaflet(run_dataframe_sp) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      # Draw only one runpath depending on the choice of the runline!
      # How would it be possible to pass(input$variable) to the @lines[[...]]@Lines??
      addPolylines(lng = run_dataframe_sp@lines[[8]]@Lines[[1]]@coords[,1], lat = run_dataframe_sp@lines[[8]]@Lines[[1]]@coords[,2], run_dataframe_sp@data[[8]])
      # addPolylines(lng = run_dataframe_sp@lines[[input$variable]]@Lines[[1]]@coords[,1], lat = run_dataframe_sp@lines[[input$variable]]@Lines[[1]]@coords[,2], run_dataframe_sp@data[[input$variable]])
      # ERROR : trying to get slot "Lines" from an object of a basic class ("NULL") with no slots
  })
} 

shinyApp(ui,server)  
