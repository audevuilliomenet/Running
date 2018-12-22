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
      selectInput("wardname",
                  label = "Start Run Ward",
                  choices = run_dataframe$WardName)
    ),
    # Create the main Panel
    mainPanel(
      leafletOutput(outputId = "londonmap", width="100%", height=1000)
    )
  )
)
server <- function(input,output){
  # create the map 
  output$londonmap <- renderLeaflet({
    leaflet(run_dataframe) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addPolylines(data = run_dataframe)
  })
}

shinyApp(ui,server)  
