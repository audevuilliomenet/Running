library(leaflet)
library(shiny)
library(dplyr)

ui <- fluidPage(
  
  # Create the application title
  titlePanel("Running Route in London"),
  
  # Create the two part of the dashboard. 
  sidebarLayout(position = "right",
                # Create the sidebar Panel
                sidebarPanel(
                  # Add a description to know what to do!
                  helpText("Choose the ward of London you would like to start your run from!"),
                  
                  # Create a Slidebar to select the London Wards with route to run from!
                  selectInput("variable",
                              label = "Start Run Ward",
                              choices = run_november_sp@data$WardName),
                  # Create a second Slidebar to select the length of the run!
                  helpText("Choose the length of your run!
               Short Run : Less than 5 km.
               Intermediate Run : Between 5 and 10 km.
               Long Run : More than 10km. You feel motivated today!"),
                  selectInput("distance",
                              label = "Run Distance",
                              choices = run_november_sp@data$distance)
                  #conditionalPanel(
                  #  condition = "input.variable == input$variable",
                  #  tableOutput("runtable")
                  #)
                ),
                # Create the main Panel
                mainPanel(
                  leafletOutput(outputId = "londonmap", width="100%", height=1000)
                )
  )
)

shinyApp(ui,server)  

