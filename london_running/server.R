library(shiny)

# source("dataloader.r")

# Define server function to draw the run routes
shinyServer(function(input, output) {
# Define server function to draw the run routes
# server <- function(input, output) {
    
  # Create color palette to draw each runs depending on their type (short, intermediate, long)
  pal <- colorFactor(palette = c("blue", "red", "green"), domain = run_november_sp$distance)
    
  # Create the London Map with the run!
  output$londonmap <- renderLeaflet({
    leaflet(run_november_sp) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      # Set the view to London
      setView(-0.118092,51.509865, zoom = 10) %>%
      # Add the LondonWards
      addPolygons(fillColor= "transparent", weight = 0.6, opacity = 0.6, color = "black", fillOpacity = 0.1, data = LondonWards_sf, label = ~as.character(WardName)) %>%
      # Plot the runs 
      addPolylines(color=~pal(distance), weight = 0.8) %>%
      # Add marker to the start of each run
      addMarkers(lat = ~latitude, lng = ~longitude, label =~as.character(distance), data = run_november_sp)
  })
    
  proxy <- leafletProxy("londonmap")
    
  # Observe function for the map element to redraw if the ward is changed!
  observe({
      
    if (input$variable != ""){
      # Select only the rows in run_dataframe_sp that correspond to the choosen ward!
      selected_ward <- subset(run_november_sp,run_november_sp@data$WardName==input$variable)
        
      # Clear the previous running path that were shown!
      proxy %>% clearShapes() %>% clearMarkers()
        
      # Redraw the Running Path that correspond to the selected ward!
      proxy %>% 
        addPolylines(color=~pal(distance), weight = 2, opacity=1, data = selected_ward) %>% 
        addMarkers(lat = ~latitude, lng = ~longitude, label =~as.character(distance), data = selected_ward)
    }
      
    if (input$distance != ""){
      # Select only the rows in run_dataframe_sp that correspond to the choosen distance!
      selected_ward_distance <- subset(selected_ward,selected_ward@data$distance==input$distance)
      # This line will select all the run in London corresponding to short, intermediate or long!        #selected_ward_distance <- subset(run_november_sp,run_november_sp@data$distance==input$distance)
        
      # Clear the previous running path that were shown! And the markers!
      proxy %>% clearShapes() %>% clearMarkers()
        
      # center the view on the first running path        
      # proxy %>% setView(lng=ward_coords[1],lat=ward_coords[2],zoom=10)
  
      # Redraw the Running Path that correspond to the selected ward!
      proxy %>% 
        addPolylines(color=~pal(distance), weight = 2,opacity = 1, data = selected_ward_distance) %>% 
        addMarkers(lat = ~latitude, lng = ~longitude,label =~as.character(run_length_november), data = selected_ward_distance)
    }
  }) 
} )
