server <- function(input,output){
  # create the map 
  output$londonmap <- renderLeaflet({
    leaflet(run_dataframe_sp) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addPolylines()
  })
  
  proxy <- leafletProxy("londonmap")
  
  # Observe function for the map element to redraw if the ward is changed!
  observe({
    if (input$variable != ""){
      # Select only the rows in run_dataframe_sp that correspond to the choosen ward!
      selected_ward <- subset(run_dataframe_sp,run_dataframe_sp@data$WardName==input$variable)
      ward_coords <- selected_ward@lines[[1]]@Lines[[1]]@coords
      
      # Clear the previous running path that were shown!
      proxy %>% clearShapes()
      
      # center the view on the first running path
      # proxy %>% setView(lng=ward_coords[1],lat=ward_coords[2],zoom=10)
      
      # Redraw the Running Path that correspond to the selected ward!
      proxy %>% addPolylines(data = selected_ward) 
    }
  })
  
  # Observe function for the map element to redraw if the length of run is changed!
#  observe({
#    ifelse (input$distance != ""){
#      # Select only the rows in chosen ward that correspond to the run distance!
#      selected_ward_distance <- subset(run_dataframe_sp,run_dataframe_sp@data$distance==input$distance)
#      
#      # Clear the previous running path that were shown!
#      proxy %>% clearShapes()
#      
#      # center the view on the first running path
#      # proxy %>% setView(lng=ward_coords[1],lat=ward_coords[2],zoom=10)
#      
#      # Redraw the Running Path that correspond to the selected ward!
#      proxy %>% addPolylines(data = selected_ward_distance) 
#    }
#  })
}

