## Categorize length of run! 
# make it a dataframe!
run_length_df <- data.frame(run_length)
# change from units to numeric to be able to use ifelse statement!
run_length_df$run_length <- as.numeric(run_length_df$run_length)
# Define the three categories, short run, intermediate run and long run. Add it as a column in the dataframe. 
run_length_df$distance <- ifelse(run_length_df$run_length <= 5000, "Short Run", 
                                 ifelse(run_length_df$run_length <= 10000.0 | run_length_df$run_length > 5000, "Intermediate Run",
                                        ifelse(run_length_df$run_length > 10000.0, "Long Run")))
# Append the run_length to the dataframe of allrun!
allrun_sf <- cbind(allrun_sf,run_length_df)

# Transform rund_dataframe to an sp object! Use in leaflet!
run_dataframe_sp <- as(run_dataframe, "Spatial")
# Understand how the latitude, longitude coordinate are coded in the sp dataframe!
run_dataframe_lng_1 <- (run_dataframe_sp@lines[[1]]@Lines[[1]]@coords[,1])
run_dataframe_lat_1 <- (run_dataframe_sp@lines[[1]]@Lines[[1]]@coords[,2])

### IF WARD NAME APPEAR ONLY ONCE (e.g = Heathfield), THEN IT POSSIBLE TO PASS THE DATA FRAME TO THE @LINES[[...]]!
### IF WARD NAME APPEAR MORE THAN ONCE, DATAFRAME HAS MORE THEN 1 ROW/RUN ID, NO MORE POSSIBLE TO GET THE LNG/LAT BY PASSING THE RESULT IN @LINES[[...]]
# EXAMPLE 1: ONLY 1 ROUTE IN THE WARD!
run_dataframe_wardname <- which(run_dataframe_sp@data$WardName == "Holloway")
run_dataframe_lng <- (run_dataframe_sp@lines[[run_dataframe_wardname]]@Lines[[1]]@coords[,1])
View(run_dataframe_wardname)
View(run_dataframe_lng)
# EXAMPLE 2: MORE THAN 1 ROUTE IN THE WARD!
run_dataframe_wardname_2 <- which(run_dataframe_sp@data$WardName == "Aldersgate")
run_dataframe_lng_2 <- (run_dataframe_sp@lines[[run_dataframe_wardname_2]]@Lines[[1]]@coords[,1])
View(run_dataframe_wardname_2)
View(run_dataframe_lng_2)