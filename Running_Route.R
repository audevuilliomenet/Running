library(sp)
library(sf)
library(dplyr)
library(tmap)

getwd()
setwd("/Users/audevuilliomenet/Documents/MSc. Smart Cities/GI Systems and Science/GIS_Running/Running/run.kml")

########  Read all the files in the folder! #############
# List all the files in the current directory! Here my kml files!
file_list <- list.files()

# Read the first file in file_list and create a dataframe.  
first_running <- data.frame(sf::st_read(file_list[1]))
dataframe_allrun <- first_running[1,]

# loops through all the file in file_list
# selection only the first row of the file
# append the first row (path route) to the dataframe
for (index in 2:length(file_list)){
  read_run_route <- data.frame(sf::st_read(file_list[index]))
  firstrow_run_route <- read_run_route[1,]
  dataframe_allrun <- rbind(dataframe_allrun, firstrow_run_route) 
  print(dataframe_allrun)
} 
# Change the dataframe to an sf object
allrun_sf <- sf::st_sf(dataframe_allrun) 
# Delete the z-coordinate of the run
allrun_sf <- st_zm(allrun_sf, drop = TRUE, what = "ZM")
# Set the coorindate system of the road to the BNG
BNG <- "+init=epsg:27700"
allrun_sf <- st_transform(allrun_sf, BNG)
# Delete a column we don't need it!
allrun_sf <- allrun_sf[,-2]

# View the run routes on an interactive map. 
tmap_mode("view")
tm_shape(allrun_sf) +
  tm_lines(col = "blue", lwd = 4, alpha = 0.5)

############### Open the LondonWardsBoundaries ###############
library(rgdal)
LondonWards <- readOGR("/Users/audevuilliomenet/Documents/MSc. Smart Cities/GI Systems and Science/GIS_Running/Running/LondonWardsBoundaries/LondonWardsNew.shp", layer="LondonWardsNew")
# Have a look at where the route are distributed in London!
tmap_mode("view")
tm_shape(LondonWards)+
  tm_polygons(col = NA, alpha = 0.2)+
  tm_shape(allrun_sf) +
  tm_lines(col = "blue", lwd = 4, alpha = 0.5)

################################################################################################################
########## Find where the route are in the London Wards! #########
# Find the intersection of the route with the London Wards! 
# Transform LondonWards to an sf object and set the coordinate system to BNG!
LondonWards_sf <- st_as_sf(LondonWards)
LondonWards_sf <- st_transform(LondonWards_sf, BNG)
# Delete some of the column in Londonwards_sf
LondonWards_sf <- LondonWards_sf[,-5]

# Find the intersection of the running route with the LondonWards. 
wards_run_intersect <- st_intersects(LondonWards_sf, allrun_sf)
View(wards_run_intersect)
# Save the values in a dataframe
wards_run_intersect_df <-  data.frame(wards_run_intersect)
# Rename the column of the dataframe to be more coherant!
names(wards_run_intersect_df)[1] <- "WardID"
names(wards_run_intersect_df)[2] <- "RunID"
View(wards_run_intersect_df)

############### Calculate the length of each run. ###############
run_length <- st_length(allrun_sf)
# Append the run_length to the dataframe of allrun!
allrun_sf <- cbind(allrun_sf,run_length)

## Categorize length of run! 
# change from units to numeric to be able to use ifelse statement!
allrun_sf$run_length <- as.numeric(run_length)
# Define the three categories, short run, intermediate run and long run. Add it as a column in the dataframe. 
allrun_sf$distance <- ifelse(allrun_sf$run_length <= 5000, "Short Run",
                             ifelse((allrun_sf$run_length >= 5000 & allrun_sf$run_length <= 10000), "Intermediate Run","Long Run"))
View(allrun_sf)

################################################################################################################
########### What are the next steps?? #################
# Need to create something that when I selection the name of my ward, the run intersecting there are plot!
# Intersection of the running route with the wards
# Return all the route id that are in a given wards

# Rename the column name for LondonWards_sf to be the same as in wards_run_intersect_df.
names(LondonWards_sf)[1] <- "WardID" 
names(LondonWards_sf)[4] <- "WardName"
names(LondonWards_sf)[2] <- "WardCode"

#### Append the Allrun and LondonWards dataframes to the wards_run_intersect dataframe
# This allow us to have the run geometry and the LondonWards name for each corresponding run!
# Create a new column for allrun_sf dataframe 
RunID <- seq(1,length(file_list))
allrun_sf <- cbind(allrun_sf,RunID)
# Append the Running Coordinate to the allrun_sf dataframe!
allrun_RunID <- merge(wards_run_intersect_df,allrun_sf, by="RunID")
# Append the LondonWards to the new allrun dataframe. 
allrun_WardID <- merge(allrun_RunID,LondonWards_sf, by="WardID")
# Delete the geometry for the LondonWards
allrun_WardID <-  allrun_WardID[,-c(12)]
# Rename the allrun_WardID geometry!
names(allrun_WardID)[6] <- "geometry"
View(allrun_WardID)
class(allrun_WardID)
## Transfrom allrun_WardID back to an sf object!
allrun_route_sf <- st_as_sf(allrun_WardID)

# Transform the coordinate system to WGS84 to be able to plot with leaflet!
WGS84 <- "+init=epsg:4326"
run_dataframe <- st_transform(allrun_route_sf, WGS84)
# Transform rund_dataframe to an sp object! Use in leaflet!
run_dataframe_sp <- as(run_dataframe, "Spatial")
# View(run_dataframe_sp)
View(run_dataframe_sp)

################################################################################################################
#### Find the first coordinate of the running path (first longitude/first latitude) to put a marker in leaflet!

# create a dataframe for adding the first lng/lat of each running path!
#### Longitude ####
first_lng <- data.frame(run_dataframe_sp@lines[[1]]@Lines[[1]]@coords[1,1])
lng <- first_lng[1,]
#### Latitude ####
first_lat <- data.frame(run_dataframe_sp@lines[[1]]@Lines[[1]]@coords[1,2])
lat <- first_lat[1,]

# loops through all the file in SpatialLine Dataframe!
# selection only the first longitude value of the running path.
# append the value to the lng dataframe. 
# Do the same for the latitude!
#### Longitude ####
for (i in 2:nrow(run_dataframe_sp)){
  get_lng <-  run_dataframe_sp@lines[[i]]@Lines[[1]]@coords[1,1]
  lng <- rbind(lng,get_lng)
}
#### Latitude ####
for (i in 2:nrow(run_dataframe_sp)){
  get_lat <-  run_dataframe_sp@lines[[i]]@Lines[[1]]@coords[1,2]
  lat <- rbind(lat,get_lat)
}
# Append both lng/lat to the data of run_dataframe_sp!
class(run_dataframe_sp)
run_dataframe_sp@data$lng <- cbind(run_dataframe_sp@data$lng, lng) 
run_dataframe_sp@data$lat <- cbind(run_dataframe_sp@data$lat, lat) 

## Function to selection the Ward to start from!
RunInWard <- function(run_dataframe){
  vars <- readline("Please enter the Ward you would like to start your run")
 
  columnvector_run <- which(run_dataframe$WardName == vars)
  run_selectedward <- run_dataframe[columnvector_run,]
  print(run_selectedward)
  # Create the plot!
  for (j in 1:nrow(run_dataframe)){
    run_map_plot <- tm_shape(run_selectedward) + 
      tm_lines(col = "blue", lwd=4, alpha=0.5)
  return(run_map_plot)
  }  
}

for (i in 1:nrow:run_dataframe_sp){
  run_distance <- 
}

###############################################################
### Color Function depending of the running length!
#getColor <- function(run_dataframe_sp) {
#  sapply(run_dataframe_sp$distance, function(distance) {
#    if(distance == "Short Run") {
#      "green"
#    } else if (distance == "Intermediate Run") {
#      "orange"
#    } else {
#      "red"
#    }})
#}

## Get color for each running path!
#color <- getColor(run_dataframe_sp)
## Append it to the sp run_dataframe!
#run_dataframe_sp@data$color <- cbind(run_dataframe_sp@data$color, color) 


### Create the Icons for the leaflet map! 
#icons <- awesomeIcons(
#  icon = 'ios-close',
#  iconColor = 'black',
#  library = 'ion',
#  markerColor = getColor(run_dataframe_sp)
#)
#View(icons)
