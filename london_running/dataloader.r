library(sp)
library(sf)
library(tmap)
library(dplyr)
library(rgdal)
library(shiny)
library(leaflet)

# Need to setwd inside the Folder that contains the kml file for all the runs!Example below!
# setwd("/Users/audevuilliomenet/Documents/MSc. Smart Cities/GI Systems and Science/GIS London Run/Running/london_running")
# setwd(".... /Run_November.kml")

run_november <- list.files(path="Run_November")

run_november_first <- data.frame(sf::st_read(paste("Run_November/",run_november[1],sep="")))
run_nov_route_1 <- run_november_first[1,]
dataframe_allrun_november <- run_nov_route_1

for (i in run_november){
  #print(paste("Run_November/",i,sep=""))
  runs_nov <- data.frame(sf::st_read(paste("Run_November/",i,sep="")))
  runs_route <- runs_nov[1,]
  dataframe_allrun_november <- rbind(dataframe_allrun_november, runs_route)
}

dataframe_allrun_november <- dataframe_allrun_november[-c(1),]

# Set wd back to the shiny Application for the London boundaries!
# LondonWards <- readOGR("/Users/audevuilliomenet/Documents/MSc. Smart Cities/GI Systems and Science/GIS London Run/Running/london_running/LondonWardsBoundaries/LondonWardsNew.shp", layer="LondonWardsNew")

# Read the first file in file_list and create a dataframe. 
# first_running <- data.frame(sf::st_read(run_november[1]))
# class(first_running$geometry)
# dataframe_allrun_november <- first_running[1,]

# loops through all the file in file_list
# selection only the first row of the file
# append the first row (path route) to the dataframe
# for (index in 2:length(run_november)){
#  read_run <- data.frame(sf::st_read(run_november[index]))
#  firstrow_run <- read_run[1,]
#  dataframe_allrun_november <- rbind(dataframe_allrun_november, firstrow_run) 
# } 

# Change the dataframe to an sf object
allrun_november_sf <- sf::st_sf(dataframe_allrun_november) 
# Delete the z-coordinate of the run
allrun_november_sf <- st_zm(allrun_november_sf, drop = TRUE, what = "ZM")
# Set the coorindate system of the road to the WGS84. Here WGS84 is choosen rather than British National Grid, as for later use in leaflet, WGS84 is prefered!
WGS84 <- "+init=epsg:4326"
allrun_november_sf  <- st_transform(allrun_november_sf, WGS84)
# Delete a column we don't need it!
allrun_november_sf  <- allrun_november_sf[,-2]

# Calculate length of each run
run_length_november <- st_length(allrun_november_sf)
# Append the run_length to the dataframe!
allrun_november_sf <- cbind(allrun_november_sf,run_length_november)

# Define Run categories
allrun_november_sf$run_length_november <- as.numeric(run_length_november)
# Define the three categories, short run, intermediate run and long run. Add it as a column in the dataframe. 
allrun_november_sf$distance <- ifelse(allrun_november_sf$run_length_november <= 5000, "Short Run",
                                      ifelse((allrun_november_sf$run_length_november >= 5000 & allrun_november_sf$run_length_november <= 10000), "Intermediate Run","Long Run"))

# Transfrom the sf object to an sp object! It will be easier to extract the first latitude/longitude for each path. 
run_november_sp <- as(allrun_november_sf, "Spatial")
# create a dataframe for adding the first lng/lat of each running path!
#### Longitude ####
longitude <- data.frame(run_november_sp@lines[[1]]@Lines[[1]]@coords[1,1])
longitude <- longitude[1,]
#### Latitude ####
latitude <- data.frame(run_november_sp@lines[[1]]@Lines[[1]]@coords[1,2])
latitude <- latitude[1,]

# loops through all the file in SpatialLine Dataframe!
# selection only the first longitude value of the running path.
# append the value to the lng dataframe. 
# Do the same for the latitude!
#### Longitude ####
for (i in 2:nrow(run_november_sp)){
  get_lng <-  run_november_sp@lines[[i]]@Lines[[1]]@coords[1,1]
  longitude <- rbind(longitude,get_lng)
}
#### Latitude ####
for (i in 2:nrow(run_november_sp)){
  get_lat <-  run_november_sp@lines[[i]]@Lines[[1]]@coords[1,2]
  latitude <- rbind(latitude,get_lat)
}
# Append both lng/lat to the data of run_dataframe_sp!
run_november_sp@data$longitude <- cbind(run_november_sp@data$longitude, longitude) 
run_november_sp@data$latitude <- cbind(run_november_sp@data$latitude, latitude) 

# Transform back to an sf object!
run_november_sf <- st_as_sf(run_november_sp)

# Read the LondonWards boundaries!
LondonWards <- readOGR("LondonWardsBoundaries/LondonWardsNew.shp", layer="LondonWardsNew")

# Transfrom the LondonWards to an sf object and set coordinate system same as the run_dataframe_sf (WGS84). 
LondonWards_sf <- st_as_sf(LondonWards)
LondonWards_sf <- st_transform(LondonWards_sf, WGS84)
# Delete some of the column in Londonwards_sf
LondonWards_sf <- LondonWards_sf[,-5]

# Find the intersection of the running route with the LondonWards. 
wards_run_intersect <- st_intersects(LondonWards_sf, run_november_sf)
# Save the values in a dataframe
wards_run_intersect_df <-  data.frame(wards_run_intersect)
# Rename the column of the dataframe to be more coherant!
names(wards_run_intersect_df)[1] <- "WardID"
names(wards_run_intersect_df)[2] <- "RunID"

# Rename the column name for LondonWards_sf to be the same as in wards_run_intersect_df.
names(LondonWards_sf)[1] <- "WardID" 
names(LondonWards_sf)[2] <- "WardCode"
names(LondonWards_sf)[4] <- "WardName"
#### Append the Allrun November and LondonWards dataframes to the wards_run_intersect dataframe
# This allow us to have the run geometry and the LondonWards name for each corresponding run!
# Create a new column for allrun_november_sf dataframe 
RunID <- seq(1,length(run_november))
run_november_sf <- cbind(run_november_sf,RunID)
# Append the Running Coordinate to the run_november_sf dataframe!
run_november_RunID <- merge(wards_run_intersect_df,run_november_sf, by="RunID")
# Append the LondonWards to the run_november dataframe. 
run_november_WardID <- merge(run_november_RunID,LondonWards_sf, by="WardID")
# View(run_november_WardID)
# Delete the geometry for the LondonWards
run_november_WardID <-  run_november_WardID[,-c(14)]
# Rename the run_november_WardID geometry!
names(run_november_WardID)[8] <- "geometry"
## Transfrom run_november_WardID back to an sf object!
run_november_sf <- st_as_sf(run_november_WardID)

# Transform the coordinate system to WGS84 to be able to plot with leaflet!
WGS84 <- "+init=epsg:4326"
run_november_crs <- st_transform(run_november_sf, WGS84)
# Transform rund_dataframe to an sp object! Use in leaflet!
run_november_sp <- as(run_november_crs, "Spatial")

