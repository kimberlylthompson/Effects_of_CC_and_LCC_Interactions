############################################################################################
########                                                                            ########
########              Determining spatial coordinates of BBS routes                 ######## 
########                                                                            ########
############################################################################################

# Author: Kimberly Thompson

# This code creates a simple features (package sf) dataframe of spatial points for each route
# in the North American Breeding Bird Survey Dataset.
# The name of the route (as an identifier that can be matched with the biodiversity data)
# is also included.

# Important websites:
# Download shapefile of BBS routes from
# https://www.mbr-pwrc.usgs.gov/bbs/geographic_information/Instructions_trend_route.htm

# Produces shapefiles:
# 'BBS Rte 20 yrs_Point.shp'
# 'BBS Rte 20 yrs_Linestring.shp'



########## clean workspace and load required packages ####################

# clean workspace to improve efficiency: #
# rm(list = ls() )
# gc() #releases memory

library(raster)
library(sp)
library(sf)

library(ggplot2) # Plotting

library(tidyverse) # Data organization
library(lubridate) # Time values
library(data.table) # Data organization


###############################################
###                                         ###
###   Some Notes about Coordinate Systems   ###
###                                         ###
###############################################

# Deciding which projection to use
# https://gis.stackexchange.com/questions/104005/choosing-projected-coordinate-system-for-mapping-all-us-states
# Albers Equal Area Conic (Heinrich Albers, 1805): Like Lambert Conformal Conic, 
# this is a very popular map projection for the US, Canada and other continental/large countries
# with a primarily E-W extent. Used by the USGS for maps showing the conterminous United States
# (48 states) or large areas of the United States. Used for many thematic maps, especially
# choropleth and dot density maps.

# From searchable list of codes at http://www.spatialreference.org 
# Could not actually find the epsg code for this reference though
# EPSG code found at this site
#https://guides.library.duke.edu/r-geospatial/CRS
# epsg 5070: USA_Contiguous_Albers_Equal_Area_Conic


###############################################
###                                         ###
###        Load Shapefiles and CSVs         ###
###                                         ###
###############################################


# Define the coordinate system
albers = sp:: CRS("epsg:5070")

# Load USA and Canada shapefile
path <- "00_Data/raw/"
usa.shape <- sf :: st_read(paste(path, "USACANAB.shp", sep = ""))

# Define coordintate system as albers:
usa.shape <- sf :: st_set_crs( usa.shape, albers )


# Load BBS routes shapefile
# The variable that I want is the geometry (and maybe the length)
path2 <- "00_Data/raw/BBS Routes Shapefiles/"
route.shapefile <- sf :: st_read(paste(path, "nabbs02_mis_alb.shp", sep = ""))

# Define coordintate system as albers:
route.shapefile <- sf :: st_set_crs( route.shapefile, albers )


# Load the BBS data on the routes
path3 <- "00_Data/processed/BBS Data/"
bbs_route.data <- read.csv(paste(path, "BBS Routes with 20 years plus.csv", sep = ""), header = TRUE)

str(bbs_route.data)


###############################################
###                                         ###
###        Collapse bbs_route data          ###
###      to have one entry per route        ###
###############################################

bbs_route.data <- bbs_route.data[!duplicated(bbs_route.data[ , 'unique_route']) , ]
# 2974 routes retained so I know this is correct

# Remove columns that would not be universal for each route
bbs_route.data <- bbs_route.data[ , c(1:4, 6:12, 30:33)]


###############################################
###                                         ###
###    Convert bbs.routes into sf object    ###
###                                         ###
###                                         ###
###############################################

# Make bbs.routes an sf object (lat, long in degrees)
bbs_route.data_sf <- st_as_sf(x = bbs_route.data,
                          coords = c("Longitude", "Latitude"),
                          crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

# Convert crs to albers:
bbs_route.data_sf <- sf :: st_transform( bbs_route.data_sf, albers )



###############################################
###                                         ###
###    Find which route shapes match the    ###
###           bbs observations              ###
###                                         ###
###############################################

# Find the route shape feature that is closest to the bbs observations (point)
closest.route <- st_nearest_feature(bbs_route.data_sf, route.shapefile) # List of 2974, it works!

# Check that the order of the vector of indices for the closest linestring lines up
# with the corresponding point
plot(bbs_route.data_sf$geometry[1], add = TRUE)
plot(route.shapefile$geometry[closest.route[1]])

# Reduce the Route shapefile based on the vector of indices - this already has it in the right order
updated.route.shapefile <- route.shapefile[closest.route, ]

# updated.route.shapefile = linestring geometry
# bbs_route.data_sf = point geometry

# Create a dataframe with all the information in both the bbs routes df and the
# updated.route df but with the point geometry only
bbs_route.data_sf2 <- cbind(bbs_route.data_sf, updated.route.shapefile) 
bbs_route.data_sf2$geometry.1 <- NULL

# Create a dataframe with all the information in both the bbs routes df and the
# updated.route df but with the route shape (linestring) geometry only
updated.route.shapefile2 <- cbind(updated.route.shapefile, bbs_route.data)

# Check that these dfs line up correctly by examining whether the two name columns match
bbs_route.data_sf2$RouteName == bbs_route.data_sf2$SRTENAME

# There are some False occurrences but in checking these the names are either just slightly
# different or the features still line up

plot(usa.shape$geometry)
plot(updated.route.shapefile2$geometry, col = "green", add = TRUE)
plot(bbs_route.data_sf2$geometry, col = "red", add = TRUE)


# Write each sf dataframe
st_write(bbs_route.data_sf2, paste(path3, "BBS Rte 20 yrs_Point.shp", sep = ""))
st_write(updated.route.shapefile2, paste(path3, "BBS Rte 20 yrs_Linestring.shp", sep = ""))
  