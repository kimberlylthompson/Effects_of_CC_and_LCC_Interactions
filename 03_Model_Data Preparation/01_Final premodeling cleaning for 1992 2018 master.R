#################################################################################################
########                                                                                 ########
########              Subsetting BBS Routes and the master to the exact                  ######## 
########                routes that will be modeled from 1992 - 2018                     ########
#################################################################################################

# Author: Kimberly Thompson

# This code does some housekeeping by paring the overall sf of BBS routes that have been surveyed
# at least 20 years to those in the contiguous USA only. It also pares the BBS route linestring 
# shapefile to match the routes in the master.

# Code also removes any routes with duplicate coordinates so that the list of routes exactly matches
# between the sf object and the master dataframe used in the modeling.

# Finally, it also adds the Bird Conservation Region to each route.

# Produces dataset:
# 'Cleaned_GLS Master_Trends of Responses and Predictors_1992 to 2018.csv'
# 'BBS Rte 20 yrs_Linestring_1992to2018.shp'


########## clean workspace and load required packages ####################

# # clean workspace to improve efficiency: #
# rm(list = ls() ) 
# gc() #releases memory

library(sf)
library(tidyverse)


###############################################
###                                         ###
###             Data Loading                ###
###                                         ###
###############################################

# Load the GLS frequentist master for years 1992 - 2018
path <- "00_Data/processed/"
master <- read.csv(paste(path,
                         "GLS Master_Trends of Responses and Predictors_1992 to 2018.csv",
                         sep = ""),
                   header = TRUE)


# Load the bbs routes linestring file
path2 <- "00_Data/processed/BBS Data/"
bbs.routes <- sf :: st_read(paste(path2,
                                  "BBS Rte 20 yrs_Linestring.shp",
                                  sep = ""))

# Load the route information that contains the BCR for each route
path3 <- "00_Data/raw/BBS Observations/State Level Data/"
route.info <- read.csv("All BBS Routes 1966_2019.csv", header = TRUE)



###############################################
###                                         ###
###           Data Manipulation             ###
###                                         ###
###############################################

###########
# Master  #
###########

# Remove routes from the master that are in Canada or Alaska
# Canada's country number is 124, Alaska's statnum is 03 (though it won't have the leading zero)

master <- master[master$countrynum != 124 & master$statenum != 3, ]


##############
# BBS Routes #
##############

# Pare down file to only be unique route, geometry, country and state columns
bbs.routes <- bbs.routes[ , c("Country", "statenm", "St_Abrv", "uniq_rt", "geometry")]

# Define the coordinate system
albers = sp :: CRS("epsg:5070")

# Make sure bbs.routes are in albers
bbs.routes <- sf :: st_transform(bbs.routes, crs = albers)

# Filter bbs routes to show usa routes only
bbs.routes <- bbs.routes[bbs.routes$Country != "CA" & bbs.routes$St_Abrv != "AK", ]



###############################################
###                                         ###
###           Matching the routes           ###
###                                         ###
###############################################

### Pare bbs.routes sf object to be only the routes in the master ###

# Create a vector of routes to retain
routes_to_retain <- unique(master$unique_route)

# Create a dataframe to store subsetted observations (same column names as originals)
bbs.routes2 <- bbs.routes[0, ]

# Subset the observation dataframe to include only these routes
for (i in 1:length(routes_to_retain)) {
  
  # Temporary dataframe for the observations
  tmp <- bbs.routes %>%
    subset(bbs.routes$uniq_rt == routes_to_retain[i])
  
  # Bind observations together
  bbs.routes2 <- rbind(bbs.routes2, tmp)
  
  print(i)
  
}


### Check for duplicates in the bbs routes ###

# If there are duplicates they need to be removed from both the route df and the master
# Create a column that will show which geometries are unique
bbs.routes2$distinct <- st_equals(bbs.routes2$geometry)

# Create a list of the elements in bbs.routes3$distinct that have a length longer than 1 
# (thereby indicating that they are part of a duplicate)
dup.routes <- bbs.routes2$distinct[sapply(bbs.routes2$distinct, function(x) length(x) > 1)]

# Create a blank vector
routes_to_remove <- vector()

# Was getting a lot of errors about incorrect dimensions and unary operators when I tried 
# to identify and remove the rows in one loop, so it's not very elegant but I've split into
# two processes. First - identify the routes to remove.
for (i in 1:length(unique(dup.routes))) {
  for (j in 1:length(bbs.routes2$uniq_rt)) {
    
    # If the duplicated route indices are in the distinct column
    if(identical(unique(dup.routes)[[i]], bbs.routes2$distinct[[j]])) {
      
      # Find the route where the condition is true
      routes_to_remove <- c(routes_to_remove, bbs.routes2$uniq_rt[j])
    }
    print(j)
  } # end of j loop
} # end of i loop

# Second - remove the routes
bbs.routes2 <- bbs.routes2 %>%
  dplyr :: filter(!(uniq_rt %in% routes_to_remove))


# These routes need to be removed from the master as well
master <- master %>%
  dplyr :: filter(!(unique_route %in% routes_to_remove))



###############################################
###                                         ###
###              Adding BCR                 ###
###                                         ###
###############################################

# Filter route info so that it contains only 1 entry per unique route
route.info <- route.info %>%
  distinct(unique_route, .keep_all = TRUE)

# Merge the BCR to the master dataframe
master <- merge(master, route.info[ , c("unique_route", "BCR")],
                by = "unique_route",
                all.x = TRUE)





# Save the revised versions of the bbs routes and the master

### Master
write.csv(master,
          paste(path, "Cleaned_GLS Master_Trends of Responses and Predictors_1992 to 2018.csv",
                sep = ""),
          row.names = FALSE)


### BBS data

# remove distinct column to enable saving
bbs.routes2$distinct <- NULL

st_write(bbs.routes2,
         paste(path2, "BBS Rte 20 yrs_Linestring_1992to2018.shp",
               sep = ""))


