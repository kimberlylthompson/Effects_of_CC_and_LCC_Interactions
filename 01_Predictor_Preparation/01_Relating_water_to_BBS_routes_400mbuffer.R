##################################################################################################
########                                                                                  ########
########         Calculating proportional surface water coverage for each BBS route       ######## 
########                              from the GSW data                                   ########
##################################################################################################

# Author: Kimberly Thompson

# This code calculates the proportion of permanent and seasonal water within each BBS route
# from the period of 1984 - 2020.

# Permanent and seasonal water are in two separate rasters, but they will be added together
# to get one value of water.

# In each raster, water cover is represented as percent coverage.

# ****NOTE: Performed on HPC Cluster

# Produces datasets:
# 'Surface_Water_400mbuff_BBS_1.csv'
# and on through _74.csv with integers 1-37 representing permanent water cover for years 1984-2020
# and 38-74 representing seasonal water cover for years 1984-2020



########## clean workspace and load required packages ####################

# clean workspace to improve efficiency: #
# rm(list = ls() ) 
# gc() #releases memory


# Load required packages
library(terra)
library(raster)
library(sf)
library(sp)
library(tidyverse)


###############################################
###                                         ###
###             Data Loading                ###
###                                         ###
###############################################


# Read in the shapefile of the BBS routes
setwd("/gpfs1/data/idiv_meyer/01_projects/Kim_T/BBS_Data")
bbs.routes <- sf :: st_read("BBS Rte 20 yrs_Linestring.shp")

# Pare down file to only be unique route, geometry, country and state columns
bbs.routes <- bbs.routes[ , c("Country", "St_Abrv", "uniq_rt", "geometry")]


# Define the coordinate system
albers = sp :: CRS("epsg:5070")

# Make sure bbs.routes are in albers
bbs.routes <- sf :: st_transform(bbs.routes, crs = albers)


###############################################
###                                         ###
###         Set up blank dataframe          ###
###                                         ###
###############################################

lulc.predictors <- data.frame(Year = rep(seq(from = 1984, to = 2020, by = 1),
                                         length(bbs.routes$uniq_rt)),
                              unique_route = rep(bbs.routes$uniq_rt,
                                                 each = length(seq(from = 1984, to = 2020, by = 1))),
                              Mean = integer(length(bbs.routes$uniq_rt) *
                                               length(seq(from = 1984, to = 2020, by = 1))))


# Fill predictor columns with NA values
lulc.predictors[ , 3] <- NA



###############################################
###                                         ###
###        Preparing the parallel job       ###
###                                         ###
###############################################

# Parallelizing over the 37 years * 2 types = 74 combos
lulc.path <- "/gpfs1/data/idiv_meyer/01_projects/Kim_T/Analysis_Output/Surface_Water"

# list the land cover files 
lulc.files <- list.files(lulc.path)

# Define the array (aka climate variable) ID
i = as.integer(Sys.getenv('SLURM_ARRAY_TASK_ID'))

lulc.file <- lulc.files[i]



###############################################
###                                         ###
###       Extract Proportional Cover        ###
###           for each BBS Route            ###
###############################################


  
# Read in the water raster  
# setwd("~/share/groups/MAS/01_projects/iUpdate/00_Data/processed/GSW_Cropped")
setwd("/gpfs1/data/idiv_meyer/01_projects/Kim_T/Analysis_Output/Surface_Water")
lulc.raster <- raster :: raster(lulc.file)

# Extract the year of the raster file for use as an index further down
year <- substr(as.integer( sub("\\D*(\\d+).*", "\\1", lulc.file) ), 1, 4)

# Project the raster to be in the same projection as the bbs routes
# This projection uses meters and is therefore appropriate for calculating planar distances
# Use method bilinear which is good for categorical variables
# This part is computationally intensive - takes about 40 minutes

lulc.raster <- raster :: projectRaster(lulc.raster, crs = albers, method = 'bilinear')


#######################################


for (j in 1:length(unique(lulc.predictors$unique_route))) { # number of routes
  
  # Execute only for the routes in the contiguous USA
  if(bbs.routes$Country[j] == "US" & bbs.routes$St_Abrv[j] != "AK") {
    
    ###################################################
    ### Determine the spatial weights of each route ###
    
    # Create a buffer around the route
    buffered.route <- sf :: st_buffer(bbs.routes$geometry[j], dist = 400)
    
    # Create vector of the linestring (bbs.route)
    tmp.route <- terra :: vect(sf :: st_geometry(buffered.route))
    
    # Crop the spatRaster (year k's climate) to the linestring (route)
    cropped.lyr <- terra :: crop(terra :: rast(lulc.raster), tmp.route, snap = "out")
    
    
    ########################################
    ###  Calculate the percentage cover  ###
    
    # Extract the water cover values of each raster cell the BBS route touches
    tmp.df <- terra :: extract(cropped.lyr, tmp.route, method = "simple", factors = TRUE,
                               touches = TRUE)
    
    # Change the name of column 2 to make it easier to make calculations on the column
    names(tmp.df)[2] <- "Cover"
    
    # Find the mean of the proportional water cover
    tmp.df_sum <- data.frame(Mean = numeric(1))
    
    # Add the values to the tmp df
    tmp.df_sum$Mean <- mean(tmp.df$Cover, na.rm = TRUE)
    
    
    # Fill in the predictor dataframe with the metric values
    lulc.predictors[lulc.predictors$Year == year & 
                      lulc.predictors$unique_route == unique(lulc.predictors$unique_route)[j], 3] <-
      tmp.df_sum[1, ]

    ########################################
    
    
    # Clean up workspace
    rm(tmp.route, cropped.lyr, x, total, tmp.df, tmp.df_sum)
    gc()
    
  } # end of if statement for assessing only contiguous USA
  
  print(paste("Year: ", year, "- Route: ", j, "- Completed"))
  
} # end of j loop


# Save the file
setwd("/gpfs1/data/idiv_meyer/01_projects/Kim_T/Analysis_Output/Surface_Water_BBS")

write.csv(lulc.predictors, file = paste("Surface_Water_400mbuff_BBS_", i, ".csv", sep = ""), row.names = FALSE)



