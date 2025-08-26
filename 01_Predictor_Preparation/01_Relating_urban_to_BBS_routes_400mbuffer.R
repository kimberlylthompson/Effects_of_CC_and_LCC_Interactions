##################################################################################################
########                                                                                  ########
########          Calculating proportional impervious cover for each BBS route            ######## 
########                              from the GAIA data                                  ########
##################################################################################################

# Author: Kimberly Thompson

# This code calculates the proportion of impervious (i.e. urban) cover within each BBS route
# from the period of 1992 - 2018 using GAIA dataset.
# Resolution is 300 meters

# In each raster, impervious cover is represented as percent coverage.

# ****NOTE: Performed on HPC Cluster

# Produces datasets:
# 'Urban_400mbuff_BBS_1.csv'
# and on through _27.csv, with each integer representing a year 1992-2018


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

lulc.predictors <- data.frame(Year = rep(seq(from = 1992, to = 2018, by = 1),
                                         length(bbs.routes$uniq_rt)),
                              unique_route = rep(bbs.routes$uniq_rt,
                                                 each = length(seq(from = 1992, to = 2018, by = 1))),
                              Mean = integer(length(bbs.routes$uniq_rt) *
                                               length(seq(from = 1992, to = 2018, by = 1))),
                              Median = integer(length(bbs.routes$uniq_rt) *
                                                 length(seq(from = 1992, to = 2018, by = 1))),
                              Min = integer(length(bbs.routes$uniq_rt) *
                                              length(seq(from = 1992, to = 2018, by = 1))),
                              Max = integer(length(bbs.routes$uniq_rt) *
                                              length(seq(from = 1992, to = 2018, by = 1))))


# Fill predictor columns with NA values
lulc.predictors[ , c(3:6)] <- NA



###############################################
###                                         ###
###        Preparing the parallel job       ###
###                                         ###
###############################################

# Parallelizing over the 27 years

lulc.path <- "/gpfs1/data/idiv_meyer/01_projects/Kim_T/Analysis_Output/Urban_Cover"

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


  
# Read in the canopy cover raster  
setwd("/gpfs1/data/idiv_meyer/01_projects/Kim_T/Analysis_Output/Urban_Cover")
lulc.raster <- raster :: raster(lulc.file)

# Extract the year of the raster file for use as an index further down
year <- substr(lulc.file, 17, 20)

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
    
    # Create a buffer around the route (in meters)
    buffered.route <- sf :: st_buffer(bbs.routes$geometry[j], dist = 400)
    
    # Create vector of the linestring (bbs.route)
    tmp.route <- terra :: vect(sf :: st_geometry(buffered.route))
    
    # Crop the spatRaster (year k's climate) to the linestring (route)
    cropped.lyr <- terra :: crop(terra :: rast(lulc.raster), tmp.route, snap = "out")
    
    
    ########################################
    ###  Calculate the percentage cover  ###
    
    # Extract the canopy cover values of each raster cell the BBS route touches
    tmp.df <- terra :: extract(cropped.lyr, tmp.route, method = "simple", factors = TRUE,
                               touches = TRUE)
    
    # Change the name of column 2 to make it easier to make calculations on the column
    names(tmp.df)[2] <- "Cover"
    
    # Sum the proportions of each cover type
    tmp.df_sum <- data.frame(Mean = numeric(1), Median = numeric(1), Min = numeric(1), Max = numeric(1),
                             CV = numeric(1))
    
    # Add the values to the tmp df
    tmp.df_sum$Mean <- mean(tmp.df$Cover, na.rm = TRUE)
    tmp.df_sum$Median <- median(tmp.df$Cover, na.rm = TRUE)
    tmp.df_sum$Min <- min(tmp.df$Cover, na.rm = TRUE)
    tmp.df_sum$Max <- max(tmp.df$Cover, na.rm = TRUE)
    
    
    # Fill in the predictor dataframe with the metric values
    lulc.predictors[lulc.predictors$Year == year & 
                      lulc.predictors$unique_route == unique(lulc.predictors$unique_route)[j], c(3:6)] <-
      tmp.df_sum[1, ]

    ########################################
    
    
    # Clean up workspace
    rm(tmp.route, cropped.lyr, x, total, tmp.df, tmp.df_sum)
    gc()
    
  } # end of if statement for assessing only contiguous USA
  
  print(paste("Year: ", year, "- Route: ", j, "- Completed"))
  
} # end of j loop


# Save the file
setwd("/gpfs1/data/idiv_meyer/01_projects/Kim_T/Analysis_Output/Urban_Cover_BBS")

write.csv(lulc.predictors, file = paste("Urban_400mbuff_BBS_", i, ".csv", sep = ""), row.names = FALSE)



