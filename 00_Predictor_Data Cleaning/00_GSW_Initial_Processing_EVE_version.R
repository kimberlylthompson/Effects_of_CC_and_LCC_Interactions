##################################################################################################
########                                                                                  ########
########                       Processing Global Surface Water Data                       ######## 
########                                                                                  ########
##################################################################################################

# Author: Kimberly Thompson

# This code processes the GSW permanent and seasonal surface water data. The data contains two sets:
# pixel percentage of permanent water and pixel percentage of seasonal water.
# Period covered is 1984 - 2020.
# Resolution is 300 meters.

# ****NOTE: Performed on HPC Cluster

# Produces datasets:
# 'GSW-permanent_19840000_300m_USACAN.tif'
# 'GSW-seasonal_19840000_300m_USACAN.tif'
# and on through 2020


# Global Surface water data downloaded from:
# https://global-surface-water.appspot.com/download



########## clean workspace and load required packages ####################

# clean workspace to improve efficiency: #
# rm(list = ls() ) 
# gc() #releases memory


# Load required packages
library(terra)
library(raster)
library(sp)
library(sf)


###############################################
###                                         ###
###             Data Loading                ###
###                                         ###
###############################################

# Load USA and Canada shapefile
setwd("/gpfs1/data/idiv_meyer/01_projects/Kim_T/USA Shapefiles")
usa.shape <- sf :: st_read("USACANAB.SHP" )

# Define the coordinate system
albers = sp :: CRS("epsg:5070")

# Define the coordinate system for the polygon
usa.shape <- sf :: st_set_crs(usa.shape, albers)




###############################################
###                                         ###
###        Preparing the parallel job       ###
###                                         ###
###############################################

# Parallelizing over the 74 files - i loop below run on HPC cluster

water.path <- "/gpfs1/data/idiv_meyer/01_projects/Kim_T/GSW_raw"

# list the land cover files 
water.files <- list.files(water.path, pattern = "permanent|seasonal")

# Define the array (aka climate variable) ID
i = as.integer(Sys.getenv('SLURM_ARRAY_TASK_ID'))



###############################################
###                                         ###
###     Processing: Land Cover States       ###
###                                         ###
###############################################

# Goals: Crop to North America, 


# for (i in 1:length(water.files)) {
  
  # Read in the water file
  setwd("/gpfs1/data/idiv_meyer/01_projects/Kim_T/GSW_raw")
  water <- terra :: rast(water.files[i])
  
  # Create new USA shapefile with same CRS as the forest layer
  usa2 <- sf :: st_transform(usa.shape, crs(water))
  
  # Crop the water raster to USA/Canada
  water <- terra :: crop(water,
                          raster :: extent(as(st_geometry(usa2), Class = "Spatial")),
                          snap = "out")
  
  # Collect garbage to minimize memory used
  gc()

  
  # Project the raster to be in the same projection as the bbs routes
  # This projection uses meters and is therefore appropriate for calculating planar distances
  # the default method - bilinear - is fine for continuous values
  # The difference between bilinear and cubic is essentially that BL will be slightly faster and 
  # result in less smoothing of the resulting surface bc CC interpolates the output value using
  # a greater number of input values within a local neighborhood
  # https://gis.stackexchange.com/questions/121646/what-resampling-technique-should-be-used-to-reproject-an-altitude-raster
  water <- terra :: project(water, albers, method = "bilinear")
  
  # Save water tif
  setwd("/gpfs1/data/idiv_meyer/01_projects/Kim_T/Analysis_Output/Surface_Water")
  terra :: writeRaster(water, paste(names(water), "_USACAN.tif", sep = ""))
  
  rm(water, usa2)
  gc()
  print(i)
  
# } # end of i loop




