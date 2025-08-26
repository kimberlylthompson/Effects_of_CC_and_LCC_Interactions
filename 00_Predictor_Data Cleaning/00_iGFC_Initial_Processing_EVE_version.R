##################################################################################################
########                                                                                  ########
########                       Processing iGFC (Canopy Cover) Data                        ######## 
########                                                                                  ########
##################################################################################################

# Author: Kimberly Thompson

# This code processes the canopy cover data. The data contains three sets:
# mean canopy cover, Canopy cover for Lower Bound of 95% Confidence Interval, and
# canopy cover for upper bound of 95% confidence interval.
# Period covered is 1992 - 2018.
# Resolution is 300 meters.
# Values represent percentage coverage.

# ****NOTE: Performed on HPC Cluster

# Produces datasets:
# 'iGFC-canopyDensity_19920000_300m_USACAN.tif'
# and on through 2018


# Tree-canopy cover data downloaded from:
# https://zenodo.org/records/7901290



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

# Parallelizing over the 27 files - i loop below run on HPC cluster

canopy.path <- "/gpfs1/data/idiv_meyer/01_projects/Kim_T/iGFC_raw"

# list the land cover files 
canopy.files <- list.files(canopy.path, pattern = "canopy")

# Define the array (aka climate variable) ID
i = as.integer(Sys.getenv('SLURM_ARRAY_TASK_ID'))



###############################################
###                                         ###
###     Processing: Land Cover States       ###
###                                         ###
###############################################

# Goals: Crop to North America, convert values to proportion


# for (i in 1:length(canopy.files)) {
  
  # Read in the canopy file
  setwd("/gpfs1/data/idiv_meyer/01_projects/Kim_T/iGFC_raw")
  forest <- terra :: rast(canopy.files[i])
  
  # Create new USA shapefile with same CRS as the forest layer
  usa2 <- sf :: st_transform(usa.shape, crs(forest))
  
  # Crop the forest raster to USA/Canada
  forest <- terra :: crop(forest,
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
  forest <- terra :: project(forest, albers, method = "bilinear")
  
  # Save forest tif
  # setwd("I:/MAS/01_projects/iUpdate/00_Data/processed/Canopy Cover")
  # setwd("~/share/groups/MAS/01_projects/iUpdate/00_Data/processed/Canopy Cover")
  setwd("/gpfs1/data/idiv_meyer/01_projects/Kim_T/Analysis_Output/Canopy_Cover")
  terra :: writeRaster(forest, paste(names(forest), "_USACAN.tif", sep = ""))
  
  rm(forest, usa2)
  gc()
  print(i)
  
# } # end of i loop




