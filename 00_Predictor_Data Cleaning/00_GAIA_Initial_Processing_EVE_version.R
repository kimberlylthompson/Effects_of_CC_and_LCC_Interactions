##################################################################################################
########                                                                                  ########
########                       Processing GAIA (Urban Cover) Data                         ######## 
########                                                                                  ########
##################################################################################################

# Author: Kimberly Thompson

# This code processes the Gaia urban cover data. 
# Period covered is 1992 - 2018.
# Resolution is 300 meters.
# Values represent percentage coverage.

# ****NOTE: Performed on HPC Cluster

# Produces datasets:
# 'GAIA-urbanCover_19920000_300m_USACAN.tif'
# and on through 2018


# GAIA data downloaded from:
# https://data-starcloud.pcl.ac.cn/


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

# Parallelizing over the files - i loop below run through HPC cluster

urban.path <- "/gpfs1/data/idiv_meyer/01_projects/Kim_T/Gaia_raw"

# list the land cover files 
urban.files <- list.files(urban.path)

# Define the array (aka climate variable) ID
i = as.integer(Sys.getenv('SLURM_ARRAY_TASK_ID'))



###############################################
###                                         ###
###     Processing: Land Cover States       ###
###                                         ###
###############################################

# Goals: Crop to North America


# for (i in 1:length(urban.files)) {
  
  # Read in the urban file
  setwd("/gpfs1/data/idiv_meyer/01_projects/Kim_T/Gaia_raw")
  urban <- terra :: rast(urban.files[i])
  
  # Create new USA shapefile with same CRS as the urban layer
  usa2 <- sf :: st_transform(usa.shape, crs(urban))
  
  # Crop the urban raster to USA/Canada
  urban <- terra :: crop(urban,
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
  urban <- terra :: project(urban, albers, method = "bilinear")
  
  # Save urban tif
  setwd("/gpfs1/data/idiv_meyer/01_projects/Kim_T/Analysis_Output/Urban_Cover")
  terra :: writeRaster(urban, paste(names(urban), "_USACAN.tif", sep = ""))
  
  rm(urban, usa2)
  gc()
  print(i)
  
# } # end of i loop




