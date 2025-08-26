##################################################################################################
########                                                                                  ########
########                       Processing ESA-CCI Bias-corrected Data                     ######## 
########                                                                                  ########
##################################################################################################

# Author: Kimberly Thompson

# This code processes the ESA-CCI bias-corrected land cover data, which I will use for cropland
# cover.
# Period covered is 1992 - 2020.
# Resolution is 300 meters.
# Values represent land cover classes (of which there are 52).

# ****NOTE: Performed on HPC Cluster

# Produces datasets: 
# 'iCCI-landCover_19920000_300m_USACAN.tif'
# and on through 2018


# Website for downloading CCI data:
# https://cds.climate.copernicus.eu/cdsapp#!/dataset/satellite-land-cover?tab=overview


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

print("USA/Can shapefile successfully loaded")

###############################################
###                                         ###
###        Preparing the parallel job       ###
###                                         ###
###############################################

# Parallelizing over the 27 files - i loop below run through the HPC cluster

lc.path <- "/gpfs1/data/idiv_meyer/01_projects/Kim_T/CCI_raw"

# list the land cover files 
lc.files <- list.files(lc.path)

# Define the array (aka file number) ID
i = as.integer(Sys.getenv('SLURM_ARRAY_TASK_ID'))

print("Parallel job set up")


###############################################
###                                         ###
###     Processing: Land Cover States       ###
###                                         ###
###############################################



# for (i in 1:length(lc.files)) {
  
  # Read in the file to crop
  setwd("/gpfs1/data/idiv_meyer/01_projects/Kim_T/CCI_raw")
  lc <- terra :: rast(lc.files[i])
  
  # Create new USA shapefile with same CRS as the land cover layer
  usa2 <- sf :: st_transform(usa.shape, crs(lc))
  
  # Crop the cropland raster to USA/Canada
  lc <- terra :: crop(lc,
                          raster :: extent(as(st_geometry(usa2), Class = "Spatial")),
                          snap = "out")
  
  # Collect garbage to minimize memory used
  gc()

  
  # Project the raster to be in the same projection as the bbs routes
  # This projection uses meters and is therefore appropriate for calculating planar distances
  # Since values are categorical the method must be nearest neighbor
  lc <- terra :: project(lc, albers, method = "near")
  
  # Save lc tif
  setwd("/gpfs1/data/idiv_meyer/01_projects/Kim_T/Analysis_Output/CCI_Cover")
  terra :: writeRaster(lc, paste(names(lc), "_USACAN.tif", sep = ""))
  
  rm(lc, usa2)
  gc()
  print(i)
  
# } # end of i loop




