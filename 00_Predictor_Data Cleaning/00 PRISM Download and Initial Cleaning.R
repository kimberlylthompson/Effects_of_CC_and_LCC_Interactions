##################################################################################################
########                                                                                  ########
########                  Downloading and Processing PRISM climate data                   ######## 
########                                                                                  ########
##################################################################################################

# Author: Kimberly Thompson

# This code downloads and processes the May and June monthly normals of precip (mm), min temp, 
# max temp, and 
# mean temp, as well as the monthly means for these same variables for each year from 1966 - 2020.

# Produces datasets:
# 'PRISM_ppt_1966to2020_4km_May.tif'
# 'PRISM_tmin_1966to2020_4km_May.tif'
# 'PRISM_tmax_1966to2020_4km_May.tif'
# 'PRISM_tmean_1966to2020_4km_May.tif'
# 'PRISM_ppt_1966to2020_4km_June.tif'
# 'PRISM_tmin_1966to2020_4km_June.tif'
# 'PRISM_tmax_1966to2020_4km_June.tif'
# 'PRISM_tmean_1966to2020_4km_June.tif'


# R tutorial for data download (not followed to the letter but used as a guide)
# https://rpubs.com/collnell/get_prism


########## clean workspace and load required packages ####################

# clean workspace to improve efficiency: #
# rm(list = ls() ) 
# gc() #releases memory


# Load required packages
library(reshape2)
library(dplyr)
library(raster)
library(sf)
library(sp)
library(prism)


###############################################
###                                         ###
###             Set parameters              ###
###                                         ###
###############################################

# Define list of variables to download normals for
climate.vars <- c("ppt", "tmin", "tmax", "tmean")

# Define the coordinate system
albers = sp :: CRS("epsg:5070")



###############################################
###                                         ###
###    Loop for downloading & processing    ###
###             Monthly Data                ###
###############################################

years <- seq(1966, 2020, by = 1)

path <- "00_data/original/PRISM"/
path2 <- "00_data/processed/PRISM/"

#### ONLY NEEDS TO BE DONE ONCE ####

# Download and process data for each climate variable of interest
for(j in 5:6) { # months of May and June
  for (i in 1:length(climate.vars)) {
    
    # Set download folder for data
    prism_set_dl_dir(paste(path, climate.vars[i], sep = ""))
    
    # Download the PRISM data for precipitation (mm, rain and melted snow), tmin, tmax, and tmean
    # for the years 1966 - 2021
    get_prism_monthlys(type = climate.vars[i], years = years, mon = j, keepZip = FALSE)
    
    # Make a raster stack of the data
    RS <- pd_stack(prism_archive_subset(type = climate.vars[i], temp_period = "monthly",
                                        mon = j))
    
    # Define the projection of the raster
    RS <- raster :: projectRaster( from = RS, crs = albers )
    
    # Save the raster stack
    if(j == 5) {
      writeRaster(RS, paste(path2, paste("PRISM_", climate.vars[i], "_1966to2020_4km_May.tif", sep = ""), sep = ""))
    } else {
      if(j == 6) {
        writeRaster(RS, paste(path2, paste("PRISM_", climate.vars[i], "_1966to2020_4km_June.tif", sep = ""), sep = ""))
      }
    }
    
    print(i) 
  } # end of i loop
  print(j)
} # end of j loop



