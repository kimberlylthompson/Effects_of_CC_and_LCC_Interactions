##################################################################################################
########                                                                                  ########
########               Calculating Average Climate values from PRISM data                 ######## 
########                             for May and June for each year                       ########
##################################################################################################

# Author: Kimberly Thompson

# This code calculates average: precipitation (mm),
# minimum temperature, maximum temperature, and mean temperature (C) for May and June
# Combined in each year
# (1966 - 2020), though actual analyis utilizes only 1992-2018

# Produces datasets:
# 'Precipitation Mean MayJune.tif'
# 'Tmax Mean MayJune.tif'
# 'Tmean Mean MayJune.tif'
# 'Tmin Mean MayJune.tif'


# Important websites:
# PRISM 
# https://www.prism.oregonstate.edu



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
###             Data Processing             ###
###                                         ###
###############################################

# Create blank stack to store the delta values in for each variable
avg.ppt <- stack()
avg.tmin <- stack()
avg.tmax <- stack()
avg.tmean <- stack()

# Define the file path
path <- "00_data/processed/PRISM/Monthly Climate Data/"


for(i in 1:length(climate.vars)) {
  
  # Read in the monthly (average) values for May
  monthly_may <- stack(paste(path, paste("PRISM_", climate.vars[i], "_1966to2020_4km_May.tif", sep = ""), sep = ""))
  
  # Read in the monthly (average) values for June
  monthly_june <- stack(paste(path, paste("PRISM_", climate.vars[i], "_1966to2020_4km_June.tif", sep = ""), sep = ""))
  
  for(j in 1:nlayers(monthly_may)) {
    
    avg <- raster :: overlay(monthly_may[[j]], monthly_june[[j]], fun = mean)
    
    if(i == 1) {
      
      avg.ppt <- raster :: addLayer(avg.ppt, avg)
      
    } else {
      
      if(i == 2) {
        
        avg.tmin <- raster :: addLayer(avg.tmin, avg)
        
      } else {
        
        if(i == 3) {
          
          avg.tmax <- raster :: addLayer(avg.tmax, avg)
          
        } else {
          
          if (i == 4) {
            
            avg.tmean <- raster :: addLayer(avg.tmean, avg)
          }
        }
      }
    }
    
    print(paste("Layer ", j, sep = ""))
  } # end of j loop
  
  print(paste("Finished with climate variable", i, sep = " "))
} # end of i loop


# Save the stack of deltas for each variable
path2 <- "00_data/processed/PRISM/MayJune Average Values/"
writeRaster(avg.ppt, paste(path2, "Precipitation Mean MayJune.tif", sep = ""), overwrite=TRUE)
writeRaster(avg.tmax, paste(path2, "Tmax Mean MayJune.tif", sep = ""),  overwrite = TRUE)
writeRaster(avg.tmin, paste(path2, "Tmin Mean MayJune.tif", sep = ""),  overwrite = TRUE)
writeRaster(avg.tmean, paste(path2, "Tmean Mean MayJune.tif", sep = ""),  overwrite = TRUE)

