##################################################################################################
########                                                                                  ########
########                Calculating the monthly climate values specific to                ######## 
########                       each BBS route - w/ weighted means                         ########
##################################################################################################

# Author: Kimberly Thompson

# Starting from the stacks of climate averages (one raster for each year for ppt, tmin, tmax, 
# and tmean, average = average monthly value between May and June) and the 
# shapefile of BBS routes, 
# this code calculates the climate values for each route.
# Climate variables are at 4 km resolution and routes span 40 km, so to derive a value for
# each route, this code calculates the weighted mean of the cells that overlap with 
# each route. Weights are determined by the proportion of each route in each cell.

# ****NOTE: Performed on HPC Cluster

# Produces datasets:
# 'Weighted MayJune Clim Values_400mbuff_for each_BBS Routes_1.csv'
# and on through _4.csv, with each digit corresponding to climate variables
# 1 = "ppt", 2 = "tmin", 3 = "tmax", 4 = "tmean"


########## clean workspace and load required packages ####################

# clean workspace to improve efficiency: #
# rm(list = ls() ) 
# gc() #releases memory


# Load required packages
library(reshape2)
library(dplyr)
library(sf)
library(sp)
library(terra)
library(raster)


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

climate.predictors <- data.frame(Year = rep(seq(from = 1966, to = 2020, by = 1),
                                            length(bbs.routes$uniq_rt)),
                                 unique_route = rep(bbs.routes$uniq_rt,
                                                    each = length(seq(from = 1966, to = 2020, by = 1))),
                                 ppt.wtmean = integer(length(bbs.routes$uniq_rt) *
                                                 length(seq(from = 1966, to = 2020, by = 1))),
                                 tmin.wtmean = integer(length(bbs.routes$uniq_rt) *
                                                  length(seq(from = 1966, to = 2020, by = 1))),
                                 tmax.wtmean = integer(length(bbs.routes$uniq_rt) *
                                                  length(seq(from = 1966, to = 2020, by = 1))),
                                 tmean.wtmean = integer(length(bbs.routes$uniq_rt) *
                                                   length(seq(from = 1966, to = 2020, by = 1))))

                                 
# Fill predictor columns with NA values
climate.predictors[ , c(3:6)] <- NA


###############################################
###                                         ###
###        Preparing the parallel job       ###
###                                         ###
###############################################

# Parallelizing over the 4 climate variables
clim.path <- "/gpfs1/data/idiv_meyer/01_projects/Kim_T/PRISM_processed/MayJune Average Values"

# list the climate files 
avg.files <- list.files(clim.path, pattern = "Mean")

# Define the array (aka climate variable) ID
i = as.integer(Sys.getenv('SLURM_ARRAY_TASK_ID'))

avg.file <- avg.files[i]

# Extract the climatic variable of the raster file for use in the print message at the end of the loop
clim.type <- substr(avg.file, 1, 4)


###############################################
###                                         ###
###    Extracting the yearly avg value      ###
###             for each route              ###
###############################################


# Read in the stack of avg rasters for the climate variable
setwd("/gpfs1/data/idiv_meyer/01_projects/Kim_T/PRISM_processed/MayJune Average Values")
avg.stack <- terra :: rast(avg.file)

# Project the bbs.routes to be in the same projection as the climate stack
# (More computationally efficient than the other way around)
bbs.routes <- sf :: st_transform(bbs.routes, crs(avg.stack))

for (j in 1:length(unique(climate.predictors$unique_route))) { # number of routes
  
  # Execute only for the routes in the contiguous USA
  if(bbs.routes$Country[j] == "US" & bbs.routes$St_Abrv[j] != "AK") {
    
    ### Determine the spatial weights with which to calculate the means ###
    
    # Isolate one layer (i.e. year) just to get the grid of climate cells
    tmp.layer <- avg.stack[[1]]
    
    # Create a buffer around the route
    buffered.route <- sf :: st_buffer(bbs.routes$geometry[j], dist = 400)
    
    # Create vector of the linestring (bbs.route)
    tmp.route <- terra :: vect(sf :: st_geometry(buffered.route))
    
    # Crop the spatRaster (year k's climate) to the linestring (route)
    cropped.lyr <- terra :: crop(tmp.layer, tmp.route, snap = "out")
    
    # Creates a raster of the linestring where the values of the cells is the length in each cell
    x <- terra :: rasterizeGeom(tmp.route, cropped.lyr, "area")
    
    # Calculate the total length of the route
    # (Sum function not working in terra, so have to use raster package here)
    total <- raster :: cellStats(raster :: raster(x), stat = 'sum')
    
    # Divide the values of the spatraster by the total length of the route to get a proportion for each cell
    x <- x / total
    
    # Clean up the workspace
    rm(tmp.layer, cropped.lyr)
    gc()
    
    ### Calculate weighted means for each year ###
    
    for (k in 1:nlyr(avg.stack)) { # number of years
      
      # x = spatRaster of spatial weights, tmp.route = the jth route from which to extract clim values
      #cropped.lyr = climate layer from which to find wgtd mean
      # Add the weighted mean to the blank dataframe
      # in the row with the matching year/route combo and the column that corresponds to
      # the correct variable measure
      # (Again, weighted mean function not working in terra, but it does work in raster pckg, so 
      # convert the spatRasters to rasters and calculate the weigthed mean)
      
      # First extract the climate values for the correct year and route
      tmp.layer <- avg.stack[[k]]
      
      # Crop the spatRaster (year k's climate) to the linestring (route)
      cropped.lyr <- terra :: crop(tmp.layer, tmp.route, snap = "out")
      
      
      if(clim.type == "Prec") { # Precipitation
        
        # Precip Weighted Mean
        climate.predictors[climate.predictors$unique_route ==
                             unique(climate.predictors$unique_route)[j] &
                             climate.predictors$Year ==
                             unique(climate.predictors$Year)[k], 3] <- 
          raster :: weighted.mean(raster :: raster(cropped.lyr), raster :: raster(x), na.rm = TRUE)
        
        
      } else {
        
        if(clim.type == "Tmin") { # Tmin
          
          # Tmin Weighted Mean
          climate.predictors[climate.predictors$unique_route ==
                               unique(climate.predictors$unique_route)[j] &
                               climate.predictors$Year ==
                               unique(climate.predictors$Year)[k], 4] <- 
            raster :: weighted.mean(raster :: raster(cropped.lyr), raster :: raster(x), na.rm = TRUE)
          
          
        } else {
          
          if(clim.type == "Tmax") { #Tmax
            
            # Tmax Weighted Mean
            climate.predictors[climate.predictors$unique_route ==
                                 unique(climate.predictors$unique_route)[j] &
                                 climate.predictors$Year ==
                                 unique(climate.predictors$Year)[k], 5] <- 
              raster :: weighted.mean(raster :: raster(cropped.lyr), raster :: raster(x), na.rm = TRUE)
            
            
          } else {
            
            if(clim.type == "Tmea") { #Tmean
              
              # Tmean Weighted Mean
              climate.predictors[climate.predictors$unique_route ==
                                   unique(climate.predictors$unique_route)[j] &
                                   climate.predictors$Year ==
                                   unique(climate.predictors$Year)[k], 6] <- 
                raster :: weighted.mean(raster :: raster(cropped.lyr), raster :: raster(x), na.rm = TRUE)
              
            }
          }
        } 
      }
      
      rm(tmp.layer, cropped.lyr)
      gc()
      
      print(paste(clim.type, ", Route ", j, ", Year", k, ": completed", sep = ""))
    } # end of the k loop
    
  } # end of US/Canada if statement
  
  print(paste("Fully Completed: Route ", j, sep = ""))
} # end of the j loop


# Save the CSV file
setwd("/gpfs1/data/idiv_meyer/01_projects/Kim_T/Analysis_Output/Climate_MayJune Averages")
write.csv(climate.predictors,
          file = paste("Weighted MayJune Clim Values_400mbuff_for each_BBS Routes_", i, ".csv", sep = ""),
          row.names = FALSE)
