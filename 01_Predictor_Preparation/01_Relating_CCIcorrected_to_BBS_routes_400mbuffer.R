##################################################################################################
########                                                                                  ########
########              Calculating proportional land cover for each BBS route              ######## 
########                    from the CCI bias-corrected land cover data                   ########
##################################################################################################

# Author: Kimberly Thompson

# This code calculates the proportion of each land cover category within each BBS route
# from the period of 1992 - 2018 using the ESA's CCI dataset which has been bias-corrected. 
# My prime motivation is to use this dataset for crop cover, though all land cover classes
# are included below.
# Resolution of cells is 300 meters.

# ****NOTE: Performed on HPC Cluster

# Produces datasets:
# 'LULC_CCI_400mbuff_BBS_1.csv'
# and on through _29.csv, with each integer corresponding to the years 1992-2020



# Categories of land cover are the following:
# 0: undefined - No data
# 10: crop.rainfed - Cropland, rainfed
# 11: herb - Herbaceous cover
# 12: tree.shrub - Tree or shrub cover
# 20: crop.irrigated - Cropland, irrigated or post-flooding
# 31: crop.mosaic1 - Mosaic cropland (>50%) / natural vegetation (tree, shrub, herbaceious cover) (<50%)
# 32: crop.mosaic2 - Mosaic cropland (>50%) / natural vegetation (tree, shrub, herbaceious cover) (<50%)
# 33: crop.mosaic3 - Mosaic cropland (>50%) / natural vegetation (tree, shrub, herbaceious cover) (<50%)
# 34: crop.mosaic4 - Mosaic cropland (>50%) / natural vegetation (tree, shrub, herbaceious cover) (<50%)
# 35: crop.mosaic5 - Mosaic cropland (>50%) / natural vegetation (tree, shrub, herbaceious cover) (<50%)
# 41: veg.mosaic1 - Mosaic natural vegetation (tree, shrub, herbaceous cover) (>50%) / cropland (<50%)
# 42: veg.mosaic2 - Mosaic natural vegetation (tree, shrub, herbaceous cover) (>50%) / cropland (<50%)
# 43: veg.mosaic3 - Mosaic natural vegetation (tree, shrub, herbaceous cover) (>50%) / cropland (<50%)
# 44: veg.mosaic4 - Mosaic natural vegetation (tree, shrub, herbaceous cover) (>50%) / cropland (<50%)
# 45: veg.mosaic5 - Mosaic natural vegetation (tree, shrub, herbaceous cover) (>50%) / cropland (<50%)
# 51: tree.b_ev_c.40 - Tree cover broadleaved evergreen closed (>40%)
# 52: tree.b_ev_o.1540 - Tree cover broadleaved evergreen open (15-40%)
# 61:	tree.b_de_c.40 - Tree cover, broadleaved, deciduous, closed (>40%)
# 62:	tree.b_de_o.1540 - Tree cover, broadleaved, deciduous, open (15-40%)
# 71:	tree.n_ev_c.40 - Tree cover, needleleaved, evergreen, closed (>40%)
# 72:	tree.n_ev_o.1540 - Tree cover, needleleaved, evergreen, open (15-40%)
# 81:	tree.n_de_c.40 - Tree cover, needleleaved, deciduous, closed (>40%)
# 82:	tree.n_de_o.1540 - Tree cover, needleleaved, deciduous, open (15-40%)
# 90:	tree.mixed - Tree cover, mixed leaf type (broadleaved and needleleaved)
# 101:	tree.mosaic1 - Mosaic tree and shrub (>50%) / herbaceous cover (<50%)
# 102:	tree.mosaic2 - Mosaic tree and shrub (>50%) / herbaceous cover (<50%)
# 103:	tree.mosaic3 - Mosaic tree and shrub (>50%) / herbaceous cover (<50%)
# 104:	tree.mosaic4 - Mosaic tree and shrub (>50%) / herbaceous cover (<50%)
# 105:	tree.mosaic5 - Mosaic tree and shrub (>50%) / herbaceous cover (<50%)
# 111:	herb.mosaic1 - Mosaic herbaceous cover (>50%) / tree and shrub (<50%)
# 112:	herb.mosaic2 - Mosaic herbaceous cover (>50%) / tree and shrub (<50%)
# 113:	herb.mosaic3 - Mosaic herbaceous cover (>50%) / tree and shrub (<50%)
# 114:	herb.mosaic4 - Mosaic herbaceous cover (>50%) / tree and shrub (<50%)
# 115:	herb.mosaic5 - Mosaic herbaceous cover (>50%) / tree and shrub (<50%)
# 120:  shrub - Shrubland
# 121:	shrub.ev - Shrubland evergreen
# 122:	shrub.de - Shrubland deciduous
# 130:	grassland - Grassland
# 140:	lichen.moss - Lichens and mosses
# 150:	veg.sparse - Sparse vegetation (tree, shrub, herbaceous cover) (<15%)
# 151:	tree.sparse - Sparse tree (<15%)
# 152:	shrub.sparse - Sparse shrub (<15%)
# 153:	herb.sparse - Sparse herbaceous cover (<15%)
# 160:	tree.freshflood - Tree cover, flooded, fresh or brakish water
# 170:	tree.saltflood - Tree cover, flooded, saline water
# 180:	herb.flood - Shrub or herbaceous cover, flooded, fresh/saline/brakish water
# 181:	herb.flood1 - Shrub or herbaceous cover, flooded, fresh/saline/brakish water
# 182:	herb.flood2 - Shrub or herbaceous cover, flooded, fresh/saline/brakish water
# 190:	urban - Urban areas
# 201:	bare.con - Consolidated bare areas
# 202:	bare.uncon - Unconsolidated bare areas
# 210:	water - Water bodies
# 220:	ice.perm - Permanent snow and ice


# Relevant classes for cropland:
# 10: Rainfed cropland
# 20: Cropland, irrigated or post-flooding
# 31: Mosaic cropland (>50%) / natural vegetation (tree, shrub, herbaceous cover) (<50%)
# 32: Mosaic cropland (>50%) / natural vegetation (tree, shrub, herbaceous cover) (<50%)
# 33: Mosaic cropland (>50%) / natural vegetation (tree, shrub, herbaceous cover) (<50%)
# 34: Mosaic cropland (>50%) / natural vegetation (tree, shrub, herbaceous cover) (<50%)
# 35: Mosaic cropland (>50%) / natural vegetation (tree, shrub, herbaceous cover) (<50%)


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
                              undefined = integer(length(bbs.routes$uniq_rt) *
                                                 length(seq(from = 1992, to = 2018, by = 1))),
                              crop.rainfed = integer(length(bbs.routes$uniq_rt) *
                                                  length(seq(from = 1992, to = 2018, by = 1))),
                              herb = integer(length(bbs.routes$uniq_rt) *
                                                  length(seq(from = 1992, to = 2018, by = 1))),
                              tree.shrub = integer(length(bbs.routes$uniq_rt) *
                                                  length(seq(from = 1992, to = 2018, by = 1))),
                              crop.irrigated = integer(length(bbs.routes$uniq_rt) *
                                                  length(seq(from = 1992, to = 2018, by = 1))),
                              crop.mosaic1 = integer(length(bbs.routes$uniq_rt) *
                                                  length(seq(from = 1992, to = 2018, by = 1))),
                              crop.mosaic2 = integer(length(bbs.routes$uniq_rt) *
                                                      length(seq(from = 1992, to = 2018, by = 1))),
                              crop.mosaic3 = integer(length(bbs.routes$uniq_rt) *
                                                      length(seq(from = 1992, to = 2018, by = 1))),
                              crop.mosaic4 = integer(length(bbs.routes$uniq_rt) *
                                                      length(seq(from = 1992, to = 2018, by = 1))),
                              crop.mosaic5 = integer(length(bbs.routes$uniq_rt) *
                                                      length(seq(from = 1992, to = 2018, by = 1))),
                              veg.mosaic1 = integer(length(bbs.routes$uniq_rt) *
                                                  length(seq(from = 1992, to = 2018, by = 1))),
                              veg.mosaic2 = integer(length(bbs.routes$uniq_rt) *
                                                     length(seq(from = 1992, to = 2018, by = 1))),
                              veg.mosaic3 = integer(length(bbs.routes$uniq_rt) *
                                                     length(seq(from = 1992, to = 2018, by = 1))),
                              veg.mosaic4 = integer(length(bbs.routes$uniq_rt) *
                                                     length(seq(from = 1992, to = 2018, by = 1))),
                              veg.mosaic5 = integer(length(bbs.routes$uniq_rt) *
                                                     length(seq(from = 1992, to = 2018, by = 1))),
                              tree.b_ev_c.40 = integer(length(bbs.routes$uniq_rt) *
                                                  length(seq(from = 1992, to = 2018, by = 1))),
                              tree.b_ev_0.1540 = integer(length(bbs.routes$uniq_rt) *
                                                  length(seq(from = 1992, to = 2018, by = 1))),
                              tree.b_de_c.40 = integer(length(bbs.routes$uniq_rt) *
                                                  length(seq(from = 1992, to = 2018, by = 1))),
                              tree.b_de_o.1540 = integer(length(bbs.routes$uniq_rt) *
                                                  length(seq(from = 1992, to = 2018, by = 1))),
                              tree.n_ev_c.40 = integer(length(bbs.routes$uniq_rt) *
                                                  length(seq(from = 1992, to = 2018, by = 1))),
                              tree.n_ev_o.1540 = integer(length(bbs.routes$uniq_rt) *
                                                length(seq(from = 1992, to = 2018, by = 1))),
                              tree.n_de_c.40 = integer(length(bbs.routes$uniq_rt) *
                                                length(seq(from = 1992, to = 2018, by = 1))),
                              tree.n_de_o.1540 = integer(length(bbs.routes$uniq_rt) *
                                                length(seq(from = 1992, to = 2018, by = 1))),
                              tree.mixed = integer(length(bbs.routes$uniq_rt) *
                                                length(seq(from = 1992, to = 2018, by = 1))),
                              tree.mosaic1 = integer(length(bbs.routes$uniq_rt) *
                                                  length(seq(from = 1992, to = 2018, by = 1))),
                              tree.mosaic2 = integer(length(bbs.routes$uniq_rt) *
                                                      length(seq(from = 1992, to = 2018, by = 1))),
                              tree.mosaic3 = integer(length(bbs.routes$uniq_rt) *
                                                      length(seq(from = 1992, to = 2018, by = 1))),
                              tree.mosaic4 = integer(length(bbs.routes$uniq_rt) *
                                                      length(seq(from = 1992, to = 2018, by = 1))),
                              tree.mosaic5 = integer(length(bbs.routes$uniq_rt) *
                                                      length(seq(from = 1992, to = 2018, by = 1))),
                              herb.mosaic1 = integer(length(bbs.routes$uniq_rt) *
                                                length(seq(from = 1992, to = 2018, by = 1))),
                              herb.mosaic2 = integer(length(bbs.routes$uniq_rt) *
                                                      length(seq(from = 1992, to = 2018, by = 1))),
                              herb.mosaic3 = integer(length(bbs.routes$uniq_rt) *
                                                      length(seq(from = 1992, to = 2018, by = 1))),
                              herb.mosaic4 = integer(length(bbs.routes$uniq_rt) *
                                                      length(seq(from = 1992, to = 2018, by = 1))),
                              herb.mosaic5 = integer(length(bbs.routes$uniq_rt) *
                                                      length(seq(from = 1992, to = 2018, by = 1))),
                              shrub = integer(length(bbs.routes$uniq_rt) *
                                                   length(seq(from = 1992, to = 2018, by = 1))),
                              shrub.ev = integer(length(bbs.routes$uniq_rt) *
                                                length(seq(from = 1992, to = 2018, by = 1))),
                              shrub.de = integer(length(bbs.routes$uniq_rt) *
                                                length(seq(from = 1992, to = 2018, by = 1))),
                              grassland = integer(length(bbs.routes$uniq_rt) *
                                                length(seq(from = 1992, to = 2018, by = 1))),
                              lichen.moss = integer(length(bbs.routes$uniq_rt) *
                                                length(seq(from = 1992, to = 2018, by = 1))),
                              veg.sparse = integer(length(bbs.routes$uniq_rt) *
                                                length(seq(from = 1992, to = 2018, by = 1))),
                              tree.sparse = integer(length(bbs.routes$uniq_rt) *
                                                length(seq(from = 1992, to = 2018, by = 1))),
                              shrub.sparse = integer(length(bbs.routes$uniq_rt) *
                                                length(seq(from = 1992, to = 2018, by = 1))),
                              herb.sparse = integer(length(bbs.routes$uniq_rt) *
                                                length(seq(from = 1992, to = 2018, by = 1))),
                              tree.freshflood = integer(length(bbs.routes$uniq_rt) *
                                                length(seq(from = 1992, to = 2018, by = 1))),
                              tree.saltflood = integer(length(bbs.routes$uniq_rt) *
                                                length(seq(from = 1992, to = 2018, by = 1))),
                              herb.flood = integer(length(bbs.routes$uniq_rt) *
                                                length(seq(from = 1992, to = 2018, by = 1))),
                              herb.flood1 = integer(length(bbs.routes$uniq_rt) *
                                                     length(seq(from = 1992, to = 2018, by = 1))),
                              herb.flood2 = integer(length(bbs.routes$uniq_rt) *
                                                     length(seq(from = 1992, to = 2018, by = 1))),
                              urban = integer(length(bbs.routes$uniq_rt) *
                                                length(seq(from = 1992, to = 2018, by = 1))),
                              bare.con = integer(length(bbs.routes$uniq_rt) *
                                                length(seq(from = 1992, to = 2018, by = 1))),
                              bare.uncon = integer(length(bbs.routes$uniq_rt) *
                                                length(seq(from = 1992, to = 2018, by = 1))),
                              water = integer(length(bbs.routes$uniq_rt) *
                                                length(seq(from = 1992, to = 2018, by = 1))),
                              ice.perm = integer(length(bbs.routes$uniq_rt) *
                                                  length(seq(from = 1992, to = 2018, by = 1))))


# Fill predictor columns with 0 values
lulc.predictors[ , c(3:55)] <- 0.00

# Make a vector of land cover class names
classes <- names(lulc.predictors)[3:55]


###############################################
###                                         ###
###        Preparing the parallel job       ###
###                                         ###
###############################################

# Parallelizing over the 27 years

lulc.path <- "/gpfs1/data/idiv_meyer/01_projects/Kim_T/Analysis_Output/CCI_Cover"

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


  
# Read in the land cover raster for the first year 
# Use raster package because it's easier to convert to categorical with it
setwd("/gpfs1/data/idiv_meyer/01_projects/Kim_T/Analysis_Output/CCI_Cover")
lulc.raster <- raster :: raster(lulc.file)

# Extract the year of the raster file for use as an index further down
year <- substr(lulc.file, 16, 19)



#######################################
#### Convert raster to categorical ####

# Ratify the raster layer
lulc.raster <- raster :: ratify(lulc.raster)

# Build the attribute table
rat <- levels(lulc.raster)[[1]]


# Some of the rasters might be missing the entire suite of level IDs so they'll have to be added
if(length(rat$ID) < (length(colnames(lulc.predictors)) - 2 )) {
  
  # make a vector of all the levels
  lev.vector <- c(0, 10, 11, 12, 20, 31, 32, 33, 34, 35, 41, 42, 43, 44, 45, 51, 52,
                  61, 62, 71, 72, 81, 82, 90,
                  101, 102, 103, 104, 105, 111, 112, 113, 114, 115,
                  120, 121, 122, 130, 140, 150, 151, 152, 153, 160, 170, 180, 181, 182,
                  190, 201, 202, 210, 220)
  
  # Find the values that are not represented
  missing.values <- setdiff(lev.vector, rat$ID)
  
  # Add in the missing levels
  for(j in 1:length(setdiff(lev.vector, rat$ID))) {
    
    # Add in the next unfilled row and 1st column
    rat[(length(rat$ID) + 1) , 1 ] <-  missing.values[j]
    
  } # end of j loop
  
  # Sort the ID column so that the attribute names will line up correctly
  rat$ID <- sort(rat$ID)
  
} # end of if statement


rat$Class_Names <- classes

# Define the levels of the raster variable
levels(lulc.raster) <- rat

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
    
    # Creates a raster of the linestring where the values of the cells is the length in each cell
    x <- terra :: rasterizeGeom(tmp.route, cropped.lyr, "count")
    
    # Calculate the total length of the route / number of cells in the buffer
    # (Sum function not working in terra, so have to use raster package here)
    total <- raster :: cellStats(raster :: raster(x), stat = 'sum')
    
    # Divide the values of the spatraster by the total length of the route to get a proportion for each cell
    x <- x / total
    
    ###################################################
    
    
    ########################################
    ### Calculate the proportional cover ###
    
    # Extract the land cover value of each raster cell the BBS route touches
    tmp.df <- terra :: extract(cropped.lyr, tmp.route, method = "simple", factors = TRUE,
                               touches = TRUE)
    
    # The below if block is a remnant from other extraction scripts, for this one it is way more 
    # complicated than it needs to be since every value in the proportion column will be the same.
    # Add a column to the temporary df for the proportional cover of the route in each raster cell
    # Include if statement for the cases where the length of extracted values does not match the
    # length of the raster values that are greater than 0
    if(length(tmp.df$ID) == length(values(x)[values(x) > 0])) { 
      
      tmp.df$Proportion <- values(x)[values(x) > 0]
      
    } else {
      
      if(length(tmp.df$ID) > length(values(x)[values(x) > 0])) {
        
        # if the number of extracted values is higher than the raster cells with proportion values
        # repeat a 0 value for proportion for as many times as necessary to have equal column lengths
        tmp.df$Proportion <- c(values(x)[values(x) > 0],
                               rep(0, length(tmp.df$ID) - length(values(x)[values(x) > 0])))
        
      } else {
        
        if(length(values(x)[values(x) > 0]) > length(tmp.df$ID)) {
          
          # if the number of  raster cells with proportion values is higher than the number of extracted values
          # repeat a 0 value for proportion for as many times as necessary to have equal column lengths
          tmp.df$Proportion <- values(x)[values(x) > 0][1:length(tmp.df$ID)]
          
        } # end of if statement
      } # end of if statement
    } # end of if statement
    
    
    # Change the name of column 2 to make it easier to group by (and iterate)
    names(tmp.df)[2] <- "Cover"
    
    # Sum the # of cells of each cover type
    tmp.df_sum <- tmp.df %>%
      dplyr :: group_by(Cover) %>%
      dplyr :: summarise(Sum = sum(Proportion))
    
    # This sum column is really the proportional cover of each class in the buffer.
    
    ########################################
    
    
    #############################################################################
    ### Add the proportional cover for each cover type to the blank dataframe ###
    
    for (k in 1:length(tmp.df_sum$Cover)) { # for each cover type represented in the route
      
      # Convert the numeric land cover code into the named land cover category
      cov.class <- rat$Class_Names[rat$ID == tmp.df_sum$Cover[k]]
      
      for (m in 3:length(colnames(lulc.predictors))) { # each land cover column
        
        if(colnames(lulc.predictors)[m] == cov.class) {
          
          lulc.predictors[lulc.predictors$Year == year & 
                            lulc.predictors$unique_route == unique(lulc.predictors$unique_route)[j], m] <-
            tmp.df_sum$Sum[k]
          
        } # end of if statement for filling lulc.predictors dataframe
        
      } # end of the m loop
    } # end of the k loop
    
    # Clean up workspace
    rm(tmp.route, cropped.lyr, x, total, tmp.df, tmp.df_sum)
    gc()
    
  } # end of if statement for assessing only contiguous USA
  
  print(paste("Year: ", year, "- Route: ", j, "- Completed"))
  
} # end of j loop

print("I have finished running all the routes and now I will write the file")

# Save the file
setwd("/gpfs1/data/idiv_meyer/01_projects/Kim_T/Analysis_Output/Land_Cover_States_CCI")

write.csv(lulc.predictors, file = paste("LULC_CCI_400mbuff_BBS_", i, ".csv", sep = ""), row.names = FALSE)

print("I have successfully written the file")

