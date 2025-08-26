##################################################################################################
########                                                                                  ########
########              Merging output of proportional land cover calculations              ######## 
########            for each BBS route from the CCI corrected land cover data             ########
##################################################################################################

# Author: Kimberly Thompson

# This code take output run on the HPC cluster which 
# calculated the proportion of each land cover category within each BBS route
# from the period of 1992 - 2020 using the CCI corrected land cover dataset 
# and merges them into a single csv file.

# One file for every year = 27 files --> 1 file

# Produces dataset:
# 'LC Proportions for each BBS Route CCI Corrected_400m buffer.csv'



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
library(tidyverse)


###############################################
###                                         ###
###             Data Loading                ###
###                                         ###
###############################################

# List the files that show proportional land cover for each route
lulc.path <- "01_Analysis/Predictor_Preparation/Land Cover CCI from EVE"

eve.files <- list.files(lulc.path, pattern = "buff")


# Set up the key for years and EVE file indices
key <- data.frame(Year = seq(1992, 2018, by = 1),
                  eve = seq(1, 27, by = 1))


###############################################
###                                         ###
###         Set up blank dataframe          ###
###                                         ###
###############################################

lulc.predictors_400m <- data.frame(Year = integer(),
                              unique_route = character(),
                              undefined = numeric(),
                              crop.rainfed = numeric(),
                              herb = numeric(),
                              tree.shrub = numeric(),
                              crop.irrigated = numeric(),
                              crop.mosaic1 = numeric(),
                              crop.mosaic2 = numeric(),
                              crop.mosaic3 = numeric(),
                              crop.mosaic4 = numeric(),
                              crop.mosaic5 = numeric(),
                              veg.mosaic1 = numeric(),
                              veg.mosaic2 = numeric(),
                              veg.mosaic3 = numeric(),
                              veg.mosaic4 = numeric(),
                              veg.mosaic5 = numeric(),
                              tree.b_ev_c.40 = numeric(),
                              tree.b_ev_0.1540 = numeric(),
                              tree.b_de_c.40 = numeric(),
                              tree.b_de_o.1540 = numeric(),
                              tree.n_ev_c.40 = numeric(),
                              tree.n_ev_o.1540 = numeric(),
                              tree.n_de_c.40 = numeric(),
                              tree.n_de_o.1540 = numeric(),
                              tree.mixed = numeric(),
                              tree.mosaic1 = numeric(),
                              tree.mosaic2 = numeric(),
                              tree.mosaic3 = numeric(),
                              tree.mosaic4 = numeric(),
                              tree.mosaic5 = numeric(),
                              herb.mosaic1 = numeric(),
                              herb.mosaic2 = numeric(),
                              herb.mosaic3 = numeric(),
                              herb.mosaic4 = numeric(),
                              herb.mosaic5 = numeric(),
                              shrub = numeric(),
                              shrub.ev = numeric(),
                              shrub.de = numeric(),
                              grassland = numeric(),
                              lichen.moss = numeric(),
                              veg.sparse = numeric(),
                              tree.sparse = numeric(),
                              shrub.sparse = numeric(),
                              herb.sparse = numeric(),
                              tree.freshflood = numeric(),
                              tree.saltflood = numeric(),
                              herb.flood = numeric(),
                              herb.flood1 = numeric(),
                              herb.flood2 = numeric(),
                              urban = numeric(),
                              bare.con = numeric(),
                              bare.uncon = numeric(),
                              water = numeric(),
                              ice.perm = integer())


###############################################
###                                         ###
###       Pare each csv and then merge      ###
###                                         ###
###############################################


for (i in 1:length(eve.files)) {
  
  # Read in the year file
  setwd(lulc.path)
  test <- read.csv(eve.files[i], header = TRUE)
  
  # the eve.files are not in chronological order so need to extract the number and then
  # match it in the key
  index <- substr(eve.files[i], 23, 24)
  
  # For single digits this will introduce a digit followed by a period, so get just the number
  index <- as.numeric(gsub("([0-9]+).*$", "\\1", index))
  
  # Pare the file down to the one year it contains
  test <- test[test$Year == key$Year[key$eve == index], ]
  
  # Extract the buffer distance
  buffer <- substr(eve.files[i], 10, 13)
  
  # Add the pared file to the appropriate blank dataframe
  lulc.predictors_400m <- rbind(lulc.predictors_400m, test)

  
  print(i)
  
} # end of i loop



###############################################
###                                         ###
###            Save the merged CSVs         ###
###                                         ###
###############################################

path <- "01_Analysis/Predictor_Preparation/Merged Land Cover Proportions CCI/"
write.csv(lulc.predictors_400m,
          paste(path, "LC Proportions for each BBS Route CCI Corrected_400m buffer.csv", sep = ""),
          row.names = FALSE)





