##################################################################################################
########                                                                                  ########
########                   Merging output of climate average calculations                 ######## 
########                        for each BBS route from PRISM data                        ########
##################################################################################################

# Author: Kimberly Thompson

# This code takes output run on the HPC cluster which 
# calculated average climate (tmin, tmax, tmean, and precip) within each BBS route
# for May and June from the period of 1966 - 2020 using PRISM data 
# and merges them into a single csv file. (only 1992-2018 used in analysis)

# One file for every climate variable (4 files) --> 1 file

# Produces dataset:
# 'Clim Raw Values for each BBS Route PRISM_400m.csv'


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

# List the files that show climate for each route
clim.path <- "01_Analysis/Predictor_Preparation/Climate Raw Values from EVE"

eve.files <- list.files(clim.path, pattern = "buff")


# indices of files are in alphabetical order of climate variable
# precip = 1
# tmax = 2
# tmean = 3
# tmin = 4


###############################################
###                                         ###
###         Set up blank dataframe          ###
###                                         ###
###############################################

# Read in one file to use as the base dataframe
setwd(clim.path)
test <- read.csv(eve.files[1])

# Set up blank dataframe
climate_400 <- test[ , c(1:2)]



###############################################
###                                         ###
###       Add relevant columns to df        ###
###                                         ###
###############################################

for (i in 1:length(eve.files)) {
  
  # Define file name
  fname <- eve.files[i]
  
  # Extract the buffer
  buffer <- substr(fname, 30, 33)
  
  # Extract the index (denotes the climate variable)
  index <- substr(fname, 59, 59)
  
  # Read in the file
  test <- read.csv(eve.files[i])
  
  # Add the appropriate climate value column to the appropriate dataframe

  
  if(buffer == "400m" & index == 1) {
    
    climate_400 <- cbind(climate_400, test$ppt.wtmean)
    
  } else {
    
    if(buffer == "400m" & index == 2) {
      
      climate_400 <- cbind(climate_400, test$tmax.wtmean)
      
    } else {
      
      if(buffer == "400m" & index == 3) {
        
        climate_400 <- cbind(climate_400, test$tmean.wtmean)
        
      } else {
        
        if(buffer == "400m" & index == 4) {
          
          climate_400 <- cbind(climate_400, test$tmin.wtmean)
        }
      }
    }
  }
  
  print (i)
} # end of i loop
          
          

# Rename the columns
names(climate_400)[3:6] <-
  c("ppt.wtmean_raw_mo", "tmax.wtmean_raw_mo", "tmean.wtmean_raw_mo", "tmin.wtmean_raw_mo")



###############################################
###                                         ###
###            Save the merged CSVs         ###
###                                         ###
###############################################

path <- "01_Analysis/Predictor_Preparation/Merged Climate Raw Values from PRISM/"
write.csv(climate_400, paste(path, "Clim Raw Values for each BBS Route PRISM_400m.csv", sep = ""),
          row.names = FALSE)

