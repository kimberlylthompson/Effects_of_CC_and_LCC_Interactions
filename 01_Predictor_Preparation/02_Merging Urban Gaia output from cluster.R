##################################################################################################
########                                                                                  ########
########                    Merging output of urban cover calculations                    ######## 
########               for each BBS route from Gaia impervious cover data                 ########
##################################################################################################

# Author: Kimberly Thompson

# This code takes output run on the HPC cluster which 
# calculated the percentage impervious cover within each BBS route
# from the period of 1992 - 2018 using the GAIA dataset 
# and merges them into a single csv file.

# One file for every year:
#  = 27 years --> 1 file


# Produces dataset:
# 'Urban Percentage for each BBS Route GAIA_400m.csv'




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

# List the files that show percentage canopy cover for each route
lulc.path <- "01_Analysis/Predictor_Preparation/Urban Coverage from EVE"

eve.files <- sort(list.files(lulc.path, pattern = "400m"))



# Set up the key for years and EVE file indices
key <- data.frame(Year = seq(1992, 2018, by = 1),
                  eve = seq(1, 27, by = 1))


###############################################
###                                         ###
###         Set up blank dataframe          ###
###                                         ###
###############################################

lulc.predictors_400 <- data.frame(Year = integer(),
                                  unique_route = character(),
                                  Mean = numeric(),
                                  Median = numeric(),
                                  Min = numeric(),
                                  Max = numeric())




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
  index <- substr(eve.files[i], 20, 21)
  
  # For single digits this will introduce a digit followed by a period, so get just the number
  index <- as.numeric(gsub("([0-9]+).*$", "\\1", index))
  
  # Pare the file down to the one year it contains
  test <- test[test$Year == key$Year[key$eve == index], ]
  
  # Add the pared file to the blank dataframe
  lulc.predictors_400 <- rbind(lulc.predictors_400, test)
   
  
  print(i)
  
}



###############################################
###                                         ###
###            Save the merged CSVs         ###
###                                         ###
###############################################

path <- "01_Analysis/Predictor_Preparation/Merged Urban Cover Percentages GAIA/"
write.csv(lulc.predictors_400,
          paste(path, "Urban Percentage for each BBS Route GAIA_400m.csv", sep = ""),
          row.names = FALSE)






