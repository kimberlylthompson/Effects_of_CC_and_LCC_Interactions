##################################################################################################
########                                                                                  ########
########           Merging output of permanent and seasonal water calculations            ######## 
########                for each BBS route from iGFC Canopy cover data                    ########
##################################################################################################

# Author: Kimberly Thompson

# This code takes output run on the cluster which 
# calculated the percentage cover of permanent and seasonal water within each BBS route
# from the period of 1984 - 2020 using the GSW dataset 
# and merges them into a single csv file.

# One file for every year and 2 different types of files:
# permanent and seasonal = 37 years x 2 types --> 1 file

# Produces dataset:
# 'Surface Water Percentage for each BBS Route GSW_400m.csv'



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

# List the files that show percentage water cover for each route
lulc.path <- "01_Analysis/Predictor_Preparation/Surface Water from EVE"

eve.files <- sort(list.files(lulc.path, pattern = "400m"))


# Set up the key for years and EVE file indices
key <- data.frame(Year = rep(seq(1984, 2020, by = 1), 2),
                  eve = seq(1, 74, by = 1),
                  Type = c(rep("permanent", 37), rep("seasonal", 37)))


###############################################
###                                         ###
###         Set up blank dataframe          ###
###                                         ###
###############################################

lulc.predictors_400 <- data.frame(Year = integer(),
                                  unique_route = character(),
                                  Mean = numeric(),
                                  Type = character())



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
  index <- substr(eve.files[i], 28, 29)
  
  # For single digits this will introduce a digit followed by a period, so get just the number
  index <- as.numeric(gsub("([0-9]+).*$", "\\1", index))
  
  # Pare the file down to the one year it contains
  test <- test[test$Year == key$Year[key$eve == index], ]
  
  # Add the type column
  if(index >= 1 & index <= 37) {
    test$Type <- "permanent"
  } else {
    if(index > 37 & index <= 74) {
      test$Type <- "seasonal"
    }
  }
  
  # Add the pared file to the blank dataframe
  lulc.predictors_400 <- rbind(lulc.predictors_400, test)
  
  
  print(i)
  
}
  



###############################################
###                                         ###
###            Save the merged CSVs         ###
###                                         ###
###############################################

path <- "01_Analysis/Predictor_Preparation/Merged Surface Water Percentages GSW/"

write.csv(lulc.predictors_400,
          paste(path, "Surface Water Percentage for each BBS Route GSW_400m.csv", sep = ""),
          row.names = FALSE)






