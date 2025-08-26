#############################################################################################
########                                                                             ########
########                  Creating Site x Species Matrix with BBS Data               ######## 
########                                                                             ########
#############################################################################################

# Author: Kimberly Thompson

# This code converts the North American Breeding Bird Survey Dataset, subsetted to the
# routes that have at least 20 consecutive years of data for the period of 1992 - 2018
# into a site by species matrix. - ONE FOR EACH YEAR

# The data can be further subset by state.

# Produces datasets:
# 'Site x Species Matrix_1992.csv'
# and on until 2018


########## clean workspace and load required packages ####################

# clean workspace to improve efficiency: #
# rm(list = ls() ) 
# gc() #releases memory


library(tidyverse) # Data cleaning
library(reshape2) # Data cleaning
library(lubridate) # Time values
library(R.utils) # time intervals



###############################################
###                                         ###
###      Load the observation data          ###
###                                         ###
###############################################

# Load the raw survey data to obtain the routes to retain
path <- "00_Data/processed/BBS Data/"
raw <- read.csv(paste(path, "BBS Observations with 20 years plus from 1992 to 2018.csv", sep = ""), header = TRUE)


###############################################
###                                         ###
###           Data Manipulation             ###
###                                         ###
###############################################

# Pare df to only unique route, AOU (species), and Species Total, and Year
raw <- raw[ , c("unique_route", "AOU", "SpeciesTotal", "Year")]



###############################################
###                                         ###
###      Create a site x species matrix     ###
###           for each year                 ###
###############################################

for (i in 1:length(unique(raw$Year))) {
  
  # Filter by year
  sub.year <- raw %>%
    filter(Year == unique(raw$Year)[i])
  
  # Remove duplicated routes if there are any (there are some for some years and they mess up the
  # transposition)
  # if the vector of duplicated T/F is not all false (meaning sum of trues = 0)
  if(sum(duplicated(sub.year[, 1:2])) != 0) {
    sub.year <- sub.year[-which(duplicated(sub.year[, 1:2])), ]
  } 
  
  # Transpose the species into columns
  # The first argument is the dataframe. The second is a formula: on the left hand side are the 
  # factors you want to include and the right hand side is the factor you want to 'transpose', 
  # or convert into columns. 
  sub.matrix <- reshape2 :: dcast(sub.year, unique_route ~ AOU, value.var = 'SpeciesTotal',
                                  fun.aggregate = NULL)
  
  # Replace any NA values for species abundances with 0s
  sub.matrix[is.na(sub.matrix)] <- 0
  
  # Since AOU IDs are numbers, change the column names to have a leading X to make them 
  # readable in R
  for (j in 2:ncol(sub.matrix)) { # don't change column 1 which is unique_route
    names(sub.matrix)[j] <- paste("X", names(sub.matrix)[j], sep = "")
    print(j)
  }

  
  # Save this dataframe
  path2 <- "00_Data/processed/BBS Data/Site x Species Matrices/"
  write.csv(sub.matrix, paste(path2, paste("Site x Species Matrix_", unique(raw$Year)[i], ".csv", sep = ""), sep = ""),
            row.names = FALSE)
  
  print(i)
  
}








