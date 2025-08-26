#############################################################################################
########                                                                             ########
########               Downloading and Cleaning BBS Data                             ######## 
########                                                                             ########
#############################################################################################

# Author: Kimberly Thompson

# This code downloads the complete North American Breeding Bird Survey Dataset (1966-2019), 
# subsets the data to those routes with 20+ years of surveys, and does initial 
# cleaning and preprocessing.

# Produces datasets:
# 'All BBS Observations 1966_2019.csv'
# 'All BBS Routes 1966_2019.csv'
# 'BBS Species List 1966_2019.csv'



# Website for data download
# https://www.sciencebase.gov/catalog/item/5ea04e9a82cefae35a129d65


########## clean workspace and load required packages ####################

# clean workspace to improve efficiency: #
# rm(list = ls() )
# gc() #releases memory

### Only needs to be done once ###
# Install BBS Assistant package to aid in retrieving data
library(devtools)
install_github("trashbirdecology/bbsAssistant")

library(tidyverse) # Data cleaning
library(reshape2) # Data cleaning
library(bbsBayes) # Data downloading
library(R.utils) # Data cleaning (seqToIntervals function)


###############################################
###                                         ###
### Retrieve the dataset using bbsBayes     ###
###                                         ###
###############################################

### Only needs to be done once, since it will save the dataset in my local directory
bbsBayes :: fetch_bbs_data(level = "state")

# Load the data
bbs.data <- bbsBayes :: load_bbs_data(level = "state")

# Result is a list of 3 elements: bird, route, and species

# Create dataframes of the list elements
bbs.obs <- as.data.frame(bbs.data$bird)

bbs.routes <- as.data.frame(bbs.data$route)

bbs.species <- as.data.frame(bbs.data$species)

# Remove the list
rm(bbs.data)


###############################################
###                                         ###
###    Initial Exploration and Cleaning     ###
###                                         ###
###############################################

# Examine the names of each df
# There are ~ 5000 routes so we are looking for columns that have that number of
# unique values
names(bbs.obs)

length(unique(bbs.obs$Route)) #470
length(unique(bbs.obs$RouteDataID)) #124391

# Route is only unique within each state but a combination of the state number
# and route number would be unique across the continent

# RouteDataID is unique for each country num, state num, route, and year combination

# Define both statenum and Route as character columns so that trailing zeros
# are retained
bbs.obs$statenum <- as.character(bbs.obs$statenum)
bbs.obs$Route <- as.character(bbs.obs$Route)

# Create a new column with the state and route combo
bbs.obs$unique_route <- paste(bbs.obs$statenum, bbs.obs$Route, sep = "_")

length(unique(bbs.obs$unique_route)) #5197



names(bbs.routes)


# Create a new column with the state and route combo
bbs.routes$unique_route <- paste(bbs.routes$statenum, bbs.routes$Route, sep = "_")

length(unique(bbs.routes$unique_route)) #5197

names(bbs.species)


# Save the dataframes
path <- "00_Data/raw/BBS Observations/State Level Data/"
write.csv(bbs.obs, paste(path, "All BBS Observations 1966_2019.csv", sep = ""), row.names = FALSE)
write.csv(bbs.routes, paste(path, "All BBS Routes 1966_2019.csv", sep = ""), row.names = FALSE)
write.csv(bbs.species, paste(path, "BBS Species List 1966_2019.csv", sep = ""), row.names = FALSE)











