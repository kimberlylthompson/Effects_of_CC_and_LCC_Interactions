#############################################################################################
########                                                                             ########
########            Creating Community Abundance Dataframes from BBS State           ######## 
########                         Level Data i.e. Abundance Data                      ########
#############################################################################################

# Author: Kimberly Thompson

# This code processes the state-level data from the
# North American Breeding Bird Survey Dataset, which has already been subset to routes containing
# at least 20 years of data in a previous script. Years covered are 1992 to 2018.
# These are counts of each species over 50 stops (aggregated into 5 bins). - AOU = species code, 
# and Species Total is the total number of individuals observed on that route for that year.

# Goal is to create a dataframe that aggregates the species to community-level total abundances.

# Produces dataset:
# 'Community Level Abundances for routes with 20 plus years from 1992 to 2018.csv'


# Website for data download
# https://www.sciencebase.gov/catalog/item/5ea04e9a82cefae35a129d65




########## clean workspace and load required packages ####################

# clean workspace to improve efficiency: #
# rm(list = ls() )
# gc() #releases memory


library(tidyverse) # Data cleaning
library(reshape2) # Data cleaning
library(R.utils) # Data cleaning (seqToIntervals function)




###############################################
###                                         ###
###              Data Loading               ###
###                                         ###
###############################################

# Load in the observation data
path <- "00_Data/processed/BBS Data/"
bbs.obs <- read.csv(paste(path, "BBS Observations with 20 years plus from 1992 to 2018.csv", sep = ""), header = TRUE)


###############################################
###                                         ###
###         Community-Level Abundances      ###
###                                         ###
###############################################


# Remove unnecessary columns
comm.abundance <- bbs.obs[ , -c(4:5, 8:13)]


# Group by unique route and year, summarize the abundances 
community <- comm.abundance %>%
  dplyr :: group_by(unique_route, Year) %>%
  mutate(Comm.Abundance = sum(SpeciesTotal))

# Adds a column with the community abundances so now collapse this down

# Remove the AOU and Species abundance columns
community <- community[ , -c(5:6)]

# Retain only unique rows
community <- community %>%
  dplyr :: distinct()

community <- arrange(community, unique_route, Year)


# Save the community level abundances (by route and year)
write.csv(community, paste(path, "Community Level Abundances for routes with 20 plus years from 1992 to 2018.csv",
                           sep = ""), row.names = FALSE)
