##################################################################################################
########                                                                                  ########
########              Calculating Rarefied Richness with mobr for BBS Routes              ######## 
########                                                                                  ########
##################################################################################################

# Author: Kimberly Thompson

# This code calculates individual-based rarefied richness (i.e. so rarefied to a particular number
# of individuals) for each year and route in the North American Breeding
# Bird Survey Dataset (subsetted to routes with at least 20 years of surveys).
# But only for the period of 1992 to 2018.
# Some route-year combinations were not surveyed,
# so those will be missing values (NAs will have to be added).

# ****NOTE: Performed on HPC Cluster

# Produces datasets:
# 'Individual Based Rarefied Richness_1992.csv'
# and on through 2018


# mobr vignette
# https://github.com/MoBiodiv/mobr/blob/master/vignettes/mobr_intro.pdf


########## clean workspace and load required packages ####################

# clean workspace to improve efficiency: #
# rm(list = ls() ) 
# gc() #releases memory


library(mobr) # rarefaction
library(ggplot2) # Plotting
library(sf)


library(tidyverse) # Data organization
library(lubridate) # Time values
library(data.table) # Data organization


###############################################
###                                         ###
###         Some notes about mobr           ###
###                                         ###
###############################################

# 2 matrix-like tables are required:
# first, species abundance data in a community matrix (rows are sites and columns are species)
# second, a site attribute table (rows are sites and columns are site attributes).

# Site table should include spatial coordinates



###############################################
###                                         ###
###        Preparing the parallel job       ###
###                                         ###
###############################################

# Parallelizing over the 27 years - i loop below is run through the HPC cluster

# List the site x species tables (one for each year)
dfs <- list.files("/gpfs1/data/idiv_meyer/01_projects/Kim_T/BBS_Data/Site_Species_Matrices")


# Define the array (aka climate variable) ID
i = as.integer(Sys.getenv('SLURM_ARRAY_TASK_ID'))

year.file <- dfs[i]



###############################################
###                                         ###
###     Calculating individual based        ###
###          rarefied richness              ###
###############################################

# Create a blank dataframe to store the results
rare.final <- data.frame(unique_route = character(), Year = integer(),
                         Individuals = integer(), Richness = numeric())
                         

# for (i in 1:length(dfs)) {
  
  ####################################
  #  Creating Site x species matrix  #
  ####################################
  
  # Read in the site x species table
  # setwd("~/share/groups/MAS/01_projects/iUpdate/00_Data/processed/BBS Data/Site x Species Matrices")
  # setwd("I:/MAS/01_projects/iUpdate/00_Data/processed/BBS Data/Site x Species Matrices")
  setwd("/gpfs1/data/idiv_meyer/01_projects/Kim_T/BBS_Data/Site_Species_Matrices")
  sp.data <- read.csv(year.file, header = TRUE)
  
  # Extract the year
  year <- substr(year.file, 23, 26)
  
  
  ### Convert sp.data to a matrix ###
  
  # Define row names as the unique routes
  row.names(sp.data) <- sp.data[ , 1]
  
  # Remove the first columns (uniq route) from the df
  sp.data <- sp.data[ , -1]
  
  # convert to a matrix
  # sp.data <- data.matrix(sp.data, rownames.force = TRUE)
  
  
  ######################################
  #   Calculating Rarefied Richness    #
  ######################################
  
  for (j in 1:nrow(sp.data)) {
    
    # Subset data to be one route/site
    sp.tmp <- sp.data[j, ]
    
    rare.tmp <- as.data.frame(rarefaction(sp.tmp, method = 'IBR'))
    
    names(rare.tmp) <- "Richness"
    
    rare.tmp$Individuals <- seq(1, length(rare.tmp$Richness), by = 1)
    
    rare.tmp$unique_route <- row.names(sp.data)[j]
    
    rare.tmp$Year <- year
    
    rare.final <- rbind(rare.final, rare.tmp)
    
    print(j)
    
  } # end of j loop
  
  # print(paste("Year ", i, " completed", sep = ""))
  
# } # end of i loop
  



# Save resulting dataframe
setwd("/gpfs1/data/idiv_meyer/01_projects/Kim_T/Analysis_Output/Rarefied_Richness")
write.csv(rare.final, paste("Individual Based Rarefied Richness_", year, ".csv", sep = ""),
          row.names = FALSE)

print(paste("Year ", i, " successfully completed", sep = ""))
