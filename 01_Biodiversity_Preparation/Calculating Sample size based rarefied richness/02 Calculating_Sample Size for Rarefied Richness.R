##################################################################################################
########                                                                                  ########
########              Calculating Rarefied Richness with mobr for BBS Routes              ######## 
########                                                                                  ########
##################################################################################################

# Author: Kimberly Thompson

# After calculating rarefied richness for a range of sample sizes, I have to determine which 
# sample size to use in the analyses. General practice is to use 2 x the minimum abundance value
# across sites. So I need to find the minimum abundance value across all sites and all years.

# This covers the period of 1992 - 2018.

# Produces dataset:
# 'Minimum Individs across sites and years.csv'



########## clean workspace and load required packages ####################

# clean workspace to improve efficiency: #
# rm(list = ls() ) 
# gc() #releases memory


library(tidyverse) # Data organization
library(lubridate) # Time values
library(data.table) # Data organization




###############################################
###                                         ###
###     Calculating individual based        ###
###          rarefied richness              ###
###############################################

# Create a blank dataframe to minimum abundance for each year
ssize.final <- data.frame(unique_route = character(27), Year = integer(27),
                         Min.Individuals = integer(27))

# List the site x species tables (one for each year)
path <- "00_Data/processed/BBS Data/Site x Species Matrices/"
dfs <- list.files(path)

                         

for (i in 1:length(dfs)) {
  
  year.file <- dfs[i]
  
  # Read in the site x species table
  sp.data <- read.csv(paste(path, year.file, sep = ""), header = TRUE)
  
  # Extract the year
  year <- substr(year.file, 23, 26)
  
  # Find the total number of individuals at each route(aka site)
  sp.data$total <- rowSums(sp.data[, 2:length(sp.data[1, ])])
  
  # Find the minimum value of the total column and add it to the summary dataframe
  ssize.final$Min.Individuals[i] <- min(sp.data$total)
  ssize.final$unique_route[i] <-
    sp.data$unique_route[sp.data$total == ssize.final$Min.Individuals[i]]
  ssize.final$Year[i] <- year
  
  print(i)
  
} # end of i loop


# Find the minimum number of individuals across all years
min(ssize.final$Min.Individuals)
# 30


# Save resulting dataframe
path2 <- "01_Analysis/Biodiversity_Preparation/Biodiversity Base Metrics/"
write.csv(ssize.final, paste(path2, "Minimum Individs across sites and years.csv", sep = ""),
          row.names = FALSE)

