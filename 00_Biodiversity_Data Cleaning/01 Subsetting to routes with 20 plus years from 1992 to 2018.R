#############################################################################################
########                                                                             ########
########                    Subsetting BBS routes to 20+ years                       ######## 
########                    for the time period of 1992 to 2018                      ########
#############################################################################################

# Author: Kimberly Thompson

# This code subsets the complete North American Breeding Bird Survey Dataset (1966-2019), 
# to those routes with 20+ years of surveys for the time period of 1992 - 2018.

# This is because some of the land cover datasets I want to use as predictors only begin 
# monitoring from 1992. 

# Produces datasets:
# 'BBS Observations with 20 years plus from 1992 to 2018.csv'


########## clean workspace and load required packages ####################

# clean workspace to improve efficiency: #
# rm(list = ls() )
# gc() #releases memory


library(tidyverse) # Data cleaning



###############################################
###                                         ###
###             Data Loading                ###
###                                         ###
###############################################

# Load all BBS observations
path <- "00_Data/raw/BBS Observations/State Level Data/"
allobs <- read.csv(paste(path, "All BBS Observations 1966_2019.csv", sep = ""), header = TRUE)



###############################################
###                                         ###
### Subset by which routes have at least    ###
###     20 years of data during the         ###
###         period of 1992-2018             ###
###############################################

# Subset data based on year range
allobs_92on <- allobs[allobs$Year >= 1992 & allobs$Year <= 2018, ]

# Make a vector of unique route numbers
route.num <- unique(allobs_92on$unique_route)

# Create a data frame in which to store results
total.years <- data.frame(Route = character(), Total = integer())

# Loop to calculate the number of years for the routes
for (i in 1:length(route.num)) {
  
  # find the unique years for each unique route
  years <- length(unique(allobs_92on$Year[allobs_92on$unique_route == route.num[i]]))
  
  # Create a dataframe for consecutive years
  tmp.df <- data.frame(Route = route.num[i], Total = years)
  
  # Rbind to the total.years dataframe
  total.years <- rbind(total.years, tmp.df)
  
  print(i)
  
}

# Filter the routes in total years by those that have more than 20 yrs
total.20 <- total.years %>%
  subset(Total >= 20)

# Create a vector of routes to retain
routes_to_retain <- total.20$Route

# Create a dataframe to store subsetted observations (same column names as originals)
all.obs_sub20 <- allobs[0, ]

# Subset the observation dataframe to include only these routes
for (i in 1:length(routes_to_retain)) {
  
  # Temporary dataframe for the observations
  tmp <- allobs_92on %>%
    subset(allobs_92on$unique_route == routes_to_retain[i])
  
  # Bind observations together
  all.obs_sub20 <- rbind(all.obs_sub20, tmp)
  
  print(i)
  
}

# Left with 2063 unique state/route combinations
# 1809 are in the USA

# Write the subsetted dataframe
path2 <- "00_Data/processed/BBS Data/"
write.csv(all.obs_sub20, paste(path2, "BBS Observations with 20 years plus from 1992 to 2018.csv", sep = ""),
          row.names = FALSE)

