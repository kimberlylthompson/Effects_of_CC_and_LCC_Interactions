##################################################################################################
########                                                                                  ########
########              Creating master dataframes for responses and predictors             ######## 
########                         calculated from gls with AR1                             ########
##################################################################################################

# Author: Kimberly Thompson

# This script creates a master dataframe of slopes for all responses and predictors calculated
# for the period of 1992 to 2018 (the limiting factor for some predictors).

# Predictor slopes calculated with 400 meter buffer.

# All slopes calculated using nlme :: gls (generalized linear regression) with an AR1 
# correlation structure,

# Produces dataset:
# 'GLS Master_Trends of Responses and Predictors_1992 to 2018.csv'


##################################################
# clean workspace to improve efficiency: #
# rm(list = ls() ) 
# gc() #releases memory


library(tidyverse) # Data organization
library(sf)
library(sp)



###############################################
###                                         ###
###             Data Loading                ###
###                                         ###
###############################################

# Load the biodiversity responses
path <- "01_Analysis/Biodiversity_Preparation/Biodiversity Trends/"
trunc.biodiv <- read.csv(paste(path,
                               "Trends_Biodiversity Metrics_Frequentist_GLS 1992 to 2018.csv",
                               sep = ""),
                         header = TRUE)


# Load the climate and land cover predictors
path2 <- "01_Analysis/Predictor_Preparation/Trends in Predictors/"
trunc.pred400m <- read.csv(paste(path2,
                                 "Trends_Climate and Land Cover Predictors_Frequentist_GLS 1992 to 2018_400m.csv",
                                 sep = ""),
                           header = TRUE)


# Load the bbs routes linestring file 
path3 <- "00_Data/processed/BBS Data/"
bbs.routes <- sf :: st_read(paste(path3, "BBS Rte 20 yrs_Linestring.shp", sep = ""))

# Pare down file to only be unique route, geometry, country and state columns
bbs.routes <- bbs.routes[ , c("Country", "statenm", "St_Abrv", "uniq_rt", "geometry")]

# Define the coordinate system
albers = sp :: CRS("epsg:5070")

# Make sure bbs.routes are in albers
bbs.routes <- sf :: st_transform(bbs.routes, crs = albers)

# Filter bbs routes to show usa routes only
bbs.routes2 <- bbs.routes[bbs.routes$Country != "CA" & bbs.routes$St_Abrv != "AK", ]


###############################################
###                                         ###
###           Data Preparation              ###
###                                         ###
###############################################

# Retain only the identifying columns, slope, std err, type, and sample.length columns
# For biodiv this is statenum, Route, countrynum, and unique_route
# For predictors this is just unique_route
trunc.biodiv <- trunc.biodiv[ , c(1:6, 12)]
trunc.pred400m <- trunc.pred400m[ , c(1:3, 9)]

# Expand each to wide form
trunc.biodiv <- trunc.biodiv %>%
  pivot_wider(names_from = Type, values_from = c(SLOPE, SLOPE.STDERR))

trunc.pred400m <- trunc.pred400m %>%
  pivot_wider(names_from = Type, values_from = c(SLOPE, SLOPE.STDERR))

# Add the buffer value to the names of each predictor
for (i in 2:length(colnames(trunc.pred400m))) {
  names(trunc.pred400m)[i] <- paste(names(trunc.pred400m)[i], "_400m", sep = "")
}


###############################################
###                                         ###
###           Merge responses and           ###
###               predictors                ###
###############################################

trunc.master <- base :: merge(trunc.biodiv, trunc.pred400m, by = "unique_route",
                              all.x = TRUE, sort = FALSE)


###############################################
###                                         ###
###           Add additional info           ###
###                                         ###
###############################################


######################################
# Get point coordinate of bbs.routes and merge with the master df
# Will be points not lines

bbs.routes2$centroid <- sf :: st_centroid(bbs.routes2$geometry)

# Add the coordinates to the master df
separated_coord <- cbind(bbs.routes2$uniq_rt,
                         bbs.routes2 %>%
                           plyr :: mutate(x = unlist(purrr :: map(bbs.routes2$centroid, 1)),
                                          y = unlist(purrr :: map(bbs.routes2$centroid, 2))))


trunc.master$x <- NA
trunc.master$y <- NA

# https://stackoverflow.com/questions/54734771/sf-write-lat-long-from-geometry-into-separate-column-and-keep-id-column

for (i in 1:length(trunc.master$unique_route)) {
  for (j in 1:length(bbs.routes2$Country)) {
    
    if(trunc.master$unique_route[i] == bbs.routes2$uniq_rt[j]) {
      
      trunc.master$x[i] <- unlist(purrr :: map(bbs.routes2$centroid[j], 1))
      trunc.master$y[i] <- unlist(purrr :: map(bbs.routes2$centroid[j], 2))
      
    }
    
  }
  print(i)
}



###############################################
###                                         ###
###         Save Trends data master         ###
###                                         ###
###############################################

path4 <- "00_Data/processed/"
write.csv(trunc.master, paste(path4,
                              "GLS Master_Trends of Responses and Predictors_1992 to 2018.csv",
                              sep = ""),
          row.names = FALSE)


