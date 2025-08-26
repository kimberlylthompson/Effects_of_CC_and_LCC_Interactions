##################################################################################################
########                                                                                  ########
########             Summary Statistics of Climate and Land Cover Predictors              ######## 
########                      for BBS Routes - Supplemental Table S1                      ########
##################################################################################################

# Author: Kimberly Thompson

# This code extracts summary statistics of unstandardized trends 
# in climate and land cover predictors (temperature, precipitation,
# impervious surface, canopy cover, and cropland).

# FOR THE TIME PERIOD OF 1992 TO 2018 

# Table data created:
# Table S1


# clean workspace to improve efficiency: #
# rm(list = ls() ) 
# gc() #releases memory


library(tidyverse) # Data organization
library(lubridate) # Time values
library(data.table) # Data organization


###############################################
###                                         ###
###             Data Loading                ###
###                                         ###
###############################################

# Load the master which has all the slopes of the predictors
path3 <- "00_Data/processed/"
master <- read.csv(paste(path3,
                         "Cleaned_GLS Master_Trends of Responses and Predictors_1992 to 2018.csv",
                         sep = ""),
                   header = TRUE)


###############################################
###                                         ###
###           Data Preparation              ###
###                                         ###
###############################################

# Reduce to only the variables used in the model
predictors <- master[ , c("SLOPE_canopy.mean_400m",
                          "SLOPE_crop.cci_400m",
                          "SLOPE_precip_400m",
                          "SLOPE_temp_400m",
                          "SLOPE_urban.gaia_400m",
                          "SLOPE_water.total_400m")]



###############################################
###                                         ###
###            Summary info                 ###
###                                         ###
###############################################

# Unstandardized Predictors

summary(predictors)


###############################################
###                                         ###
###      Conversion for Standardized        ###
###              Predictors                 ###
###############################################

# 1 unit of standardized change = change of sd on the original scale

sd(predictors$SLOPE_temp_400m, na.rm = TRUE)

sd(predictors$SLOPE_precip_400m, na.rm = TRUE)

sd(predictors$SLOPE_canopy.mean_400m, na.rm = TRUE)

sd(predictors$SLOPE_crop.cci_400m, na.rm = TRUE)

sd(predictors$SLOPE_urban.gaia_400m, na.rm = TRUE)

sd(predictors$SLOPE_water.total_400m, na.rm = TRUE)







