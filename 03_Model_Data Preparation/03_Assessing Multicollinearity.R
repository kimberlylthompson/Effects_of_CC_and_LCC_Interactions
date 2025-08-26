################################################################################
########                                                                ########
########          Testing for correlations among predictor variables    ######## 
########                                                                ########
################################################################################

# Author: Kimberly Thompson

# This code checks the correlations between continuous predictor variables:
# "Tree Canopy Trend",
# "Cropland Trend",
# "Precipitation Trend",
# "Temperature Trend",
# "Urban Trend",
# "Water Trend",

# Produces supplemental figure
# 

########## clean workspace and load required packages ####################

# clean workspace to improve efficiency: #
# rm(list = ls() ) 
# gc() #releases memory

library(ggplot2)
library(GGally)
library(performance)



###############################################
###                                         ###
###             Data Loading                ###
###                                         ###
###############################################

# Load the GLS frequentist master for years 1992 - 2018
# setwd("~/share/groups/mas/01_projects/iUpdate/00_Data/processed")
# master400 <- read.csv("Cleaned_GLS Master_Trends of Responses and Predictors_1992 to 2018.csv",
#                       header = TRUE)

path <- "00_Data/processed/"
master400 <- read.csv(paste(path,
                            "Cleaned_GLS Master_Trends of Responses and Predictors_1992 to 2018.csv",
                            sep = ""),
                      header = TRUE)

# Load the models
path <- "Output Data/Model Objects"
observed <- readRDS("Observed Richness_CC LUCC interactions.rds")
rarefied <- readRDS("Rarefied Richness_CC LUCC interactions.rds")
abundance <- readRDS("Abundance_CC LUCC interactions.rds")




###############################################
###                                         ###
###           Data Manipulation             ###
###                                         ###
###############################################

# Continuous predictors
predictors <- master400[ , c("SLOPE_canopy.mean_400m",
                             "SLOPE_crop.cci_400m",
                             "SLOPE_precip_400m",
                             "SLOPE_temp_400m",
                             "SLOPE_urban.gaia_400m",
                             "SLOPE_water.total_400m")]


###############################################
###                                         ###
###              Plotting                   ###
###                                         ###
###############################################

cor.plot <- ggpairs(predictors,
                    columnLabels = c("Tree Canopy Trend",
                                     "Cropland Trend",
                                     "Precipitation Trend",
                                     "Temperature Trend",
                                     "Urban Trend",
                                     "Water Trend"),
                    upper = list(continuous = "cor"),
                    lower = list(continuous = "smooth"),
                    diag = list(continuous = "densityDiag"))


path <- "01_Analysis/Model_Plotting/"
ggsave(paste(path, "Correlation Plot.jpg", sep = ""),
       cor.plot,
       width = 12, height = 10,
       dpi = 300)


# setwd("~/share/groups/mas/01_projects/iUpdate/01_Analysis/Modeling_Preparation")
# ggsave("Correlation Plot.jpg",
#        cor.plot,
#        width = 12, height = 10,
#        dpi = 300)


###############################################
###                                         ###
###       Variance Inflation Factors        ###
###                                         ###
###############################################

obs.vif <- performance :: check_collinearity(observed)

rare.vif <- performance :: check_collinearity(rarefied)

abundance.vif <- performance :: check_collinearity(abundance)


# Add a type to each dataframe
obs.vif$Type <- "Observed"
rare.vif$Type <- "Rarefied"
abundance.vif$Type <- "Abundance"

# Combine dataframes
overall.vif <- rbind(obs.vif, rare.vif, abundance.vif)

# Save the dataframe
path <- "01_Analysis/Model Evaluation"
write.csv(overall.vif, "Variance Inflation Factors.csv", row.names = FALSE)
