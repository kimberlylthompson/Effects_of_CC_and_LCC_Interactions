#################################################################################################
########                                                                                 ########
########                    Obtaining residuals for each model                           ######## 
########                                                                                 ########
#################################################################################################

# Author: Kimberly Thompson

# This code finds the mean residuals for Bayesian models built for the investigation of the 
# impact of climate and land use on biodiversity. 

# Produces dataset:
# 'Cleaned Master with Model Residuals.csv'



########## clean workspace and load required packages ####################

# clean workspace to improve efficiency: #
# rm(list = ls() ) 
# gc() #releases memory

library(rstan)
library(brms)
library(tidybayes)

library(tidyverse) # Data organization
library(lubridate) # Time values
library(data.table) # Data organization
library(readr)
library(purrr)
library(plyr)

library(GGally)
library(usdm) # calculate VIF
library(MuMIn)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

###############################################
###                                         ###
###             Data Loading                ###
###                                         ###
###############################################

# Load the GLS frequentist master for years 1992 - 2018
path.master <- "00_Data/processed/"
master400 <- read.csv(paste(path.master,
                         "Cleaned_GLS Master_Trends of Responses and Predictors_1992 to 2018.csv",
                         sep = ""),
                   header = TRUE)


###############################################
###                                         ###
###             Data Preparation            ###
###                                         ###
###############################################

# Make predictor + 1 standard error columns
master400$temp.upper <- master400$SLOPE_temp_400m + master400$SLOPE.STDERR_temp_400m
master400$precip.upper <- master400$SLOPE_precip_400m + master400$SLOPE.STDERR_precip_400m
master400$canopy.upper <- master400$SLOPE_canopy.mean_400m + master400$SLOPE.STDERR_canopy.mean_400m
master400$crop.upper <- master400$SLOPE_crop.cci_400m + master400$SLOPE.STDERR_crop.cci_400m
master400$urban.upper <- master400$SLOPE_urban.gaia_400m + master400$SLOPE.STDERR_urban.gaia_400m
master400$water.upper <- master400$SLOPE_water.total_400m + master400$SLOPE.STDERR_water.total_400m

master400$temp.lower <- master400$SLOPE_temp_400m - master400$SLOPE.STDERR_temp_400m
master400$precip.lower <- master400$SLOPE_precip_400m - master400$SLOPE.STDERR_precip_400m
master400$canopy.lower <- master400$SLOPE_canopy.mean_400m - master400$SLOPE.STDERR_canopy.mean_400m
master400$crop.lower <- master400$SLOPE_crop.cci_400m - master400$SLOPE.STDERR_crop.cci_400m
master400$urban.lower <- master400$SLOPE_urban.gaia_400m - master400$SLOPE.STDERR_urban.gaia_400m
master400$water.lower <- master400$SLOPE_water.total_400m - master400$SLOPE.STDERR_water.total_400m

# Set the path where the model objects are
path <- "01_Analysis/Modeling_BRMS/Model Objects/CC and LUCC interactions only/"

# Make a list of the models
model.list <- list.files(path, pattern = "interactions.rds|SE.rds")

# Make a vector of what the column names for each residual will be
name.list <- c("res_ab", "res_ab_lower", "res_ab_upper", "res_obs", "res_obs_lower",
               "res_obs_upper", "res_rare", "res_rare_lower", "res_rare_upper")


###############################################
###                                         ###
###             Residual Extraction         ###
###                                         ###
###############################################

for (i in 1:length(model.list)) {
  
  # Read the model
  mod <- readRDS(paste(path, model.list[i], sep = ""))
  
  # Find the predicted values
  preds <- as.data.frame(predict(mod))
  
  # Add the residual value to the master dataframe
  if (i == 1 | i == 2 | i == 3) {
    master400[ , 1 + ncol(master400)] <- master400$SLOPE_LogAb - preds$Estimate
    names(master400)[max(ncol(master400))] <- name.list[i]
  } else {
    if(i == 4 | i == 5 | i == 6) {
      master400[ , 1 + ncol(master400)] <- master400$SLOPE_LogObs - preds$Estimate
      names(master400)[max(ncol(master400))] <- name.list[i]
    } else {
      if(i == 7 | i == 8 | i == 9) {
        master400[ , 1 + ncol(master400)] <- master400$SLOPE.STDERR_LogRare - preds$Estimate
        names(master400)[max(ncol(master400))] <- name.list[i]
      }
    }
  }
  
  # Clean up workspace
  rm(mod, preds)
  gc()
  
  print(i)
}


# Save the dataframe that is now the master + the residuals of each model
path2 <- "01_Analysis/Modeling_BRMS/"
write.csv(master400,
          paste(path2, "Cleaned Master with Model Residuals.csv", sep = ""),
          row.names = FALSE)

  
