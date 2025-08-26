#################################################################################################
########                                                                                 ########
########                 Bayesian Model of Biodiversity metrics                          ######## 
########                  in relation to climate and land cover                          ########
#################################################################################################

# Author: Kimberly Thompson

# Modeling uncertatinty in the predictors - These models are based on the predictor slopes
# +/- 1 standard error

# Produces model objects:
# 'Observed Richness_Upper Predictor SE.rds'
# 'Observed Richness_Lower Predictor SE.rds'
# 'Rarefied Richness_Upper Predictor SE.rds'
# 'Rarefied Richness_Lower Predictor SE.rds'
# 'Abundance_Upper Predictor SE.rds'
# 'Abundance_Lower Predictor SE.rds'



# This code builds a Bayesian model using brms that models observed richness, rarefied richness,
# and abundance trends based on trends of climate and land cover at a 400 meter buffer.

# Importantly, response is the trend in the log value of either richness or abundance.
# This is critical for having comparable results across responses.

# Includes uncertainty in biodiversity responses, but not uncertainty in predictors.

# Does not include spatial autocorrelation.

# all response and predictor trends calculated with nlme::gls with AR1 correlation structure.




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
path <- "00_Data/processed/"
master400 <- read.csv(paste(path,
                            "Cleaned_GLS Master_Trends of Responses and Predictors_1992 to 2018.csv",
                            sep = ""),
                      header = TRUE)


###############################################
###                                         ###
###           Data Manipulation             ###
###                                         ###
###############################################

###########
# Master  #
###########

# There will be NAs for some land cover predictors where that cover type does not exist
# Replace this values with 0's
master400 <- master400 %>%
  mutate_at(c(13:16, 21:24), ~replace(., is.na(.), 0))


# Make predictor + 1 standard error columns
master400$temp.upper <- master400$SLOPE_temp_400m + master400$SLOPE.STDERR_temp_400m
master400$precip.upper <- master400$SLOPE_precip_400m + master400$SLOPE.STDERR_precip_400m
master400$canopy.upper <- master400$SLOPE_canopy.mean_400m + master400$SLOPE.STDERR_canopy.mean_400m
master400$crop.upper <- master400$SLOPE_crop.cci_400m + master400$SLOPE.STDERR_crop.cci_400m
master400$urban.upper <- master400$SLOPE_urban.gaia_400m + master400$SLOPE.STDERR_urban.gaia_400m
master400$water.upper <- master400$SLOPE_water.total_400m + master400$SLOPE.STDERR_water.total_400m

# Make predictor - 1 standard error columns
master400$temp.lower <- master400$SLOPE_temp_400m - master400$SLOPE.STDERR_temp_400m
master400$precip.lower <- master400$SLOPE_precip_400m - master400$SLOPE.STDERR_precip_400m
master400$canopy.lower <- master400$SLOPE_canopy.mean_400m - master400$SLOPE.STDERR_canopy.mean_400m
master400$crop.lower <- master400$SLOPE_crop.cci_400m - master400$SLOPE.STDERR_crop.cci_400m
master400$urban.lower <- master400$SLOPE_urban.gaia_400m - master400$SLOPE.STDERR_urban.gaia_400m
master400$water.lower <- master400$SLOPE_water.total_400m - master400$SLOPE.STDERR_water.total_400m


###############################################
###                                         ###
###       Standardize the continuous        ###
###               predictors                ###
###############################################


# Standardize the continuous predictors
master400 <- master400 %>%
  dplyr :: mutate_at(c(25:36),
                     ~(MuMIn :: stdize(.) %>% as.vector))



###############################################
###                                         ###
###            Setting priors               ###
###                                         ###
###############################################


# Set priors - same for both models
mypriors <- c(prior(normal(0.02, 0.01), class = b, coef = temp.upper),
              prior(normal(1.1, 0.5), class = b, coef = precip.upper),
              prior(normal(0, 0.03), class = b, coef = canopy.upper),
              prior(normal(0.25, 0.25), class = b, coef = urban.upper),
              prior(normal(0, 0.75), class = b, coef = crop.upper),
              prior(normal(0, 1), class = b, coef = water.upper))

mypriors2 <- c(prior(normal(0.02, 0.01), class = b, coef = temp.lower),
              prior(normal(1.1, 0.5), class = b, coef = precip.lower),
              prior(normal(0, 0.03), class = b, coef = canopy.lower),
              prior(normal(0.25, 0.25), class = b, coef = urban.lower),
              prior(normal(0, 0.75), class = b, coef = crop.lower),
              prior(normal(0, 1), class = b, coef = water.lower))


###############################################
###                                         ###
###        Fitting models with BRMS         ###
###           OBSERVED RICHNESS             ###
###############################################

# Log(Observed) - FULL MODEL

# Log(Observed richness) - predictors with interactions
# Response weighted by measurement error of biodiversity slopes
logobs.model <- brms :: brm(bf(SLOPE_LogObs | se(SLOPE.STDERR_LogObs) ~
                                 temp.upper +
                                 precip.upper +
                                 canopy.upper +
                                 crop.upper +
                                 urban.upper +
                                 water.upper +
                                 
                                 temp.upper*canopy.upper +
                                 temp.upper*crop.upper +
                                 temp.upper*urban.upper +
                                 temp.upper*water.upper +
                                 
                                 precip.upper*canopy.upper +
                                 precip.upper*crop.upper +
                                 precip.upper*urban.upper +
                                 precip.upper*water.upper),
                            
                         data = master400,
                         family = gaussian(),
                         prior = mypriors,
                         warmup = 1000, iter = 10000, cores = 4, chains = 4,
                         control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(logobs.model)

# Lower SE
logobs.model_lower <- brms :: brm(bf(SLOPE_LogObs | se(SLOPE.STDERR_LogObs) ~
                                 temp.lower +
                                 precip.lower +
                                 canopy.lower +
                                 crop.lower +
                                 urban.lower +
                                 water.lower +
                                 
                                 temp.lower*canopy.lower +
                                 temp.lower*crop.lower +
                                 temp.lower*urban.lower +
                                 temp.lower*water.lower +
                                 
                                 precip.lower*canopy.lower +
                                 precip.lower*crop.lower +
                                 precip.lower*urban.lower +
                                 precip.lower*water.lower),
                            
                            data = master400,
                            family = gaussian(),
                            prior = mypriors2,
                            warmup = 1000, iter = 10000, cores = 4, chains = 4,
                            control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(logobs.model_lower)


# Save model
path2 <- "01_Analysis/Modeling_BRMS/Model Objects/CC and LUCC interactions only/"
saveRDS(logobs.model,
        paste(path2, "Observed Richness_Upper Predictor SE.rds", sep = ""))
saveRDS(logobs.model_lower,
        paste(path2,"Observed Richness_Lower predictor SE.rds", sep = ""))


###############################################
###                                         ###
###        Fitting models with BRMS         ###
###           RAREFIED RICHNESS             ###
###############################################

# Rarefied - FULL MODEL

# Log(Rarefied richness) - predictors with interactions
# Response weighted by measurement error of biodiversity slopes
rare.model_upper <- brms :: brm(bf(SLOPE_LogRare | se(SLOPE.STDERR_LogRare) ~
                                 temp.upper +
                                 precip.upper +
                                 canopy.upper +
                                 crop.upper +
                                 urban.upper +
                                 water.upper +
                                 
                                 temp.upper*canopy.upper +
                                 temp.upper*crop.upper +
                                 temp.upper*urban.upper +
                                 temp.upper*water.upper +
                                 
                                 precip.upper*canopy.upper +
                                 precip.upper*crop.upper +
                                 precip.upper*urban.upper +
                                 precip.upper*water.upper),
                            
                            data = master400,
                            family = gaussian(),
                            prior = mypriors,
                            warmup = 1000, iter = 10000, cores = 4, chains = 4,
                            control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(rare.model_upper)

# Lower SE
rare.model_lower <- brms :: brm(bf(SLOPE_LogRare | se(SLOPE.STDERR_LogRare) ~
                                       temp.lower +
                                       precip.lower +
                                       canopy.lower +
                                       crop.lower +
                                       urban.lower +
                                       water.lower +
                                       
                                       temp.lower*canopy.lower +
                                       temp.lower*crop.lower +
                                       temp.lower*urban.lower +
                                       temp.lower*water.lower +
                                       
                                       precip.lower*canopy.lower +
                                       precip.lower*crop.lower +
                                       precip.lower*urban.lower +
                                       precip.lower*water.lower),
                                  
                                  data = master400,
                                  family = gaussian(),
                                  prior = mypriors2,
                                  warmup = 1000, iter = 10000, cores = 4, chains = 4,
                                  control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(rare.model_lower)

# Save model
saveRDS(rare.model_upper,
        paste(path2, "Rarefied Richness_Upper Predictor SE.rds", sep = ""))
saveRDS(rare.model_lower,
        paste(path2, "Rarefied Richness_Lower predictor SE.rds", sep = ""))



###############################################
###                                         ###
###        Fitting models with BRMS         ###
###                ABUNDANCE                ###
###############################################

# Abundance- FULL MODEL

# Log(Abundance) - predictors with interactions
# Response weighted by measurement error of biodiversity slopes
ab.model_upper <- brms :: brm(bf(SLOPE_LogAb | se(SLOPE.STDERR_LogAb) ~
                                     temp.upper +
                                     precip.upper +
                                     canopy.upper +
                                     crop.upper +
                                     urban.upper +
                                     water.upper +
                                     
                                     temp.upper*canopy.upper +
                                     temp.upper*crop.upper +
                                     temp.upper*urban.upper +
                                     temp.upper*water.upper +
                                     
                                     precip.upper*canopy.upper +
                                     precip.upper*crop.upper +
                                     precip.upper*urban.upper +
                                     precip.upper*water.upper),
                                
                                data = master400,
                                family = gaussian(),
                                prior = mypriors,
                                warmup = 1000, iter = 10000, cores = 4, chains = 4,
                                control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(ab.model_upper)

# Lower SE
ab.model_lower <- brms :: brm(bf(SLOPE_LogAb | se(SLOPE.STDERR_LogAb) ~
                                     temp.lower +
                                     precip.lower +
                                     canopy.lower +
                                     crop.lower +
                                     urban.lower +
                                     water.lower +
                                     
                                     temp.lower*canopy.lower +
                                     temp.lower*crop.lower +
                                     temp.lower*urban.lower +
                                     temp.lower*water.lower +
                                     
                                     precip.lower*canopy.lower +
                                     precip.lower*crop.lower +
                                     precip.lower*urban.lower +
                                     precip.lower*water.lower),
                                
                                data = master400,
                                family = gaussian(),
                                prior = mypriors2,
                                warmup = 1000, iter = 10000, cores = 4, chains = 4,
                                control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(ab.model_lower)

# Save model
setwd("~/share/groups/MAS/01_projects/iUpdate/01_Analysis/Modeling_BRMS/Model Objects/CC and LUCC interactions only")
saveRDS(ab.model_upper,
        paste(path2, "Abundance_Upper Predictor SE.rds", sep = ""))
saveRDS(ab.model_lower,
        paste(path2, "Abundance_Lower predictor SE.rds", sep = ""))
