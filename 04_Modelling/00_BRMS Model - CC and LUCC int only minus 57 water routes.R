#################################################################################################
########                                                                                 ########
########                 Bayesian Model of Biodiversity metrics                          ######## 
########                  in relation to climate and land cover                          ########
#################################################################################################

# Author: Kimberly Thompson

# These models are excluding the 57 potentially problematic surface-water routes.
# Idea is to run these models and compare the results for surface water to the ones
# that included all the routes.


# Produces model objects:
# 'Observed Richness_CC LUCC interactions_minus57.rds'
# 'Rarefied Richness_CC LUCC interactions_minus57.rds'
# 'Abundance_CC LUCC interactions_minus57.rds'


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

# Load the routes to remove
path2 <- "00_Data/raw/"
routes.to.remove <- read.csv(paste(path2,
                                   "Routes with potentially problematic water trends.csv",
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



###############################################
###                                         ###
###       Standardize the continuous        ###
###               predictors                ###
###############################################

# This is only for the slopes though, not the slope std errors

# Standardize the continuous predictors
master400 <- master400 %>%
  dplyr :: mutate_at(c(13, 15, 17, 19, 21, 23),
                     ~(MuMIn :: stdize(.) %>% as.vector))


# Remove the routes that might have problematic water trends
master400 <- master400 %>%
  filter(!(unique_route %in% routes.to.remove$problem.routes))



###############################################
###                                         ###
###            Setting priors               ###
###                                         ###
###############################################

# Using Observed richness as an example, find what priors are required for model 
# with all two-way interactions between climate and LC

# Get a list of which priors are needed
brms :: get_prior(bf(SLOPE_LogObs | se(SLOPE.STDERR_LogObs) ~
                       SLOPE_temp_400m +
                       SLOPE_precip_400m +
                       SLOPE_canopy.mean_400m +
                       SLOPE_crop.cci_400m +
                       SLOPE_urban.gaia_400m +
                       SLOPE_water.total_400m +
                      
                       SLOPE_temp_400m*SLOPE_canopy.mean_400m +
                       SLOPE_temp_400m*SLOPE_crop.cci_400m +
                       SLOPE_temp_400m*SLOPE_urban.gaia_400m +
                       SLOPE_temp_400m*SLOPE_water.total_400m +
                       
                       SLOPE_precip_400m*SLOPE_canopy.mean_400m +
                       SLOPE_precip_400m*SLOPE_crop.cci_400m +
                       SLOPE_precip_400m*SLOPE_urban.gaia_400m +
                       SLOPE_precip_400m*SLOPE_water.total_400m),
                       
                  data = master,
                  family = gaussian())

# Set priors
mypriors <- c(prior(normal(0.02, 0.01), class = b, coef = SLOPE_temp_400m),
              prior(normal(1.1, 0.5), class = b, coef = SLOPE_precip_400m),
              prior(normal(0, 0.03), class = b, coef = SLOPE_canopy.mean_400m),
              prior(normal(0.25, 0.25), class = b, coef = SLOPE_urban.gaia_400m),
              prior(normal(0, 0.75), class = b, coef = SLOPE_crop.cci_400m),
              prior(normal(0, 1), class = b, coef = SLOPE_water.total_400m))


###############################################
###                                         ###
###        Fitting models with BRMS         ###
###           OBSERVED RICHNESS             ###
###############################################

# Log(Observed) - FULL MODEL

# Log(Observed richness) - predictors with interactions
# Response weighted by measurement error of biodiversity slopes
start.time <- Sys.time()
logobs.model <- brms :: brm(bf(SLOPE_LogObs | se(SLOPE.STDERR_LogObs) ~
                                 SLOPE_temp_400m +
                                 SLOPE_precip_400m +
                                 SLOPE_canopy.mean_400m +
                                 SLOPE_crop.cci_400m +
                                 SLOPE_urban.gaia_400m +
                                 SLOPE_water.total_400m +

                                 SLOPE_temp_400m*SLOPE_canopy.mean_400m +
                                 SLOPE_temp_400m*SLOPE_crop.cci_400m +
                                 SLOPE_temp_400m*SLOPE_urban.gaia_400m +
                                 SLOPE_temp_400m*SLOPE_water.total_400m +
                                 
                                 SLOPE_precip_400m*SLOPE_canopy.mean_400m +
                                 SLOPE_precip_400m*SLOPE_crop.cci_400m +
                                 SLOPE_precip_400m*SLOPE_urban.gaia_400m +
                                 SLOPE_precip_400m*SLOPE_water.total_400m),

                         data = master400,
                         family = gaussian(),
                         prior = mypriors,
                         warmup = 1000, iter = 10000, cores = 4, chains = 4,
                         control = list(max_treedepth = 15, adapt_delta = 0.99))
end.time <- Sys.time()
total.time <- end.time - start.time # 2.89 minutes
# Model ran without warnings!
summary(logobs.model)

# Save model
path3 <- "01_Analysis/Modeling_BRMS/Model Objects/CC and LUCC interactions only"
saveRDS(logobs.model, paste(path3,
                            "Observed Richness_CC LUCC interactions_minus57.rds",
                            sep = ""))


###############################################
###                                         ###
###        Fitting models with BRMS         ###
###           RAREFIED RICHNESS             ###
###############################################

# Rarefied - FULL MODEL

# Log(Rarefied richness) - predictors with interactions
# Response weighted by measurement error of biodiversity slopes
start.time <- Sys.time()
rare.model <- brms :: brm(bf(SLOPE_LogRare | se(SLOPE.STDERR_LogRare) ~
                               SLOPE_temp_400m +
                               SLOPE_precip_400m +
                               SLOPE_canopy.mean_400m +
                               SLOPE_crop.cci_400m +
                               SLOPE_urban.gaia_400m +
                               SLOPE_water.total_400m +

                               SLOPE_temp_400m*SLOPE_canopy.mean_400m +
                               SLOPE_temp_400m*SLOPE_crop.cci_400m +
                               SLOPE_temp_400m*SLOPE_urban.gaia_400m +
                               SLOPE_temp_400m*SLOPE_water.total_400m +
                               
                               SLOPE_precip_400m*SLOPE_canopy.mean_400m +
                               SLOPE_precip_400m*SLOPE_crop.cci_400m +
                               SLOPE_precip_400m*SLOPE_urban.gaia_400m +
                               SLOPE_precip_400m*SLOPE_water.total_400m),

                          data = master400,
                          family = gaussian(),
                          prior = mypriors,
                          warmup = 1000, iter = 10000, cores = 4, chains = 4,
                          control = list(max_treedepth = 15, adapt_delta = 0.99))
end.time <- Sys.time()
total.time <- end.time - start.time # 1.75 minutes

# Model ran without warnings!
summary(rare.model)

# Save model
saveRDS(rare.model, paste(path3, "Rarefied Richness_CC LUCC interactions_minus57.rds", sep = ""))



###############################################
###                                         ###
###        Fitting models with BRMS         ###
###                ABUNDANCE                ###
###############################################

# Abundance- FULL MODEL

# Log(Abundance) - predictors with interactions
# Response weighted by measurement error of biodiversity slopes
start.time <- Sys.time()
ab.model <- brms :: brm(bf(SLOPE_LogAb | se(SLOPE.STDERR_LogAb) ~
                             SLOPE_temp_400m +
                             SLOPE_precip_400m +
                             SLOPE_canopy.mean_400m +
                             SLOPE_crop.cci_400m +
                             SLOPE_urban.gaia_400m +
                             SLOPE_water.total_400m +

                             SLOPE_temp_400m*SLOPE_canopy.mean_400m +
                             SLOPE_temp_400m*SLOPE_crop.cci_400m +
                             SLOPE_temp_400m*SLOPE_urban.gaia_400m +
                             SLOPE_temp_400m*SLOPE_water.total_400m +
                             
                             SLOPE_precip_400m*SLOPE_canopy.mean_400m +
                             SLOPE_precip_400m*SLOPE_crop.cci_400m +
                             SLOPE_precip_400m*SLOPE_urban.gaia_400m +
                             SLOPE_precip_400m*SLOPE_water.total_400m),

                          data = master400,
                          family = gaussian(),
                          prior = mypriors,
                          warmup = 1000, iter = 10000, cores = 4, chains = 4,
                          control = list(max_treedepth = 15, adapt_delta = 0.99))
end.time <- Sys.time()
total.time <- end.time - start.time # 1.75 minutes

# Model ran without warnings!
summary(ab.model)

# Save model
saveRDS(ab.model, paste(path3, "Abundance_CC LUCC interactions_minus57.rds", sep = ""))
