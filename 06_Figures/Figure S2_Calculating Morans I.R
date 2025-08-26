#################################################################################################
########                                                                                 ########
########                Calculating Moran's I for richness and abundance                 ######## 
########                                                                                 ########
#################################################################################################

# Author: Kimberly Thompson

# This code uses calculates Moran's I for the responses of species richness and abundance,
# as well as the residuals of species richness and abundance for brms models to determine if 
# there is spatial autocorrelation.

# Plots created:
# Figure S1



########## clean workspace and load required packages ####################

# clean workspace to improve efficiency: #
# rm(list = ls() )
# gc() #releases memory


library(spdep) # for Moran.I analysis
library(tidyverse) # Data organization
library(ggplot2)



###############################################
###                                         ###
###             Data Loading                ###
###                                         ###
###############################################

# Read in the inverse distance matrix
path <- "00_Data/processed/BBS Data/"
inv.dist.mat <- read.table(paste(path,
                                 "BBS inverse distance matrix_1992 to 2018.txt",
                                 sep = ""),
                           header = FALSE)

# Read in the master dataframe with the residuals for each model
path2 <- "01_Analysis/Modeling_BRMS/"
master <- read.csv(paste(path2, "Cleaned Master with Model Residuals.csv", sep = ""),
                   header = TRUE)


###############################################
###                                         ###
###           Data Manipulation             ###
###                                         ###
###############################################

# Remove first row
inv.dist.mat <- inv.dist.mat[-1, ]

# Change all columns to numeric
inv.dist.mat <- sapply(inv.dist.mat, as.numeric)

rownames(inv.dist.mat) <- master$unique_route
colnames(inv.dist.mat) <- master$unique_route

###############################################
###                                         ###
###                Moran's I                ###
###            for responses and            ###
###                residuals                ###
###############################################

# If the observed value of I is significantly greater than the expected value, 
# then the values of x are positively autocorrelated, whereas if Iobserved < Iexpected, 
# this will indicate negative autocorrelation.

# NULL hypothesis: values are randomly distributed across the US following a completely
# random process

#### Responses Slope (Log(Biodiversity Metric)) ####

### Observed Richness ###

# Convert inv.dist.mat to a listw object
lw <- spdep :: mat2listw(inv.dist.mat)

# Moran's I from spdep package with MCMC sampling
MC <- spdep :: moran.mc(master$SLOPE_LogObs, lw, nsim = 999, alternative= "two.sided")

MC

### Rarefied Richness ###

MC2 <- spdep :: moran.mc(master$SLOPE_LogRare, lw, nsim = 999, alternative= "two.sided")

MC2


### Abundance ###

MC3 <- spdep :: moran.mc(master$SLOPE_LogAb, lw, nsim = 999, alternative= "two.sided")

MC3






#### Residuals (residual from non-spatial brms model) ####

### Observed Richness ###

# Moran's I from spdep package with MCMC sampling
MC <- spdep :: moran.mc(master$res_obs, lw, nsim = 999, alternative= "two.sided")

MC

### Rarefied Richness ###

MC2 <- spdep :: moran.mc(master$res_rare, lw, nsim = 999, alternative= "two.sided")

MC2


### Abundance ###

MC3 <- spdep :: moran.mc(master$res_ab, lw, nsim = 999, alternative= "two.sided")

MC3



######################################################################

# Code for graphs from 
# https://stackoverflow.com/questions/59600252/r-ploting-moran-i-plot-in-ggplot


# Creating ggplots of Moran's I of raw responses (log (biodiversity metric))
# and their residuals

# Two step process where you first prepare the data to use in the ggplot
# and then you create the ggplots

# Formula for preparing the data:
prepare_data <- function(data, x, listw){
  # prepare a dataframe with variables x and wx, from the x and listw arguments
  # this dataframe will be the base data for the ggplot() call
  plot_data <- data %>% 
    mutate(
      x = !!enquo(x),
      wx = lag.listw(listw, x, zero.policy = NULL),
      label = as.character(attr(listw, "region.id"))
    ) %>% 
    select(x, wx, label)
  
  # Prepare other needed objects that don't fit into a dataframe
  xwx.lm <- lm(plot_data$wx ~ plot_data$x)
  infl.xwx <- influence.measures(xwx.lm)
  
  # add non variables objects as attributes
  attr(plot_data, which = "is.inf") <- which(apply(infl.xwx$is.inf, 1, any))
  attr(plot_data, which = 'xwx.lm') <- xwx.lm
  
  return(plot_data)
}

##########################
# Log Observed Richness  #
##########################

# Prepare the data for the plot
moran_plot_data <- prepare_data(master, SLOPE_LogObs, lw)

# Prepare the outlier data for the plot labels 
inf_data <- moran_plot_data[attr(moran_plot_data, "is.inf"), ]

# Sort and reduce so not all points are labeled
inf_data2 <- inf_data[inf_data$wx <= -0.000125 | inf_data$wx > 0.0002, ]

# Make the graph
logobs_raw <- ggplot() + 
  geom_point(data = moran_plot_data, mapping = aes(x, wx)) +
  scale_x_continuous("Slope log(Observed Richness)") +
  scale_y_continuous("Spatially Lagged Response") +
  # geom_abline("wxw.lm coefficients used here") +  
  geom_hline(data = moran_plot_data, aes(yintercept = mean(wx))) +
  geom_vline(data = moran_plot_data, aes(xintercept = mean(x))) +
  geom_point(data = inf_data, mapping = aes(x, wx)) + 
  geom_text(data = inf_data2, mapping = aes(x, wx, label = label)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30"))

path3 <- "01_Analysis/Model_Plotting/"
ggsave(paste(path3, "Morans I - log Observed Richness.jpg", sep = ""), plot=logobs_raw,
       device = "jpg",
       width=8, height=6, dpi=600)


###################################
# Log Observed Richness Residuals #
###################################

# Prepare the data for the plot
moran_plot_data <- prepare_data(master400, master400$Mean.residual, lw)

# Prepare the outlier data for the plot labels 
inf_data <- moran_plot_data[attr(moran_plot_data, "is.inf"), ]

# Sort and reduce so not all points are labeled
inf_data2 <- inf_data[inf_data$wx <= -0.000125 | inf_data$wx > 0.0002, ]

# Make the graph
logobs_resid <- ggplot() + 
  geom_point(data = moran_plot_data, mapping = aes(x, wx)) +
  scale_x_continuous("Residuals") +
  scale_y_continuous("Spatially Lagged Response") +
  # geom_abline("wxw.lm coefficients used here") +  
  geom_hline(data = moran_plot_data, aes(yintercept = mean(wx))) +
  geom_vline(data = moran_plot_data, aes(xintercept = mean(x))) +
  geom_point(data = inf_data, mapping = aes(x, wx)) + 
  geom_text(data = inf_data2, mapping = aes(x, wx, label = label)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30"))

ggsave(paste(path3, "Morans I - log Observed Richness Residuals.jpg", sep = ""),
       plot=logobs_resid,
       device = "jpg",
       width=8, height=6, dpi=600)




##########################
# Log Rarefied Richness  #
##########################

# Prepare the data for the plot
moran_plot_data <- prepare_data(master, SLOPE_LogRare, lw)

# Prepare the outlier data for the plot labels 
inf_data <- moran_plot_data[attr(moran_plot_data, "is.inf"), ]

# Sort and reduce so not all points are labeled
inf_data2 <- inf_data[inf_data$wx <= -0.000125 | inf_data$wx > 0.0002, ]

# Make the graph
lograre_raw <- ggplot() + 
  geom_point(data = moran_plot_data, mapping = aes(x, wx)) +
  scale_x_continuous("Slope log(Rarefied Richness)") +
  scale_y_continuous("Spatially Lagged Response") +
  # geom_abline("wxw.lm coefficients used here") +  
  geom_hline(data = moran_plot_data, aes(yintercept = mean(wx))) +
  geom_vline(data = moran_plot_data, aes(xintercept = mean(x))) +
  geom_point(data = inf_data, mapping = aes(x, wx)) + 
  geom_text(data = inf_data2, mapping = aes(x, wx, label = label)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30"))

ggsave(paste(path3, "Morans I - log Rarefied Richness.jpg", sep = ""),
       plot=lograre_raw,
       device = "jpg",
       width=8, height=6, dpi=600)


###################################
# Log Rarefied Richness Residuals #
###################################

# Prepare the data for the plot
moran_plot_data <- prepare_data(master400rare, master400rare$Mean.residual, lw)

# Prepare the outlier data for the plot labels 
inf_data <- moran_plot_data[attr(moran_plot_data, "is.inf"), ]

# Sort and reduce so not all points are labeled
inf_data2 <- inf_data[inf_data$wx <= -0.000125 | inf_data$wx > 0.0002, ]

# Make the graph
lograre_resid <- ggplot() + 
  geom_point(data = moran_plot_data, mapping = aes(x, wx)) +
  scale_x_continuous("Residuals") +
  scale_y_continuous("Spatially Lagged Response") +
  # geom_abline("wxw.lm coefficients used here") +  
  geom_hline(data = moran_plot_data, aes(yintercept = mean(wx))) +
  geom_vline(data = moran_plot_data, aes(xintercept = mean(x))) +
  geom_point(data = inf_data, mapping = aes(x, wx)) + 
  geom_text(data = inf_data2, mapping = aes(x, wx, label = label)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30"))

ggsave(paste(path3, "Morans I - log Rarefied Richness Residuals.jpg", sep = ""),
       plot=lograre_resid,
       device = "jpg",
       width=8, height=6, dpi=600)



##################
# Log Abundance  #
##################

# Prepare the data for the plot
moran_plot_data <- prepare_data(master, SLOPE_LogAb, lw)

# Prepare the outlier data for the plot labels 
inf_data <- moran_plot_data[attr(moran_plot_data, "is.inf"), ]

# Sort and reduce so not all points are labeled
inf_data2 <- inf_data[inf_data$wx <= -0.000125 | inf_data$wx > 0.0002, ]

# Make the graph
logab_raw <- ggplot() + 
  geom_point(data = moran_plot_data, mapping = aes(x, wx)) +
  scale_x_continuous("Slope log(Abundance)") +
  scale_y_continuous("Spatially Lagged Response") +
  # geom_abline("wxw.lm coefficients used here") +  
  geom_hline(data = moran_plot_data, aes(yintercept = mean(wx))) +
  geom_vline(data = moran_plot_data, aes(xintercept = mean(x))) +
  geom_point(data = inf_data, mapping = aes(x, wx)) + 
  geom_text(data = inf_data2, mapping = aes(x, wx, label = label)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30"))

ggsave(paste(path3, "Morans I - log Abundance.jpg", sep = ""),
       plot=logab_raw,
       device = "jpg",
       width=8, height=6, dpi=600)


###########################
# Log Abundance Residuals #
###########################

# Prepare the data for the plot
moran_plot_data <- prepare_data(master400ab, master400ab$Mean.residual, lw)

# Prepare the outlier data for the plot labels 
inf_data <- moran_plot_data[attr(moran_plot_data, "is.inf"), ]

# Sort and reduce so not all points are labeled
inf_data2 <- inf_data[inf_data$wx <= -0.000125 | inf_data$wx > 0.0002, ]

# Make the graph
logab_resid <- ggplot() + 
  geom_point(data = moran_plot_data, mapping = aes(x, wx)) +
  scale_x_continuous("Residuals") +
  scale_y_continuous("Spatially Lagged Response") +
  # geom_abline("wxw.lm coefficients used here") +  
  geom_hline(data = moran_plot_data, aes(yintercept = mean(wx))) +
  geom_vline(data = moran_plot_data, aes(xintercept = mean(x))) +
  geom_point(data = inf_data, mapping = aes(x, wx)) + 
  geom_text(data = inf_data2, mapping = aes(x, wx, label = label)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30"))

ggsave(paste(path3, "Morans I - log Abundance Residuals.jpg", sep = ""),
       plot=logab_resid,
       device = "jpg",
       width=8, height=6, dpi=600)



