#################################################################################################
########                                                                                 ########
########                  Graphs of conditional effects for interactions                 ######## 
########                   btw predictors for each biodiversity response                 ########
#################################################################################################


# This code uses the three brms models constructed for observed richness, rarefied richness,
# abundance (1 model for each) and creates graphs of the results.

# Models - Lower standard error of predictors

# Plots created:
# Figure S6
# Figure S7




# Website used for tutorial:
# http://mjskay.github.io/tidybayes/articles/tidy-brms.html



########## clean workspace and load required packages ####################

# clean workspace to improve efficiency: #
rm(list = ls() )
gc() #releases memory

library(rstan)
library(brms)
library(tidybayes)
library(ggmcmc)
library(ggdist)

library(tidyverse) # Data organization

library(magrittr)
library(dplyr)
library(purrr)
library(forcats)
library(tidyr)
library(modelr)
library(ggplot2)
library(cowplot)
library(ggrepel)
library(RColorBrewer)
library(gganimate)
library(posterior)
library(sf)


###############################################
###                                         ###
###             Data Loading                ###
###                                         ###
###############################################

# Load the three model objects
path <- "01_Analysis/Modeling_BRMS/Model Objects/CC and LUCC interactions only/"
observed <- readRDS(paste(path, "Observed Richness_Lower predictor SE.rds", sep = ""))
rarefied <- readRDS(paste(path, "Rarefied Richness_Lower predictor SE.rds", sep = ""))
abundance <- readRDS(paste(path, "Abundance_Lower predictor SE.rds", sep = ""))



###############################################
###                                         ###
###           Data Preparation              ###
###                                         ###
###############################################

# Set theme
theme_set(theme_tidybayes() + panel_border())


# Get a list of raw variable names (will be the same for each model)
variables <- tidybayes :: get_variables(observed)



############################################
###            Interaction Plots         ###
############################################

# observed, rarefied, abundance

# canopy temp
# cropland temp
# urban temp
# water temp

# canopy precip
# cropland precip
# urban precip
# water precip

# Define biodiversity metrics
category <- c("observed", "rarefied", "abundance")

# Define the interaction types
type <- c("temp.can", "temp.crop", "temp.urban", "temp.water",
          "precip.can", "precip.crop", "precip.urban", "precip.water")

# Set the path to save the plots
# path2 <- "01_Analysis/Model_Plotting/Interaction Conditional Effects/Main Model CC and LUCC only/"
path2 <- "~/share/groups/mas/01_projects/iUpdate/01_Analysis/Manuscript_Figures/"  

for (i in 1:length(category)) {
  
  # Define which brms model to use
  if(i == 1) {
    master <- observed
    
  } else if(i == 2) {
    master <- rarefied
    
  } else if(i == 3) {
    master <- abundance
  } # end of if for setting the brms model
  
  for (j in 1:length(type)) {
    
    if(j == 1) { # temp x canopy
      
      interaction.df <- conditional_effects(master,
                                            effects = "temp.lower:canopy.lower")
      interaction.df <- as.data.frame(interaction.df$`temp.lower:canopy.lower`)
      
      name.scale <- "Tree-canopy\n(% Change/Year)"
      
      values.scale <- c('1' = viridis(100)[75], 
                        '0' = cividis(100)[50],
                        '-1' = rocket(100)[60])
      
      labels.scale <- c('1' = " 0.09", 
                        '0' = "-0.13",
                        '-1' = "-0.35")
      
    } else if(j == 2) { # temp x cropland
      
      interaction.df <- conditional_effects(master,
                                            effects = "temp.lower:crop.lower")
      interaction.df <- as.data.frame(interaction.df$`temp.lower:crop.lower`)
      
      name.scale <- "Cropland\n(% Change/Year)"
      
      values.scale <- c('1' = inferno(100)[75],
                        '0' = cividis(100)[50], 
                        '-1' = viridis(100)[50])
      
      labels.scale <- c('1' = " 0.001", 
                        '0' = " 0.000",
                        '-1' = "-0.001")
      
    } else if(j == 3){ # temp x urban
      
      interaction.df <- conditional_effects(master,
                                            effects = "temp.lower:urban.lower")
      interaction.df <- as.data.frame(interaction.df$`temp.lower:urban.lower`)
      
      name.scale <- "Urban\n(% Change/Year)"
      
      values.scale <- c('1' = turbo(100)[95],
                        '0' = cividis(100)[50], 
                        '-1' = plasma(100)[25])
      
      labels.scale <- c('1' = " 0.18", 
                        '0' = " 0.05",
                        '-1' = "-0.08")
      
    } else if(j == 4){ # temp x water
      
      interaction.df <- conditional_effects(master,
                                            effects = "temp.lower:water.lower")
      interaction.df <- as.data.frame(interaction.df$`temp.lower:water.lower`)
      
      name.scale <- "Surface Water\n(% Change/Year)"
      
      values.scale <- c('1' = mako(100)[40],
                        '0' = cividis(100)[50], 
                        '-1' = mako(100)[60])
      
      labels.scale <- c('1' = " 0.16", 
                        '0' = " 0.07",
                        '-1' = "-0.02")
      
    } else if(j == 5){ # precip x canopy
      
      interaction.df <- conditional_effects(master,
                                            effects = "precip.lower:canopy.lower")
      interaction.df <- as.data.frame(interaction.df$`precip.lower:canopy.lower`)
      
      name.scale <- "Tree-canopy\n(% Change/Year)"
      
      values.scale <- c('1' = viridis(100)[75], 
                        '0' = cividis(100)[50],
                        '-1' = rocket(100)[60])
      
      labels.scale <- c('1' = " 0.09", 
                        '0' = "-0.13",
                        '-1' = "-0.35")
      
    } else if(j == 6){ # precip x cropland
      
      interaction.df <- conditional_effects(master,
                                            effects = "precip.lower:crop.lower")
      interaction.df <- as.data.frame(interaction.df$`precip.lower:crop.lower`)
      
      name.scale <- "Cropland\n(% Change/Year)"
      
      values.scale <- c('1' = inferno(100)[75],
                        '0' = cividis(100)[50], 
                        '-1' = viridis(100)[50])
      
      labels.scale <- c('1' = " 0.001", 
                        '0' = " 0.000",
                        '-1' = "-0.001")
      
    } else if(j == 7){ # precip x urban
      
      interaction.df <- conditional_effects(master,
                                            effects = "precip.lower:urban.lower")
      interaction.df <- as.data.frame(interaction.df$`precip.lower:urban.lower`)
      
      name.scale <- "Urban\n(% Change/Year)"
      
      values.scale <- c('1' = turbo(100)[95],
                        '0' = cividis(100)[50], 
                        '-1' = plasma(100)[25])
      
      labels.scale <- c('1' = " 0.18", 
                        '0' = " 0.05",
                        '-1' = "-0.08")
      
    } else if(j == 8){ # precip x water
      
      interaction.df <- conditional_effects(master,
                                            effects = "precip.lower:water.lower")
      interaction.df <- as.data.frame(interaction.df$`precip.lower:water.lower`)
      
      name.scale <- "Surface Water\n(% Change/Year)"
      
      values.scale <- c('1' = mako(100)[40],
                        '0' = cividis(100)[50], 
                        '-1' = mako(100)[60])
      
      labels.scale <- c('1' = " 0.16", 
                        '0' = " 0.07",
                        '-1' = "-0.02")
      
    } # end of if else statement setting up the data
    
    # Change the name in column 1 to unify across the interctions
    names(interaction.df)[1] <- "predictor"
    
    # Set y axis names
    if(i == 1){
      # Observed Richness
      name.y <- "Observed Richness\n(% Change/Year)"
    } else if(i == 2){
      # Rarefied Richness
      name.y <- "Rarefied Richness\n(% Change/Year)"
    } else if(i == 3) {
      # Abundance
      name.y <- "Abundance\n(% Change/Year)"
    }
    
    # Set other y-axis parameters which are universal across the biodiversity 
    # metrics
    limits.y <- c(-0.0121, 0.005)
    breaks.y <- c(-0.010, -0.005, 0.000, 0.005)
    labels.y <- c("-2.28", "-1.14", "0.00", "1.16")
    
    # Set x axis parameters: Temperature interactions
    if(j == 1 | j == 2 | j == 3 | j == 4) {
      breaks <- c(-2, 0, 2)
      labels <- c("-0.01", "0.04", "0.09")
      name <- "Temperature (\u00b0C/Year)"
      limitsx <- c(min(range(interaction.df$predictor)),
                   max(range(interaction.df$predictor)))
      # precipitation interactions
    } else if(j == 5 | j == 6 | j == 7 | j ==8 ){
      breaks <- c(-3, 0, 3)
      labels <- c("-2.37", "0.56", "3.50")
      name <- "Precipitation (mm/Year)"
      limitsx <- c(min(range(interaction.df$predictor)),
                   max(range(interaction.df$predictor)))
      
    } # end of if setting x axis paramters
    
    # Make a plot with a legend for each of the land-cover types
    # Only needs to be done for 1 biodiversity metric and each land-cover since
    # the same legends can be used in the other plots
    if(i == 1 & (j == 1 | j == 2 | j == 3 | j == 4)) {
      
      interaction.plot_leg <- ggplot() +
        # Zero lines
        geom_hline(yintercept = 0, color = "black") +
        geom_vline(xintercept = 0, color = "black") +
        geom_line(data = interaction.df, aes(x = predictor, y = estimate__, color = effect2__),
                  linewidth = 1) +
        geom_ribbon(data = interaction.df, aes(x = predictor, y = estimate__, ymin = lower__,
                                               ymax = upper__, fill = effect2__), alpha = 0.5) +
        theme_bw() +
        theme(axis.text.x = element_text(size=22, face="bold")) +
        theme(axis.text.y = element_text(size=22, face="bold")) +
        theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
        theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
        scale_x_continuous(name = name,
                           limits = limitsx,
                           breaks = breaks,
                           labels = labels) +
        scale_y_continuous(name = name.y, 
                           limits = limits.y,
                           breaks = breaks.y,
                           labels = labels.y) +
        scale_colour_manual(name = name.scale, 
                            values = values.scale,
                            labels = labels.scale) +
        scale_fill_manual(name = name.scale,
                          values = values.scale,
                          labels = labels.scale) +
        theme(legend.text = element_text(size=18)) +
        theme(legend.title = element_text(size = 18, hjust = 0.5)) +
        theme(legend.key.size = unit(0.75, 'cm')) +
        guides(colour = guide_legend(override.aes = list(linewidth = 3)))
      # theme(legend.position = c(0.80, 0.85)) +
      # theme(legend.position = "none")
      
      # Save the plot with the legend
      ggsave(paste(path2, "LEGEND_", type[j], ".jpg",
                   sep = ""),
             plot = interaction.plot_leg,
             device = "jpg",
             width=7, height=5, dpi=600)
      
    } # end of if statement to make plots with legend
    
    # Make plot without legend for all biodiv and interaction types
    
    interaction.plot <- ggplot() +
      # Zero lines
      geom_hline(yintercept = 0, color = "black") +
      geom_vline(xintercept = 0, color = "black") +
      geom_line(data = interaction.df, aes(x = predictor, y = estimate__, color = effect2__),
                linewidth = 1) +
      geom_ribbon(data = interaction.df, aes(x = predictor, y = estimate__, ymin = lower__,
                                             ymax = upper__, fill = effect2__), alpha = 0.5) +
      theme_bw() +
      theme(axis.text.x = element_text(size=22, face="bold")) +
      theme(axis.text.y = element_text(size=22, face="bold")) +
      theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
      theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
      scale_x_continuous(name = name,
                         limits = limitsx,
                         breaks = breaks,
                         labels = labels) +
      scale_y_continuous(name = name.y, 
                         limits = limits.y,
                         breaks = breaks.y,
                         labels = labels.y) +
      scale_colour_manual(name = name.scale, 
                          values = values.scale,
                          labels = labels.scale) +
      scale_fill_manual(name = name.scale,
                        values = values.scale,
                        labels = labels.scale) +
      theme(legend.position = "none")
    
    # Save the plot with the legend
    ggsave(paste(path2, "IntPlot_", category[i], "_",
                 type[j], ".jpg",
                 sep = ""),
           plot = interaction.plot,
           device = "jpg",
           width=7, height=5, dpi=600)
    
    print(paste(category[i], type[j], "done", sep = " "))
    
  } # end of j loop for interaction type
  
} # end of i loop for biodiversity type



