#################################################################################################
########                                                                                 ########
########                  Graphs of conditional effects for interactions                 ######## 
########                   btw predictors for each biodiversity response                 ########
#################################################################################################

# Author: Kimberly Thompson

# This code uses the three brms models constructed for observed richness, rarefied richness,
# abundance (1 model for each) and creates graphs of the results.

# Model = Upper standard error of predictors

# Plots created:
# Figure S9
# Figure S10



# Website used for tutorial:
# http://mjskay.github.io/tidybayes/articles/tidy-brms.html



########## clean workspace and load required packages ####################

# clean workspace to improve efficiency: #
rm(list = ls() )
gc() #releases memory


library(brms)
library(tidybayes)
library(cowplot)

library(tidyverse) # Data organization

library(ggplot2)

library(viridis)


###############################################
###                                         ###
###             Data Loading                ###
###                                         ###
###############################################

# Load the three model objects
path <- "01_Analysis/Modeling_BRMS/Model Objects/CC and LUCC interactions only/"
observed <- readRDS(paste(path, "Observed Richness_Upper Predictor SE.rds", sep = ""))
rarefied <- readRDS(paste(path, "Rarefied Richness_Upper Predictor SE.rds", sep = ""))
abundance <- readRDS(paste(path, "Abundance_Upper Predictor SE.rds", sep = ""))



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
                                            effects = "temp.upper:canopy.upper")
      interaction.df <- as.data.frame(interaction.df$`temp.upper:canopy.upper`)
      
      name.scale <- "Tree-canopy\n(% Change/Year)"
      
      values.scale <- c('1' = viridis(100)[75], 
                        '0' = cividis(100)[50],
                        '-1' = rocket(100)[60])
      
      labels.scale <- c('1' = " 0.09", 
                        '0' = "-0.13",
                        '-1' = "-0.35")
      
    } else if(j == 2) { # temp x cropland
      
      interaction.df <- conditional_effects(master,
                                            effects = "temp.upper:crop.upper")
      interaction.df <- as.data.frame(interaction.df$`temp.upper:crop.upper`)
      
      name.scale <- "Cropland\n(% Change/Year)"
      
      values.scale <- c('1' = inferno(100)[75],
                        '0' = cividis(100)[50], 
                        '-1' = viridis(100)[50])
      
      labels.scale <- c('1' = " 0.001", 
                        '0' = " 0.000",
                        '-1' = "-0.001")
      
    } else if(j == 3){ # temp x urban
      
      interaction.df <- conditional_effects(master,
                                            effects = "temp.upper:urban.upper")
      interaction.df <- as.data.frame(interaction.df$`temp.upper:urban.upper`)
      
      name.scale <- "Urban\n(% Change/Year)"
      
      values.scale <- c('1' = turbo(100)[95],
                        '0' = cividis(100)[50], 
                        '-1' = plasma(100)[25])
      
      labels.scale <- c('1' = " 0.18", 
                        '0' = " 0.05",
                        '-1' = "-0.08")
      
    } else if(j == 4){ # temp x water
      
      interaction.df <- conditional_effects(master,
                                            effects = "temp.upper:water.upper")
      interaction.df <- as.data.frame(interaction.df$`temp.upper:water.upper`)
      
      name.scale <- "Surface Water\n(% Change/Year)"
      
      values.scale <- c('1' = mako(100)[40],
                        '0' = cividis(100)[50], 
                        '-1' = mako(100)[60])
      
      labels.scale <- c('1' = " 0.16", 
                        '0' = " 0.07",
                        '-1' = "-0.02")
      
    } else if(j == 5){ # precip x canopy
      
      interaction.df <- conditional_effects(master,
                                            effects = "precip.upper:canopy.upper")
      interaction.df <- as.data.frame(interaction.df$`precip.upper:canopy.upper`)
      
      name.scale <- "Tree-canopy\n(% Change/Year)"
      
      values.scale <- c('1' = viridis(100)[75], 
                        '0' = cividis(100)[50],
                        '-1' = rocket(100)[60])
      
      labels.scale <- c('1' = " 0.09", 
                        '0' = "-0.13",
                        '-1' = "-0.35")
      
    } else if(j == 6){ # precip x cropland
      
      interaction.df <- conditional_effects(master,
                                            effects = "precip.upper:crop.upper")
      interaction.df <- as.data.frame(interaction.df$`precip.upper:crop.upper`)
      
      name.scale <- "Cropland\n(% Change/Year)"
      
      values.scale <- c('1' = inferno(100)[75],
                        '0' = cividis(100)[50], 
                        '-1' = viridis(100)[50])
      
      labels.scale <- c('1' = " 0.001", 
                        '0' = " 0.000",
                        '-1' = "-0.001")
      
    } else if(j == 7){ # precip x urban
      
      interaction.df <- conditional_effects(master,
                                            effects = "precip.upper:urban.upper")
      interaction.df <- as.data.frame(interaction.df$`precip.upper:urban.upper`)
      
      name.scale <- "Urban\n(% Change/Year)"
      
      values.scale <- c('1' = turbo(100)[95],
                        '0' = cividis(100)[50], 
                        '-1' = plasma(100)[25])
      
      labels.scale <- c('1' = " 0.18", 
                        '0' = " 0.05",
                        '-1' = "-0.08")
      
    } else if(j == 8){ # precip x water
      
      interaction.df <- conditional_effects(master,
                                            effects = "precip.upper:water.upper")
      interaction.df <- as.data.frame(interaction.df$`precip.upper:water.upper`)
      
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


















####################
## Canopy :: Temp ##
####################

# Make the dfs

obs.tempcan <- conditional_effects(observed, effects = "temp.upper:canopy.upper")
obs.tempcan <- as.data.frame(obs.tempcan$`temp.upper:canopy.upper`)
obs.tempcan$Type <- "observed"
rare.tempcan <- conditional_effects(rarefied, effects = "temp.upper:canopy.upper")
rare.tempcan <- as.data.frame(rare.tempcan$`temp.upper:canopy.upper`)
rare.tempcan$Type <- "rarefied"
ab.tempcan <- conditional_effects(abundance, effects = "temp.upper:canopy.upper")
ab.tempcan <- as.data.frame(ab.tempcan$`temp.upper:canopy.upper`)
ab.tempcan$Type <- "abundance"

# effect1__ is the range of temperature slope values
# effect2__ is the categorical values of canopy: 1, 0, -1

# FYI: effect1__ is identical to temp.upper (will be identical to whatever first varying
# parameter of interest is)

# Categorical levels are not exactly 1, 0 and negative 1 so they need to be recoded
# First change to numeric then round, then convert back to factor
obs.tempcan$effect2__ <- factor(round(as.numeric(as.character(obs.tempcan$effect2__))))
rare.tempcan$effect2__ <- factor(round(as.numeric(as.character(rare.tempcan$effect2__))))
ab.tempcan$effect2__ <- factor(round(as.numeric(as.character(ab.tempcan$effect2__))))

# Observed
tempcanplot <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = obs.tempcan, aes(x = temp.upper, y = estimate__, color = effect2__)) +
  geom_ribbon(data = obs.tempcan, aes(x = temp.upper, y = estimate__, ymin = lower__,
                                    ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Temperature Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2)) +
  # scale_x_continuous(name="\u0394Temperature (\u00b0C)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Observed Richness Trend", limits = c(-0.0125, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Canopy", values = c('1' = "forestgreen",
                                                      '0' = "gray40", 
                                                      '-1' = "brown4")) +
  scale_fill_manual(name="\u0394Canopy", values = c('1' = "forestgreen",
                                                    '0' = "gray40", 
                                                    '-1' = "brown4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
# theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
# guides(colour = guide_legend(override.aes = (list(size=3))))

path2 <- "01_Analysis/Model_Plotting/Interaction Conditional Effects/Upper SE for predictors/"
ggsave(paste(path2, "Temperature and Canopy - Observed conditional effects.jpg", sep = ""),
       plot=tempcanplot,
       device = "jpg",
       width=7, height=5, dpi=600)

# Rarefied
tempcanplotR <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = rare.tempcan, aes(x = temp.upper, y = estimate__, color = effect2__)) +
  geom_ribbon(data = rare.tempcan, aes(x = temp.upper, y = estimate__, ymin = lower__,
                                      ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Temperature Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2)) +
  # scale_x_continuous(name="\u0394Temperature (\u00b0C)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Rarefied Richness Trend", limits = c(-0.0125, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Canopy", values = c('1' = "forestgreen",
                                                      '0' = "gray40", 
                                                      '-1' = "brown4")) +
  scale_fill_manual(name="\u0394Canopy", values = c('1' = "forestgreen",
                                                    '0' = "gray40", 
                                                    '-1' = "brown4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
# theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
# guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Temperature and Canopy - Rarefied conditional effects.jpg", sep = ""),
       plot=tempcanplotR,
       device = "jpg",
       width=7, height=5, dpi=600)


# Abundance
tempcanplotA <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = ab.tempcan, aes(x = temp.upper, y = estimate__, color = effect2__)) +
  geom_ribbon(data = ab.tempcan, aes(x = temp.upper, y = estimate__, ymin = lower__,
                                       ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Temperature Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2)) +
  # scale_x_continuous(name="\u0394Temperature (\u00b0C)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Abundance Trend", limits = c(-0.0125, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Canopy", values = c('1' = "forestgreen",
                                                      '0' = "gray40", 
                                                      '-1' = "brown4")) +
  scale_fill_manual(name="\u0394Canopy", values = c('1' = "forestgreen",
                                                    '0' = "gray40", 
                                                    '-1' = "brown4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
# theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
# guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Temperature and Canopy - Abundance conditional effects.jpg", sep = ""),
       plot=tempcanplotA,
       device = "jpg",
       width=7, height=5, dpi=600)



######################
## Canopy :: Precip ##
######################

# Make the dfs

obs.precipcan <- conditional_effects(observed, effects = "precip.upper:canopy.upper")
obs.precipcan <- as.data.frame(obs.precipcan$`precip.upper:canopy.upper`)
obs.precipcan$Type <- "observed"
rare.precipcan <- conditional_effects(rarefied, effects = "precip.upper:canopy.upper")
rare.precipcan <- as.data.frame(rare.precipcan$`precip.upper:canopy.upper`)
rare.precipcan$Type <- "rarefied"
ab.precipcan <- conditional_effects(abundance, effects = "precip.upper:canopy.upper")
ab.precipcan <- as.data.frame(ab.precipcan$`precip.upper:canopy.upper`)
ab.precipcan$Type <- "abundance"

# effect1__ is the range of precip slope values
# effect2__ is the categorical values of canopy: 1, 0, -1

# Categorical levels are not exactly 1, 0 and negative 1 so they need to be recoded
# First change to numeric then round, then convert back to factor
obs.precipcan$effect2__ <- factor(round(as.numeric(as.character(obs.precipcan$effect2__))))
rare.precipcan$effect2__ <- factor(round(as.numeric(as.character(rare.precipcan$effect2__))))
ab.precipcan$effect2__ <- factor(round(as.numeric(as.character(ab.precipcan$effect2__))))

# FYI: effect1__ is identical to whatever first varying
# parameter of interest is)

range(obs.precipcan$precip.upper)
range(obs.precipcan$estimate__)
#-0.0027  0.0035
range(rare.precipcan$estimate__)
#-0.0004 0.0033
range(ab.precipcan$estimate__)
# -0.01 -0.0042

# Observed
precipcanplot <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = obs.precipcan, aes(x = precip.upper, y = estimate__, color = effect2__)) +
  geom_ribbon(data = obs.precipcan, aes(x = precip.upper, y = estimate__, ymin = lower__,
                                      ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Precipitation Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  # scale_x_continuous(name="\u0394Precipitation (mm)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Observed Richness Trend", limits = c(-0.0125, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Canopy", values = c('1' = "forestgreen",
                                                      '0' = "gray40", 
                                                      '-1' = "brown4")) +
  scale_fill_manual(name="\u0394Canopy", values = c('1' = "forestgreen",
                                                    '0' = "gray40", 
                                                    '-1' = "brown4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
# theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
# guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Precipitation and Canopy - Observed conditional effects.jpg", sep = ""),
       plot=precipcanplot,
       device = "jpg",
       width=7, height=5, dpi=600)

# Rarefied
precipcanplotR <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = rare.precipcan, aes(x = precip.upper, y = estimate__, color = effect2__)) +
  geom_ribbon(data = rare.precipcan, aes(x = precip.upper, y = estimate__, ymin = lower__,
                                       ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Precipitation Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  # scale_x_continuous(name="\u0394Precipitation (mm)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Rarefied Richness Trend", limits = c(-0.0125, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Canopy", values = c('1' = "forestgreen",
                                                      '0' = "gray40", 
                                                      '-1' = "brown4")) +
  scale_fill_manual(name="\u0394Canopy", values = c('1' = "forestgreen",
                                                    '0' = "gray40", 
                                                    '-1' = "brown4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
# theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
# guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Precipitation and Canopy - Rarefied conditional effects.jpg", sep = ""),
       plot=precipcanplotR,
       device = "jpg",
       width=7, height=5, dpi=600)


# Abundance
precipcanplotA <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = ab.precipcan, aes(x = precip.upper, y = estimate__, color = effect2__)) +
  geom_ribbon(data = ab.precipcan, aes(x = precip.upper, y = estimate__, ymin = lower__,
                                     ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Precipitation Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  # scale_x_continuous(name="\u0394Precipitation (mm)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Abundance Trend", limits = c(-0.0125, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Canopy", values = c('1' = "forestgreen",
                                                      '0' = "gray40", 
                                                      '-1' = "brown4")) +
  scale_fill_manual(name="\u0394Canopy", values = c('1' = "forestgreen",
                                                    '0' = "gray40", 
                                                    '-1' = "brown4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
# theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
# guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Precipitation and Canopy - Abundance conditional effects.jpg",
             sep = ""),
       plot=precipcanplotA,
       device = "jpg",
       width=7, height=5, dpi=600)



####################
## Crop :: Temp ##
####################

# Make the dfs

obs.tempcrop <- conditional_effects(observed, effects = "temp.upper:crop.upper")
obs.tempcrop <- as.data.frame(obs.tempcrop$`temp.upper:crop.upper`)
obs.tempcrop$Type <- "observed"
rare.tempcrop <- conditional_effects(rarefied, effects = "temp.upper:crop.upper")
rare.tempcrop <- as.data.frame(rare.tempcrop$`temp.upper:crop.upper`)
rare.tempcrop$Type <- "rarefied"
ab.tempcrop <- conditional_effects(abundance, effects = "temp.upper:crop.upper")
ab.tempcrop <- as.data.frame(ab.tempcrop$`temp.upper:crop.upper`)
ab.tempcrop$Type <- "abundance"

# effect1__ is the range of temperature slope values
# effect2__ is the categorical values of canopy: 1, 0, -1

# FYI: effect1__ is identical to whatever first varying
# parameter of interest is)

# Categorical levels are not exactly 1, 0 and negative 1 so they need to be recoded
# First change to numeric then round, then convert back to factor
obs.tempcrop$effect2__ <- factor(round(as.numeric(as.character(obs.tempcrop$effect2__))))
rare.tempcrop$effect2__ <- factor(round(as.numeric(as.character(rare.tempcrop$effect2__))))
ab.tempcrop$effect2__ <- factor(round(as.numeric(as.character(ab.tempcrop$effect2__))))

range(obs.tempcrop$estimate__)
range(rare.tempcrop$estimate__)
range(ab.tempcrop$estimate__)

# Observed
tempcropplot <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = obs.tempcrop, aes(x = temp.upper, y = estimate__, color = effect2__)) +
  geom_ribbon(data = obs.tempcrop, aes(x = temp.upper, y = estimate__, ymin = lower__,
                                      ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Temperature Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2)) +
  # scale_x_continuous(name="\u0394Temperature (\u00b0C)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Observed Richness Trend", limits = c(-0.0125, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Cropland", values = c('1' = "orange1",
                                                        '0' = "gray40", 
                                                        '-1' = "turquoise4")) +
  scale_fill_manual(name="\u0394Cropland", values = c('1' = "orange1",
                                                      '0' = "gray40", 
                                                      '-1' = "turquoise4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
# theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
# guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Temperature and Cropland - Observed conditional effects.jpg", sep = ""),
       plot=tempcropplot,
       device = "jpg",
       width=7, height=5, dpi=600)

# Rarefied
tempcropplotR <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = rare.tempcrop, aes(x = temp.upper, y = estimate__, color = effect2__)) +
  geom_ribbon(data = rare.tempcrop, aes(x = temp.upper, y = estimate__, ymin = lower__,
                                       ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Temperature Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2)) +
  # scale_x_continuous(name="\u0394Temperature (\u00b0C)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Rarefied Richness Trend", limits = c(-0.0125, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Cropland", values = c('1' = "orange1",
                                                        '0' = "gray40", 
                                                        '-1' = "turquoise4")) +
  scale_fill_manual(name="\u0394Cropland", values = c('1' = "orange1",
                                                      '0' = "gray40", 
                                                      '-1' = "turquoise4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
# theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
# guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Temperature and Cropland - Rarefied conditional effects.jpg", sep = ""),
       plot=tempcropplotR,
       device = "jpg",
       width=7, height=5, dpi=600)


# Abundance
tempcropplotA <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = ab.tempcrop, aes(x = temp.upper, y = estimate__, color = effect2__)) +
  geom_ribbon(data = ab.tempcrop, aes(x = temp.upper, y = estimate__, ymin = lower__,
                                     ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Temperature Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2)) +
  # scale_x_continuous(name="\u0394Temperature (\u00b0C)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Abundance Trend", limits = c(-0.0125, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Cropland", values = c('1' = "orange1",
                                                        '0' = "gray40", 
                                                        '-1' = "turquoise4")) +
  scale_fill_manual(name="\u0394Cropland", values = c('1' = "orange1",
                                                      '0' = "gray40", 
                                                      '-1' = "turquoise4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
# theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
# guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Temperature and Cropland - Abundance conditional effects.jpg",
             sep = ""),
       plot=tempcropplotA,
       device = "jpg",
       width=7, height=5, dpi=600)



######################
## Crop :: Precip ##
######################

# Make the dfs

obs.precipcrop <- conditional_effects(observed, effects = "precip.upper:crop.upper")
obs.precipcrop <- as.data.frame(obs.precipcrop$`precip.upper:crop.upper`)
obs.precipcrop$Type <- "observed"
rare.precipcrop <- conditional_effects(rarefied, effects = "precip.upper:crop.upper")
rare.precipcrop <- as.data.frame(rare.precipcrop$`precip.upper:crop.upper`)
rare.precipcrop$Type <- "rarefied"
ab.precipcrop <- conditional_effects(abundance, effects = "precip.upper:crop.upper")
ab.precipcrop <- as.data.frame(ab.precipcrop$`precip.upper:crop.upper`)
ab.precipcrop$Type <- "abundance"

# effect1__ is the range of precip slope values
# effect2__ is the categorical values of canopy: 1, 0, -1

# FYI: effect1__ is identical to whatever first varying
# parameter of interest is)

# Categorical levels are not exactly 1, 0 and negative 1 so they need to be recoded
# First change to numeric then round, then convert back to factor
obs.precipcrop$effect2__ <- factor(round(as.numeric(as.character(obs.precipcrop$effect2__))))
rare.precipcrop$effect2__ <- factor(round(as.numeric(as.character(rare.precipcrop$effect2__))))
ab.precipcrop$effect2__ <- factor(round(as.numeric(as.character(ab.precipcrop$effect2__))))

range(obs.precipcrop$precip.upper)
range(obs.precipcrop$estimate__)
#-0.0027  0.0024
range(rare.precipcrop$estimate__)
#-0.0006 0.0026
range(ab.precipcrop$estimate__)
# -0.01 -0.0046

# Observed
precipcropplot <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = obs.precipcrop, aes(x = precip.upper, y = estimate__, color = effect2__)) +
  geom_ribbon(data = obs.precipcrop, aes(x = precip.upper, y = estimate__, ymin = lower__,
                                        ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Precipitation Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  # scale_x_continuous(name="\u0394Precipitation (mm)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Observed Richness Trend", limits = c(-0.0125, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Cropland", values = c('1' = "orange1",
                                                        '0' = "gray40", 
                                                        '-1' = "turquoise4")) +
  scale_fill_manual(name="\u0394Cropland", values = c('1' = "orange1",
                                                      '0' = "gray40", 
                                                      '-1' = "turquoise4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
# theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
# guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Precipitation and Cropland - Observed conditional effects.jpg",
             sep = ""),
       plot=precipcropplot,
       device = "jpg",
       width=7, height=5, dpi=600)

# Rarefied
precipcropplotR <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = rare.precipcrop, aes(x = precip.upper, y = estimate__, color = effect2__)) +
  geom_ribbon(data = rare.precipcrop, aes(x = precip.upper, y = estimate__, ymin = lower__,
                                         ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Precipitation Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  # scale_x_continuous(name="\u0394Precipitation (mm)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Rarefied Richness Trend", limits = c(-0.0125, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Cropland", values = c('1' = "orange1",
                                                        '0' = "gray40", 
                                                        '-1' = "turquoise4")) +
  scale_fill_manual(name="\u0394Cropland", values = c('1' = "orange1",
                                                      '0' = "gray40", 
                                                      '-1' = "turquoise4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
# theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
# guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Precipitation and Cropland - Rarefied conditional effects.jpg",
             sep = ""),
       plot=precipcropplotR,
       device = "jpg",
       width=7, height=5, dpi=600)


# Abundance
precipcropplotA <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = ab.precipcrop, aes(x = precip.upper, y = estimate__, color = effect2__)) +
  geom_ribbon(data = ab.precipcrop, aes(x = precip.upper, y = estimate__, ymin = lower__,
                                       ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Precipitation Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  # scale_x_continuous(name="\u0394Precipitation (mm)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Abundance Trend", limits = c(-0.0125, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Cropland", values = c('1' = "orange1",
                                                        '0' = "gray40", 
                                                        '-1' = "turquoise4")) +
  scale_fill_manual(name="\u0394Cropland", values = c('1' = "orange1",
                                                      '0' = "gray40", 
                                                      '-1' = "turquoise4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
# theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
# guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Precipitation and Cropland - Abundance conditional effects.jpg",
             sep = ""),
       plot=precipcropplotA,
       device = "jpg",
       width=7, height=5, dpi=600)


####################
## Urban :: Temp ##
####################

# Make the dfs

obs.tempurban <- conditional_effects(observed, effects = "temp.upper:urban.upper")
obs.tempurban <- as.data.frame(obs.tempurban$`temp.upper:urban.upper`)
obs.tempurban$Type <- "observed"
rare.tempurban <- conditional_effects(rarefied, effects = "temp.upper:urban.upper")
rare.tempurban <- as.data.frame(rare.tempurban$`temp.upper:urban.upper`)
rare.tempurban$Type <- "rarefied"
ab.tempurban <- conditional_effects(abundance, effects = "temp.upper:urban.upper")
ab.tempurban <- as.data.frame(ab.tempurban$`temp.upper:urban.upper`)
ab.tempurban$Type <- "abundance"

# effect1__ is the range of temp slope values
# effect2__ is the categorical values of canopy: 1, 0, -1

# FYI: effect1__ is identical to whatever first varying
# parameter of interest is)

# Categorical levels are not exactly 1, 0 and negative 1 so they need to be recoded
# First change to numeric then round, then convert back to factor
obs.tempurban$effect2__ <- factor(round(as.numeric(as.character(obs.tempurban$effect2__))))
rare.tempurban$effect2__ <- factor(round(as.numeric(as.character(rare.tempurban$effect2__))))
ab.tempurban$effect2__ <- factor(round(as.numeric(as.character(ab.tempurban$effect2__))))

range(obs.tempurban$estimate__)
range(rare.tempurban$estimate__)
range(ab.tempurban$estimate__)
# -0.011 0.0024

# Observed
tempurbanplot <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = obs.tempurban, aes(x = temp.upper, y = estimate__, color = effect2__)) +
  geom_ribbon(data = obs.tempurban, aes(x = temp.upper, y = estimate__, ymin = lower__,
                                       ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Temperature Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2)) +
  # scale_x_continuous(name="\u0394Temperature (\u00b0C)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Observed Richness Trend", limits = c(-0.0125, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Urban", values = c('1' = "red3",
                                                     '0' = "gray40", 
                                                     '-1' = "blueviolet")) +
  scale_fill_manual(name="\u0394Urban", values = c('1' = "red3",
                                                   '0' = "gray40", 
                                                   '-1' = "blueviolet")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
# theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
# guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Temperature and Urban - Observed conditional effects.jpg",
             sep = ""),
       plot=tempurbanplot,
       device = "jpg",
       width=7, height=5, dpi=600)

# Rarefied
tempurbanplotR <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = rare.tempurban, aes(x = temp.upper, y = estimate__, color = effect2__)) +
  geom_ribbon(data = rare.tempurban, aes(x = temp.upper, y = estimate__, ymin = lower__,
                                        ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Temperature Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2)) +
  # scale_x_continuous(name="\u0394Temperature (\u00b0C)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Rarefied Richness Trend", limits = c(-0.0125, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Urban", values = c('1' = "red3",
                                                     '0' = "gray40", 
                                                     '-1' = "blueviolet")) +
  scale_fill_manual(name="\u0394Urban", values = c('1' = "red3",
                                                   '0' = "gray40", 
                                                   '-1' = "blueviolet")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
# theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
# guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Temperature and Urban - Rarefied conditional effects.jpg",
             sep = ""),
       plot=tempurbanplotR,
       device = "jpg",
       width=7, height=5, dpi=600)


# Abundance
tempurbanplotA <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = ab.tempurban, aes(x = temp.upper, y = estimate__, color = effect2__)) +
  geom_ribbon(data = ab.tempurban, aes(x = temp.upper, y = estimate__, ymin = lower__,
                                      ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Temperature Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2)) +
  # scale_x_continuous(name="\u0394Temperature (\u00b0C)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Abundance Trend", limits = c(-0.0125, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Urban", values = c('1' = "red3",
                                                     '0' = "gray40", 
                                                     '-1' = "blueviolet")) +
  scale_fill_manual(name="\u0394Urban", values = c('1' = "red3",
                                                   '0' = "gray40", 
                                                   '-1' = "blueviolet")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
# theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
# guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Temperature and Urban - Abundance conditional effects.jpg",
             sep = ""),
       plot=tempurbanplotA,
       device = "jpg",
       width=7, height=5, dpi=600)


######################
## Urban :: Precip ##
######################

# Make the dfs

obs.precipurban <- conditional_effects(observed, effects = "precip.upper:urban.upper")
obs.precipurban <- as.data.frame(obs.precipurban$`precip.upper:urban.upper`)
obs.precipurban$Type <- "observed"
rare.precipurban <- conditional_effects(rarefied, effects = "precip.upper:urban.upper")
rare.precipurban <- as.data.frame(rare.precipurban$`precip.upper:urban.upper`)
rare.precipurban$Type <- "rarefied"
ab.precipurban <- conditional_effects(abundance, effects = "precip.upper:urban.upper")
ab.precipurban <- as.data.frame(ab.precipurban$`precip.upper:urban.upper`)
ab.precipurban$Type <- "abundance"

# effect1__ is the range of precip slope values
# effect2__ is the categorical values of canopy: 1, 0, -1

# FYI: effect1__ is identical to whatever first varying
# parameter of interest is)

# Categorical levels are not exactly 1, 0 and negative 1 so they need to be recoded
# First change to numeric then round, then convert back to factor
obs.precipurban$effect2__ <- factor(round(as.numeric(as.character(obs.precipurban$effect2__))))
rare.precipurban$effect2__ <- factor(round(as.numeric(as.character(rare.precipurban$effect2__))))
ab.precipurban$effect2__ <- factor(round(as.numeric(as.character(ab.precipurban$effect2__))))

range(obs.precipurban$precip.upper)
range(obs.precipurban$estimate__)
#-0.0027  0.0024
range(rare.precipurban$estimate__)
#-0.0006 0.0026
range(ab.precipurban$estimate__)
# -0.01 -0.0046

# Observed
precipurbanplot <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = obs.precipurban, aes(x = precip.upper, y = estimate__, color = effect2__)) +
  geom_ribbon(data = obs.precipurban, aes(x = precip.upper, y = estimate__, ymin = lower__,
                                         ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Precipitation Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  # scale_x_continuous(name="\u0394Precipitation (mm)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Observed Richness Trend", limits = c(-0.0125, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Urban", values = c('1' = "red3",
                                                     '0' = "gray40", 
                                                     '-1' = "blueviolet")) +
  scale_fill_manual(name="\u0394Urban", values = c('1' = "red3",
                                                   '0' = "gray40", 
                                                   '-1' = "blueviolet")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
# theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
# guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Precipitation and Urban - Observed conditional effects.jpg",
             sep = ""),
       plot=precipurbanplot,
       device = "jpg",
       width=7, height=5, dpi=600)

# Rarefied
precipurbanplotR <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = rare.precipurban, aes(x = precip.upper, y = estimate__, color = effect2__)) +
  geom_ribbon(data = rare.precipurban, aes(x = precip.upper, y = estimate__, ymin = lower__,
                                          ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Precipitation Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  # scale_x_continuous(name="\u0394Precipitation (mm)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Rarefied Richness Trend", limits = c(-0.0125, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Urban", values = c('1' = "red3",
                                                     '0' = "gray40", 
                                                     '-1' = "blueviolet")) +
  scale_fill_manual(name="\u0394Urban", values = c('1' = "red3",
                                                   '0' = "gray40", 
                                                   '-1' = "blueviolet")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
# theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
# guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Precipitation and Urban - Rarefied conditional effects.jpg",
             sep = ""),
       plot=precipurbanplotR,
       device = "jpg",
       width=7, height=5, dpi=600)


# Abundance
precipurbanplotA <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = ab.precipurban, aes(x = precip.upper, y = estimate__, color = effect2__)) +
  geom_ribbon(data = ab.precipurban, aes(x = precip.upper, y = estimate__, ymin = lower__,
                                        ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Precipitation Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  # scale_x_continuous(name="\u0394Precipitation (mm)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Abundance Trend", limits = c(-0.0125, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Urban", values = c('1' = "red3",
                                                     '0' = "gray40", 
                                                     '-1' = "blueviolet")) +
  scale_fill_manual(name="\u0394Urban", values = c('1' = "red3",
                                                   '0' = "gray40", 
                                                   '-1' = "blueviolet")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
# theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
# guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Precipitation and Urban - Abundance conditional effects.jpg", sep = ""),
       plot=precipurbanplotA,
       device = "jpg",
       width=7, height=5, dpi=600)



######################
## Water :: Precip  ##
######################

# Make the dfs

obs.precipwater <- conditional_effects(observed, effects = "precip.upper:water.upper")
obs.precipwater <- as.data.frame(obs.precipwater$`precip.upper:water.upper`)
obs.precipwater$Type <- "observed"
rare.precipwater <- conditional_effects(rarefied, effects = "precip.upper:water.upper")
rare.precipwater <- as.data.frame(rare.precipwater$`precip.upper:water.upper`)
rare.precipwater$Type <- "rarefied"
ab.precipwater <- conditional_effects(abundance, effects = "precip.upper:water.upper")
ab.precipwater <- as.data.frame(ab.precipwater$`precip.upper:water.upper`)
ab.precipwater$Type <- "abundance"

# effect1__ is the range of precip slope values
# effect2__ is the categorical values of water: 1, 0, -1

# FYI: effect1__ is identical to whatever first varying
# parameter of interest is)

# Categorical levels are not exactly 1, 0 and negative 1 so they need to be recoded
# First change to numeric then round, then convert back to factor
obs.precipwater$effect2__ <- factor(round(as.numeric(as.character(obs.precipwater$effect2__))))
rare.precipwater$effect2__ <- factor(round(as.numeric(as.character(rare.precipwater$effect2__))))
ab.precipwater$effect2__ <- factor(round(as.numeric(as.character(ab.precipwater$effect2__))))

range(obs.precipwater$precip.upper)
range(obs.precipwater$estimate__)
#-0.0036  0.0026
range(rare.precipwater$estimate__)
#-0.0006 0.0026
range(ab.precipwater$estimate__)
# -0.0111 -0.0046

# Observed
precipwaterplot <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = obs.precipwater, aes(x = precip.upper, y = estimate__, color = effect2__)) +
  geom_ribbon(data = obs.precipwater, aes(x = precip.upper, y = estimate__, ymin = lower__,
                                          ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Precipitation Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  # scale_x_continuous(name="\u0394Precipitation (mm)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Observed Richness Trend", limits = c(-0.0125, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Water", values = c('1' = "blue4",
                                                     '0' = "gray40", 
                                                     '-1' = "turquoise4")) +
  scale_fill_manual(name="\u0394Water", values = c('1' = "blue4",
                                                   '0' = "gray40", 
                                                   '-1' = "turquoise4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
# theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
# guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Precipitation and Water - Observed conditional effects.jpg",
             sep = ""),
       plot=precipwaterplot,
       device = "jpg",
       width=7, height=5, dpi=600)

# Rarefied
precipwaterplotR <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = rare.precipwater, aes(x = precip.upper, y = estimate__, color = effect2__)) +
  geom_ribbon(data = rare.precipwater, aes(x = precip.upper, y = estimate__, ymin = lower__,
                                          ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Precipitation Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  # scale_x_continuous(name="\u0394Precipitation (mm)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Rarefied Richness Trend", limits = c(-0.0125, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Water", values = c('1' = "blue4",
                                                     '0' = "gray40", 
                                                     '-1' = "turquoise4")) +
  scale_fill_manual(name="\u0394Water", values = c('1' = "blue4",
                                                   '0' = "gray40", 
                                                   '-1' = "turquoise4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
# theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
# guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Precipitation and Water - Rarefied conditional effects.jpg",
             sep = ""),
       plot=precipwaterplotR,
       device = "jpg",
       width=7, height=5, dpi=600)


# Abundance
precipwaterplotA <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = ab.precipwater, aes(x = precip.upper, y = estimate__, color = effect2__)) +
  geom_ribbon(data = ab.precipwater, aes(x = precip.upper, y = estimate__, ymin = lower__,
                                           ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Precipitation Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  # scale_x_continuous(name="\u0394Precipitation (mm)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Abundance Trend", limits = c(-0.0125, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Water", values = c('1' = "blue4",
                                                     '0' = "gray40", 
                                                     '-1' = "turquoise4")) +
  scale_fill_manual(name="\u0394Water", values = c('1' = "blue4",
                                                   '0' = "gray40", 
                                                   '-1' = "turquoise4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
# theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
# guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Precipitation and Water - Abundance conditional effects.jpg",
             sep = ""),
       plot=precipwaterplotA,
       device = "jpg",
       width=7, height=5, dpi=600)



######################
## Water :: temp    ##
######################

# Make the dfs

obs.tempwater <- conditional_effects(observed, effects = "temp.upper:water.upper")
obs.tempwater <- as.data.frame(obs.tempwater$`temp.upper:water.upper`)
obs.tempwater$Type <- "observed"
rare.tempwater <- conditional_effects(rarefied, effects = "temp.upper:water.upper")
rare.tempwater <- as.data.frame(rare.tempwater$`temp.upper:water.upper`)
rare.tempwater$Type <- "rarefied"
ab.tempwater <- conditional_effects(abundance, effects = "temp.upper:water.upper")
ab.tempwater <- as.data.frame(ab.tempwater$`temp.upper:water.upper`)
ab.tempwater$Type <- "abundance"

# effect1__ is the range of temp slope values
# effect2__ is the categorical values of water: 1, 0, -1

# FYI: effect1__ is identical to whatever first varying
# parameter of interest is)

# Categorical levels are not exactly 1, 0 and negative 1 so they need to be recoded
# First change to numeric then round, then convert back to factor
obs.tempwater$effect2__ <- factor(round(as.numeric(as.character(obs.tempwater$effect2__))))
rare.tempwater$effect2__ <- factor(round(as.numeric(as.character(rare.tempwater$effect2__))))
ab.tempwater$effect2__ <- factor(round(as.numeric(as.character(ab.tempwater$effect2__))))

range(obs.tempwater$temp.upper)
range(obs.tempwater$estimate__)
#-0.0036  0.0026
range(rare.tempwater$estimate__)
#-0.0006 0.0026
range(ab.tempwater$estimate__)
# -0.0111 -0.0046

# Observed
tempwaterplot <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = obs.tempwater, aes(x = temp.upper, y = estimate__, color = effect2__)) +
  geom_ribbon(data = obs.tempwater, aes(x = temp.upper, y = estimate__, ymin = lower__,
                                          ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Temperature Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2)) +
  # scale_x_continuous(name="\u0394Temperature (\u00b0C)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Observed Richness Trend", limits = c(-0.0125, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Water", values = c('1' = "blue4",
                                                     '0' = "gray40", 
                                                     '-1' = "turquoise4")) +
  scale_fill_manual(name="\u0394Water", values = c('1' = "blue4",
                                                   '0' = "gray40", 
                                                   '-1' = "turquoise4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
# theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
# guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Temperature and Water - Observed conditional effects.jpg",
             sep = ""),
       plot=tempwaterplot,
       device = "jpg",
       width=7, height=5, dpi=600)

# Rarefied
tempwaterplotR <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = rare.tempwater, aes(x = temp.upper, y = estimate__, color = effect2__)) +
  geom_ribbon(data = rare.tempwater, aes(x = temp.upper, y = estimate__, ymin = lower__,
                                           ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Temperature Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2)) +
  # scale_x_continuous(name="\u0394Temperature (\u00b0C)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Rarefied Richness Trend", limits = c(-0.0125, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Water", values = c('1' = "blue4",
                                                     '0' = "gray40", 
                                                     '-1' = "turquoise4")) +
  scale_fill_manual(name="\u0394Water", values = c('1' = "blue4",
                                                   '0' = "gray40", 
                                                   '-1' = "turquoise4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
# theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
# guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Temperature and Water - Rarefied conditional effects.jpg",
             sep = ""),
       plot=tempwaterplotR,
       device = "jpg",
       width=7, height=5, dpi=600)


# Abundance
tempwaterplotA <- ggplot() +
  # Zero lines
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_line(data = ab.tempwater, aes(x = temp.upper, y = estimate__, color = effect2__)) +
  geom_ribbon(data = ab.tempwater, aes(x = temp.upper, y = estimate__, ymin = lower__,
                                         ymax = upper__, fill = effect2__), alpha = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.x = element_text(size=22, face="bold", color="gray30")) +
  theme(axis.title.y = element_text(size=22, face="bold", color="gray30")) +
  scale_x_continuous(name="Temperature Trend",
                     breaks=c(-3, -2, -1, 0, 1, 2)) +
  # scale_x_continuous(name="\u0394Temperature (\u00b0C)/Year",
  #                    breaks=c(-3, -2, -1, 0, 1, 2)) +
  theme(axis.title.y.right = element_text(vjust = 1.7)) +
  scale_y_continuous(name="Abundance Trend", limits = c(-0.0125, 0.005),
                     breaks = c(-0.010, -0.005, 0.000, 0.005),
                     sec.axis = dup_axis(name = "% Change",
                                         labels = c("-2.28", "-1.14", "0.00", "1.16"))) +
  scale_colour_manual(name="\u0394Water", values = c('1' = "blue4",
                                                     '0' = "gray40", 
                                                     '-1' = "turquoise4")) +
  scale_fill_manual(name="\u0394Water", values = c('1' = "blue4",
                                                   '0' = "gray40", 
                                                   '-1' = "turquoise4")) +
  # theme(legend.position = c(0.80, 0.85)) +
  theme(legend.position = "none")
  # theme(legend.text = element_text(size=18), legend.title = element_text(size = 18)) +
  # guides(colour = guide_legend(override.aes = (list(size=3))))

ggsave(paste(path2, "Temperature and Water - Abundance conditional effects.jpg",
             sep = ""),
       plot=tempwaterplotA,
       device = "jpg",
       width=7, height=5, dpi=600)

