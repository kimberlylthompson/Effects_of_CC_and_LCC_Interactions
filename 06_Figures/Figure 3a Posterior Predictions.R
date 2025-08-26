#################################################################################################
########                                                                                 ########
########                      Graph of the posterior predictive                          ######## 
########                    distributions for each biodiv response                       ########
#################################################################################################

# Author: Kimberly Thompson

# This code uses the three brms models constructed for observed richness, rarefied richness,
# abundance (1 model for each) and creates graphs of the posterior predictions.


# Plot created:
# Figure 


# Website used for tutorial:
# http://mjskay.github.io/tidybayes/articles/tidy-brms.html



########## clean workspace and load required packages ####################

# clean workspace to improve efficiency: #
# rm(list = ls() ) 
# gc() #releases memory

# Libraries actually loaded: brms, ggplot, tidybayes, tidyverse


library(brms)
library(tidybayes)
library(cowplot)
library(tidyverse) # Data organization

library(ggplot2)

library(viridis)

library(MetBrewer)
library(colorblindcheck)


###############################################
###                                         ###
###             Data Loading                ###
###                                         ###
###############################################

# Load the three model objects
## Load the three model objects
path <- "01_Analysis/Modeling_BRMS/Model Objects/CC and LUCC interactions only/"
observed <- readRDS(paste(path, "Observed Richness_CC LUCC interactions.rds", sep = ""))
rarefied <- readRDS(paste(path, "Rarefied Richness_CC LUCC interactions.rds", sep = ""))
abundance <- readRDS(paste(path, "Abundance_CC LUCC interactions.rds", sep = ""))




###############################################
###                                         ###
###           Data Preparation              ###
###                                         ###
###############################################

# Set theme
theme_set(theme_tidybayes() + panel_border())

get_variables(observed)




###############################
### Full Model estimates of each biodiversity trend

obs.predicted <- predict(observed)
rare.predicted <- predict(rarefied)
ab.predicted <- predict(abundance)

obs.predicted <- obs.predicted %>%
  as.data.frame()

rare.predicted <- rare.predicted %>%
  as.data.frame()

ab.predicted <- ab.predicted %>%
  as.data.frame()



obs.predicted$Type <- "observed"
rare.predicted$Type <- "rarefied"
ab.predicted$Type <- "abundance"

# Summarizing to find the mean predicted values and its associated error
mean.obs.pred <- mean(obs.predicted$Estimate)
obs.error <- sd(obs.predicted$Estimate) / sqrt(length(obs.predicted$Estimate))
lower_bound.obs <- quantile(obs.predicted$Estimate, probs = 0.025)
upper_bound.obs <- quantile(obs.predicted$Estimate, probs = 0.975)

mean.rare.pred <- mean(rare.predicted$Estimate)
rare.error <- sd(rare.predicted$Estimate) / sqrt(length(rare.predicted$Estimate))
lower_bound.rare <- quantile(rare.predicted$Estimate, probs = 0.025)
upper_bound.rare <- quantile(rare.predicted$Estimate, probs = 0.975)

mean.ab.pred <- mean(ab.predicted$Estimate)
ab.error <- sd(ab.predicted$Estimate) / sqrt(length(ab.predicted$Estimate))
lower_bound.ab <- quantile(ab.predicted$Estimate, probs = 0.025)
upper_bound.ab <- quantile(ab.predicted$Estimate, probs = 0.975)

# finding % change values
(10^(mean.obs.pred * 1)-1) * 100 
(10^(lower_bound.obs * 1)-1) * 100
(10^(upper_bound.obs * 1)-1) * 100
# -0.01 [-0.60, 0.34] 95% credible interval

(10^(mean.rare.pred * 1)-1) * 100 
(10^(lower_bound.rare * 1)-1) * 100
(10^(upper_bound.rare * 1)-1) * 100
#0.15 [-0.28, 0.46]


(10^(mean.ab.pred * 1)-1) * 100 
(10^(lower_bound.ab * 1)-1) * 100
(10^(upper_bound.ab * 1)-1) * 100
# -1.60 [-2.36, -0.89]



# For each x axis label, get the % change as well
# This formula comes from Roel - notes about it are in a word doc 
# I:\MAS\01_projects\iUpdate\01_Analysis\Interpreting log slopes as percent change.docx
# -0.008
(10^(-0.008 * 1)-1) * 100 # -1.83
# -0.006
(10^(-0.006 * 1)-1) * 100 # -1.37
# -0.004
(10^(-0.004 * 1)-1) * 100 # -0.92
# -0.002
(10^(-0.002 * 1)-1) * 100 # -0.46
# 0.00
(10^(0 * 1) - 1) * 100 # 0
# 0.002
(10^(0.002 * 1) - 1) * 100 # 0.46


# Axis labels for -0.009, -0.006, -0.003, 0.003
# -0.009
(10^(-0.009 * 1)-1) * 100 # -2.05

# -0.006
(10^(-0.006 * 1)-1) * 100 # -1.37

# -0.003
(10^(-0.003 * 1)-1) * 100 # -0.69

# 0.003
(10^(0.003 * 1)-1) * 100 # 0.69

colorblindcheck::palette_check(met.brewer("Egypt", n = 4), plot = TRUE)


# Make the graph - original x axis
post.predicted <- ggplot() +
  geom_vline(aes(xintercept = 0), linewidth = 1, color = "black", alpha = 0.7) +
  stat_halfeye(data = obs.predicted, aes(y = Type, x = Estimate,
                                         xmin = Q2.5, xmax = Q97.5),
               slab_fill = met.brewer("Egypt", n = 4)[2], slab_alpha = 0.75, 
               slab_color = "black", slab_size = 1,
               interval_color = "black", interval_size = 10) +
  stat_halfeye(data = rare.predicted, aes(y = Type, x = Estimate,
                                          xmin = Q2.5, xmax = Q97.5),
               slab_fill = met.brewer("Egypt", n = 4)[3], slab_alpha = 0.75,
               slab_color = "black", slab_size = 1,
               interval_color = "black", interval_size = 10) +
  stat_halfeye(data = ab.predicted, aes(y = Type, x = Estimate,
                                        xmin = Q2.5, xmax = Q97.5),
               slab_fill = met.brewer("Egypt", n = 4)[4], slab_alpha = 0.75,
               slab_color = "black", slab_size = 1,
               interval_color = "black", interval_size = 10) +
  scale_x_continuous(limits = c(-0.01, 0.003),
                     name="\n% Change/Year",
                     breaks = c(-0.008, -0.006, -0.004, -0.002, 0, 0.002),
                     labels = c("-1.83%", "-1.37%", "-0.92%",
                                "-0.46%", "0.00%", "0.46%")) +
  # Ggplot reads order from bottom to top rather than top to bottom
  scale_y_discrete(name = "", limits = c("abundance", "rarefied", "observed"),
                   labels = c("Abundance", "Rarefied\nRichness", "Observed\nRichness")) +
  theme_bw() +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.y = element_blank()) +
  theme(axis.title.x = element_text(size=24, face="bold", color="gray30"))


path2 <- "01_Analysis/Model_Plotting/Posterior Intercept Plots/"
ggsave(paste(path2,
             "Posterior Predictions of Biodiversity Responses_Main Model CC LUCC.jpg",
             sep = ""),
       plot=post.predicted, device = "jpeg",
       width=9, height=7, dpi=600)


# Make the graph - x axis for merging with posterior intercept plot
post.predicted2 <- ggplot() +
  geom_vline(aes(xintercept = 0), linewidth = 1, color = "black", alpha = 0.7) +
  stat_halfeye(data = obs.predicted, aes(y = Type, x = Estimate,
                                         xmin = Q2.5, xmax = Q97.5),
               slab_fill = met.brewer("Egypt", n = 4)[2], slab_alpha = 0.75, 
               slab_color = "black", slab_size = 1,
               interval_color = "black", interval_size = 10) +
  stat_halfeye(data = rare.predicted, aes(y = Type, x = Estimate,
                                          xmin = Q2.5, xmax = Q97.5),
               slab_fill = met.brewer("Egypt", n = 4)[3], slab_alpha = 0.75,
               slab_color = "black", slab_size = 1,
               interval_color = "black", interval_size = 10) +
  stat_halfeye(data = ab.predicted, aes(y = Type, x = Estimate,
                                        xmin = Q2.5, xmax = Q97.5),
               slab_fill = met.brewer("Egypt", n = 4)[4], slab_alpha = 0.75,
               slab_color = "black", slab_size = 1,
               interval_color = "black", interval_size = 10) +
  scale_x_continuous(limits = c(-0.01, 0.003),
                     name="\n% Change/Year",
                     breaks = c(-0.009, -0.006, -0.003, 0, 0.003),
                     labels = c("-2.05%", "-1.37%", "-0.69%",
                                "0.00%", "0.69%")) +
  # Ggplot reads order from bottom to top rather than top to bottom
  scale_y_discrete(name = "", limits = c("abundance", "rarefied", "observed"),
                   labels = c("Abundance", "Rarefied\nRichness", "Observed\nRichness")) +
  theme_bw() +
  theme(plot.margin = margin(5.5, 10.5, 5.5, 5.5, "points")) +
  theme(axis.text.x = element_text(size=22, face="bold")) +
  theme(axis.text.y = element_text(size=22, face="bold")) +
  theme(axis.title.y = element_blank()) +
  theme(axis.title.x = element_text(size=24, face="bold", color="gray30"))


ggsave(paste(path2,
             "Posterior Predictions of Biodiversity Responses_Main Model CC LUCC_longer axis.jpg",
             sep = ""),
       plot=post.predicted2, device = "jpeg",
       width=10.5, height=7, dpi=600)

