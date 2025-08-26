#################################################################################################
########                                                                                 ########
########                       Graph of the posterior intercept                          ######## 
########                    distributions for each biodiv response                       ########
#################################################################################################

# Author: Kimberly Thompson

# This code uses the three brms models constructed for observed richness, rarefied richness,
# abundance (1 model for each) and creates graphs of the posterior intercept.

# Plot created:
# Figure 1g


# Website used for tutorial:
# http://mjskay.github.io/tidybayes/articles/tidy-brms.html



########## clean workspace and load required packages ####################

# clean workspace to improve efficiency: #
# rm(list = ls() ) 
# gc() #releases memory


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
### Posterior Summary

obs.test <- observed %>%
  spread_rvars(b_Intercept)
rare.test <- rarefied %>%
  spread_rvars(b_Intercept)
ab.test <- abundance %>%
  spread_rvars(b_Intercept)

obs.test$Type <- "observed"
rare.test$Type <- "rarefied"
ab.test$Type <- "abundance"

# Finding the 95% credible intervals for the manuscript results section
obs.post.int <- spread_draws(observed, b_Intercept)
obs.CI <- quantile(obs.post.int$b_Intercept, probs = c(0.025, 0.975))

lower.obs <- (10^(obs.CI[1] * 1)-1) * 100 # -0.03
upper.obs <- (10^(obs.CI[2] * 1)-1) * 100 # 0.01


rare.post.int <- spread_draws(rarefied, b_Intercept)
rare.CI <- quantile(rare.post.int$b_Intercept, probs = c(0.025, 0.975))

lower.rare <- (10^(rare.CI[1] * 1)-1) * 100 # 0.14
upper.rare <- (10^(rare.CI[2] * 1)-1) * 100 # 0.17


ab.post.int <- spread_draws(abundance, b_Intercept)
ab.CI <- quantile(ab.post.int$b_Intercept, probs = c(0.025, 0.975))

lower.ab <- (10^(ab.CI[1] * 1)-1) * 100 # -1.63
upper.ab <- (10^(ab.CI[2] * 1)-1) * 100 # -1.54




# For each x axis label, get the % change as well
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

# Make the graph - original with its own axes
post.intercept <- ggplot() +
  geom_vline(aes(xintercept = 0), linewidth = 1, color = "black", alpha = 0.7) +
  stat_dist_halfeye(data = obs.test, aes(y = Type, dist = b_Intercept),
                    slab_fill = met.brewer("Egypt", n = 4)[2], slab_alpha = 0.75, 
                    slab_color = "black", slab_size = 1,
                    interval_color = "black", interval_size = 10) +
  stat_dist_halfeye(data = rare.test, aes(y = Type, dist = b_Intercept),
                    slab_fill = met.brewer("Egypt", n = 4)[3], slab_alpha = 0.75,
                    slab_color = "black", slab_size = 1,
                    interval_color = "black", interval_size = 10) +
  stat_dist_halfeye(data = ab.test, aes(y = Type, dist = b_Intercept),
                    slab_fill = met.brewer("Egypt", n = 4)[4], slab_alpha = 0.75,
                    slab_color = "black", slab_size = 1,
                    interval_color = "black", interval_size = 10) +
  scale_x_continuous(limits = c(-0.008, 0.0025),
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
             "Posterior Intercept of Biodiversity Responses_Main Model CC LUCC.jpg",
             sep = ""),
       plot=post.intercept, device = "jpeg",
       width=10, height=7, dpi=600)



# Make the graph - x axis limits and breaks based on what was 
# needed for prediction plot
post.intercept2 <- ggplot() +
  geom_vline(aes(xintercept = 0), linewidth = 1, color = "black", alpha = 0.7) +
  stat_dist_halfeye(data = obs.test, aes(y = Type, dist = b_Intercept),
                    slab_fill = met.brewer("Egypt", n = 4)[2], slab_alpha = 0.75, 
                    slab_color = "black", slab_size = 1,
                    interval_color = "black", interval_size = 10) +
  stat_dist_halfeye(data = rare.test, aes(y = Type, dist = b_Intercept),
                    slab_fill = met.brewer("Egypt", n = 4)[3], slab_alpha = 0.75,
                    slab_color = "black", slab_size = 1,
                    interval_color = "black", interval_size = 10) +
  stat_dist_halfeye(data = ab.test, aes(y = Type, dist = b_Intercept),
                    slab_fill = met.brewer("Egypt", n = 4)[4], slab_alpha = 0.75,
                    slab_color = "black", slab_size = 1,
                    interval_color = "black", interval_size = 10) +
  scale_x_continuous(limits = c(-0.01, 0.003),
                     name="\nTrend\n% Change",
                     breaks = c(-0.009, -0.006, -0.003, 0, 0.003),
                     labels = c("-0.009\n-2.05%", "-0.006\n-1.37%", "-0.003\n-0.69%",
                                "0.000\n0.00%", "0.003\n0.69%")) +
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
             "Posterior Intercept of Biodiversity Responses_Main Model CC LUCC_longer axis.jpg",
             sep = ""),
       plot=post.intercept2, device = "jpeg",
       width=9, height=7, dpi=600)
