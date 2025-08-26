##################################################################################################
########                                                                                  ########
########         Maps and Distributions of Richness, Rarefied Richness and Abundance      ######## 
########                        for BBS Routes - Figure 1a-f, S2a - 2c                    ########
##################################################################################################

# Author: Kimberly Thompson

# This code examines trends in biodiversity metrics for all North American Breeding Bird
# Survey routes with more than 20 years of data by creating a graph visualization 
# of which routes experienced positive, negative, or zero trends
# FOR THE TIME PERIOD OF 1992 TO 2018

# Plots created:
# Figure 2a - 2f
# Figure S3a - S3c



#######################################################
# clean workspace to improve efficiency: #
# rm(list = ls() ) 
# gc() #releases memory


library(tidyverse) # Data organization
library(lubridate) # Time values
library(data.table) # Data organization
library(ggplot2)    # graphs
library(sf)

library(grDevices)
library(RColorBrewer)
library(viridis)
library(scales)

library(colorblindcheck)



###############################################
###                                         ###
###       Data Loading & Manipulation       ###
###                                         ###
###############################################


# Read in the shapefile of the BBS routes
path <- "00_Data/processed/BBS Data/"
bbs.routes <- sf :: st_read(paste(path, "BBS Rte 20 yrs_Linestring_1992to2018.shp", sep = ""))

# Pare down file to only be unique route, geometry, country and state columns
bbs.routes <- bbs.routes[ , c("Country", "statenm", "St_Abrv", "uniq_rt", "geometry")]

# Define the coordinate system
albers = sp :: CRS("epsg:5070")

# Make sure bbs.routes are in albers
bbs.routes <- sf :: st_transform(bbs.routes, crs = albers)


# Load USA and Canada shapefile
path2 <- "00_Data/raw/"
usa.shape <- sf :: st_read(paste(path2, "USACANAB.shp", sep = ""))

# Define coordintate system as albers:
usa.shape <- sf :: st_set_crs( usa.shape, albers )


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


# Convert to sf object
master <- st_as_sf(master, coords = c("x", "y"), crs = albers)

# Crop to USA
usa.shape2 <- usa.shape[usa.shape$STATE != "AK" & usa.shape$STATE != "NWT" &
                          usa.shape$STATE != "YT" & usa.shape$STATE != "BC" &
                          usa.shape$STATE != "QUE" & usa.shape$STATE != "LAB" &
                          usa.shape$STATE != "SAS" & usa.shape$STATE != "xx" &
                          usa.shape$STATE != "MAN" & usa.shape$STATE != "NFD" &
                          usa.shape$STATE != "ONT" & usa.shape$STATE != "NS" &
                          usa.shape$STATE != "NB" & usa.shape$STATE != "PEI" &
                          usa.shape$STATE != "ALB", ]

###############################################
###                                         ###
###        Determining which slope vals     ###
###                  overlap 0              ###
###############################################

# Limit to only the columns we need - unique route, responses, and geometry
master <- master[ , c(1, 7:8, 11:12, 15:16,
                      80)]

# Add a column for no change, increasing, decreasing
# no change = (slope + CI) < 0 < (slope + confidence interval)  
# increasing = both greater than zero
# decreasing = both less than zero

# Add column for LogObs
master <- master %>%
  mutate(
    Direction_LogObs = case_when(
      (SLOPE_LogObs + SLOPE.STDERR_LogObs > 0) &
        (SLOPE_LogObs - SLOPE.STDERR_LogObs < 0) ~ "nochange",
      (SLOPE_LogObs + SLOPE.STDERR_LogObs > 0) &
        (SLOPE_LogObs - SLOPE.STDERR_LogObs > 0) ~ "increasing",
      (SLOPE_LogObs + SLOPE.STDERR_LogObs < 0) &
        (SLOPE_LogObs - SLOPE.STDERR_LogObs < 0) ~ "decreasing"
    )
  )

# Add column for LogRare
master <- master %>%
  mutate(
    Direction_LogRare = case_when(
      (SLOPE_LogRare + SLOPE.STDERR_LogRare > 0) &
        (SLOPE_LogRare - SLOPE.STDERR_LogRare < 0) ~ "nochange",
      (SLOPE_LogRare + SLOPE.STDERR_LogRare > 0) &
        (SLOPE_LogRare - SLOPE.STDERR_LogRare > 0) ~ "increasing",
      (SLOPE_LogRare + SLOPE.STDERR_LogRare < 0) &
        (SLOPE_LogRare - SLOPE.STDERR_LogRare < 0) ~ "decreasing"
    )
  )

# Add column for LogAb
master <- master %>%
  mutate(
    Direction_LogAb = case_when(
      (SLOPE_LogAb + SLOPE.STDERR_LogAb > 0) &
        (SLOPE_LogAb - SLOPE.STDERR_LogAb < 0) ~ "nochange",
      (SLOPE_LogAb + SLOPE.STDERR_LogAb > 0) &
        (SLOPE_LogAb - SLOPE.STDERR_LogAb > 0) ~ "increasing",
      (SLOPE_LogAb + SLOPE.STDERR_LogAb < 0) &
        (SLOPE_LogAb - SLOPE.STDERR_LogAb < 0) ~ "decreasing"
    )
  )




### Graphs are more aesthetically pleasing with latitude and longitude values
### So switch to EPSG 4735 for USA contiguous Albers Equal Area Conic
### There are others but this one looks good. https://spatialreference.org/ref/?search=USA




#### Graph the results ####

colorblindcheck::palette_check(c(viridis(100)[1], 
                                 viridis(100)[85]), plot = TRUE)

# Figure 1b, 1d, and 1f

path4 <- "01_Analysis/Biodiversity_Plotting/Figure 1/Truncated Time Period/"

graph.titles <- c("Observed Richness", "Rarefied Richness", "Abundance")

for (i in 1:3) { 
  
  if(i == 1) {
    x <- master$Direction_LogObs
  } else {
    if (i == 2) {
      x <- master$Direction_LogRare
    } else {
      if (i == 3) {
        x <- master$Direction_LogAb
      }
    }
  }
  
  # MAP showing directional values of slopes #
  fre_map.plot2 <- ggplot() +
    geom_sf(data = usa.shape2) +
    geom_sf(data = master, aes(fill = factor(x)), 
            color = "black", # Black outline for all points
            size = 1.5, 
            shape = 21) +    # Use shape 21 for fillable points
    theme_bw() +
    coord_sf(datum = st_crs(4735)) +
    theme(axis.text.x = element_text(size=18)) +
    theme(axis.text.y = element_text(size=18)) +
    theme(axis.title.x = element_blank()) +
    theme(axis.title.y = element_blank()) +
    scale_fill_manual(
      name = "",
      values = c(
        'decreasing' = viridis(100)[1], 
        'nochange' = "#FFFFFF",
        'increasing' = viridis(100)[85]
      ),
      labels = c("Decreasing", "Increasing", "No Change")
    ) +
    theme(
      legend.text = element_text(size=18),
      legend.title = element_text(size = 20)
    ) +
    guides(fill = guide_legend(
      override.aes = list(
        size = 5,
        shape = 21,
        color = "black" # Black outline in legend
      )
    ))
    # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    #       panel.background = element_blank())
  
  
  ggsave(paste(path4,
               paste("Map_r2 - Directional Change of Log ", graph.titles[i], ".jpg", sep = ""),
               sep = ""),
         plot=fre_map.plot2,
         device = "jpg",
         width=9, height=5, dpi=600)
  
  print(i)
  
}


###############################################
###                                         ###
###           Maps showing slopes           ###
###            of biodiv change             ###
###############################################

#### Figure 1a, 1c, 1e
#### Supplemental Figure 2a, 2b, 2c

# Formula for converting log value to percentage change (per year)
# ((10^log.val) - 1)*100

# Observed and Rarefied Breaks: -0.02, 0, 0.02
((10^-0.02) - 1)*100
((10^0) - 1)*100
((10^0.02) - 1)*100

# Abundance breaks: -0.08, -0.04, 0, 0.04, 0.08
((10^-0.08) - 1)*100
((10^-0.04) - 1)*100
((10^0) - 1)*100
((10^-0.04) - 1)*100
((10^-0.08) - 1)*100




for (i in 1:3) {
  
  # Define the mean value, axis limits, and breaks
  if(i == 1) {
    x <- master$SLOPE_LogObs
    mean.val <- mean(x)
    limits <- c(-0.026, 0.035)
    breaks <- c(-0.02, 0, 0.02)
    labels <- c("-4.50", " 0.00", " 4.71")
  } else {
    if (i == 2) {
      x <- master$SLOPE_LogRare
      mean.val <- mean(x)
      limits <- c(-0.026, 0.035)
      breaks <- c(-0.02, 0, 0.02)
      labels <- c("-4.50", " 0.00", " 4.71")
    } else {
      if (i == 3) {
        x <- master$SLOPE_LogAb
        mean.val <- mean(x)
        limits <- c(-0.085, 0.072)
        breaks <- c(-0.08, -0.04, 0, 0.04, 0.08)
        labels <- c("-16.82", "-8.80", " 0.00", " 8.80", " 16.82")
        
      }
    }
  }
  
  # MAP showing raw values of slopes #
  fre_map.plot <- ggplot() +
    geom_sf(data = usa.shape2) +
    geom_sf(data = master,
            aes(color = x), size = 1.5) +
    theme_bw() +
    coord_sf(datum = st_crs(4735)) +
    theme(axis.text.x = element_text(size=18)) +
    theme(axis.text.y = element_text(size=18)) +
    # theme(axis.ticks.x = element_blank()) +
    # theme(axis.ticks.y = element_blank()) +
    theme(axis.title.x = element_blank()) +
    theme(axis.title.y = element_blank()) +
    # theme(legend.position = "none") +
    scale_x_continuous(name="Longitude") +
    scale_y_continuous(name="Latitude") +
    scale_color_continuous(name = "% Change/Year",
                           type = "viridis",
                           limits = limits,
                           breaks = breaks,
                           labels = labels) +
    # scale_color_gradient(name = "Total Years", low = "#440154", high = "#fde725",
    #                      breaks = c(20, 30, 40, 50)) +
    theme(legend.text = element_text(size=18),
          legend.title = element_text(size = 20))
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank())
  
  
  ggsave(paste(path4,
               paste("Map - Slope of Log ", graph.titles[i], ".jpg", sep = ""),
               sep = ""),
         plot=fre_map.plot,
         device = "jpg",
         width=9, height=5, dpi=600)
  
  
  # Density Plots
  den.plot <- ggplot() +
    geom_density(aes(x=x, fill = 'rich'),
                 alpha = 0.7, position = "identity", color = "grey3") +
    geom_vline(aes(xintercept=mean.val),
               linetype="dashed", color="purple4", size = 2, alpha = 0.8) + # mean value
    geom_vline(aes(xintercept = 0), size = 1, color = "black") +
    theme(axis.text.x = element_text(size=16, color="black")) +
    theme(axis.text.y = element_text(size=16, color="black")) +
    theme(axis.title.x = element_text(size=22, face="bold", color="black")) +
    theme(axis.title.y = element_text(size=22, face="bold", color="black")) +
    scale_x_continuous("Slope", limits = c(-0.085, 0.072), breaks = c(-0.08, -0.04, 0, 0.04, 0.08)) +
    scale_y_continuous("Density") +
    scale_fill_manual(name='', values = c('rich' = "purple4"),
                      labels = '') +
    theme(legend.position = "none")


  ggsave(paste(path4,
               paste("Density - Slope of Log", graph.titles[i], ".jpg", sep = ""),
               sep = ""),
         plot=den.plot, device = "jpeg",
         width=7, height=5, dpi=600)
  
  print(i)
}




###########################################################################################

# Without the legend to ease arranging of the panels

### Directional Change ###

graph.titles <- c("Observed Richness", "Rarefied Richness", "Abundance")

# MAP showing directional values of slopes #
for (i in 1:3) { 
  
  if(i == 1) {
    x <- master$Direction_LogObs
  } else {
    if (i == 2) {
      x <- master$Direction_LogRare
    } else {
      if (i == 3) {
        x <- master$Direction_LogAb
      }
    }
  }
  
  # MAP showing directional values of slopes #
  fre_map.plot_noleg <- ggplot() +
    geom_sf(data = usa.shape2) +
    geom_sf(data = master, aes(fill = factor(x)), 
            color = "black", # Black outline for all points
            size = 1.5, 
            shape = 21) +    # Use shape 21 for fillable points
    theme_bw() +
    coord_sf(datum = st_crs(4735)) +
    theme(axis.text.x = element_text(size=18)) +
    theme(axis.text.y = element_text(size=18)) +
    theme(axis.title.x = element_blank()) +
    theme(axis.title.y = element_blank()) +
    scale_fill_manual(
      name = "",
      values = c(
        'decreasing' = viridis(100)[1], 
        'nochange' = "#FFFFFF",
        'increasing' = viridis(100)[85]
      ),
      labels = c("Decreasing", "Increasing", "No Change")
    ) +
    theme(legend.position = "none")
  # theme(
  #   legend.text = element_text(size=18),
  #   legend.title = element_text(size = 20)
  # ) +
  # guides(fill = guide_legend(
  #   override.aes = list(
  #     size = 5,
  #     shape = 21,
  #     color = "black" # Black outline in legend
  #   )
  # ))
  
  ggsave(paste(path4,
               paste("Map_r2 no legend - Directional Change of Log ",
                     graph.titles[i], ".jpg", sep = ""),
               sep = ""),
         plot=fre_map.plot_noleg,
         device = "jpg",
         width=9, height=5, dpi=600)
  
  print(i)
  
}


#### Raw Values ####

for (i in 1:3) {
  
  if(i == 1) {
    x <- master$SLOPE_LogObs
    mean.val <- mean(x)
    limits <- c(-0.026, 0.035)
    breaks <- c(-0.02, 0, 0.02)
  } else {
    if (i == 2) {
      x <- master$SLOPE_LogRare
      mean.val <- mean(x)
      limits <- c(-0.026, 0.035)
      breaks <- c(-0.02, 0, 0.02)
    } else {
      if (i == 3) {
        x <- master$SLOPE_LogAb
        mean.val <- mean(x)
        limits <- c(-0.085, 0.072)
        breaks <- c(-0.08, -0.04, 0, 0.04, 0.08)
      }
    }
  }
  
  # MAP showing raw values of slopes #
  fre_map.plot <- ggplot() +
    geom_sf(data = usa.shape2) +
    geom_sf(data = master,
            aes(color = x), size = 1.5) +
    theme_bw() +
    coord_sf(datum = st_crs(4735)) +
    theme(axis.text.x = element_text(size=18)) +
    theme(axis.text.y = element_text(size=18)) +
    # theme(axis.ticks.x = element_blank()) +
    # theme(axis.ticks.y = element_blank()) +
    theme(axis.title.x = element_blank()) +
    theme(axis.title.y = element_blank()) +
    theme(legend.position = "none") +
    # scale_x_continuous(name="Longitude") +
    # scale_y_continuous(name="Latitude") +
    scale_color_continuous(name = "",
                           type = "viridis",
                           limits = limits,
                           breaks = breaks) 
    # scale_color_gradient(name = "Total Years", low = "#440154", high = "#fde725",
    #                      breaks = c(20, 30, 40, 50)) +
    # theme(legend.text = element_text(size=18), legend.title = element_text(size = 20)) 
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank())
  

  ggsave(paste(path4,
               paste("Map no legend - Slope of Log ", graph.titles[i], ".jpg", sep = ""),
               sep = ""),
         plot=fre_map.plot,
         device = "jpg",
         width=9, height=5, dpi=600)
  
  print(i)
}
