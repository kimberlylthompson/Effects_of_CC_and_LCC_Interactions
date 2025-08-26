##################################################################################################
########                                                                                  ########
########         Maps and Distributions of Climate and Land Cover Predictors              ######## 
########                            for BBS Routes - Figure 1:                            ########
##################################################################################################

# Author: Kimberly Thompson

# This code examines trends in climate and land cover predictors (temperature, precipitation,
# impervious surface, canopy cover, and cropland).

# FOR THE TIME PERIOD OF 1992 TO 2018 

# Plots created:
# Figure S2d - S2i
# Figure S3


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


###############################################
###                                         ###
###             Data Loading                ###
###                                         ###
###############################################


# Read in the shapefile of the BBS routes
path <- "00_Data/processed/BBS Data/"
bbs.routes <- sf :: st_read(paste(path, "BBS Rte 20 yrs_Linestring_1992to2018.shp", sep = ""))

# Pare down file to only be unique route, geometry, country and state columns
bbs.routes <- bbs.routes[ , c("Country", "statenm", "St_Abrv", "uniq_rt", "geometry")]

# Define the coordinate system
albers = sp :: CRS("+init=epsg:5070")

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

# Master is already filtered to contiguous USA only

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
###           Slope Maps & Plots            ###
###                                         ###
###############################################

# Subset master
tmp.data <- master %>%
  select(SLOPE_canopy.mean_400m, SLOPE_crop.cci_400m, SLOPE_precip_400m, SLOPE_temp_400m,
         SLOPE_urban.gaia_400m, SLOPE_water.total_400m)

# Reorder the columns so that land cover predictors are together
tmp.data <- tmp.data[ , c(1, 2, 5, 6, 3, 4, 7)]

# Make a vector of types
type <- c("SLOPE_canopy.mean_400", "SLOPE_crop.cci_400m",
          "SLOPE_urban.gaia_400m", "SLOPE_water.total_400m",
          "SLOPE_precip_400m", "SLOPE_temp_400m")

file.name <- c("Canopy Cover", "Cropland CCI",
               "Urban Gaia", "Water",
               "Precipitation", "Temperature")

legend.title <- c("Canopy", "Cropland", "Urban", "Water", "Precip", "Temp")


### Land cover predictors ####

for (i in 1:4) { 
  
# MAP showing raw values of slopes #
fre_map.plot <- ggplot() +
  geom_sf(data = usa.shape2) +
  geom_sf(data = tmp.data[ , i],
          aes_string(color = names(tmp.data)[i]), size = 1.5) +
  theme_bw() +
  coord_sf(datum = st_crs(albers)) +
  theme(axis.text.x =element_blank()) +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  # theme(legend.position = "none") +
  # scale_x_continuous(name="Longitude") +
  # scale_y_continuous(name="Latitude") +
  # scale_color_continuous(name = paste("\u0394", legend.title[i], sep = ""),
  #                        type = "viridis") +
  scale_color_viridis_c(name = paste("\u0394", legend.title[i], sep = ""),
                         direction = -1) +
                        # limits = c(-1.50, 1.60),
                        # breaks = c(-1.5, -0.75, 0, 0.75, 1.5)) +
  # scale_color_gradient(name = "Total Years", low = "#440154", high = "#fde725",
  #                      breaks = c(20, 30, 40, 50)) +
  theme(legend.text = element_text(size=18), legend.title = element_text(size = 20)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())


path4 <- "01_Analysis/Predictor_Plotting/"
ggsave(paste(path4, 
             paste("01_Analysis/Predictor_Plotting/Map - 400m Slope of ",
                   file.name[i],
                   " 1992 to 2018.jpg",
                   sep = ""),
             sep = ""),
       plot=fre_map.plot,
       device = "jpg",
       width=9, height=5, dpi=600)

print(i)
}

# Climaate predictors #
for (i in 5:6) { 
  
  # MAP showing raw values of slopes #
  fre_map.plot <- ggplot() +
    geom_sf(data = usa.shape2) +
    geom_sf(data = tmp.data[ , i],
            aes_string(color = names(tmp.data)[i]), size = 1.5) +
    theme_bw() +
    coord_sf(datum = st_crs(albers)) +
    theme(axis.text.x =element_blank()) +
    theme(axis.text.y = element_blank()) +
    theme(axis.ticks.x = element_blank()) +
    theme(axis.ticks.y = element_blank()) +
    theme(axis.title.x = element_blank()) +
    theme(axis.title.y = element_blank()) +
    # theme(legend.position = "none") +
    # scale_x_continuous(name="Longitude") +
    # scale_y_continuous(name="Latitude") +
    # scale_color_continuous(name = paste("\u0394", legend.title[i], sep = ""),
    #                        type = "viridis") +
    scale_color_viridis_c(name = paste("\u0394", legend.title[i], sep = ""),
                          direction = -1) +
    # scale_color_gradient(name = "Total Years", low = "#440154", high = "#fde725",
    #                      breaks = c(20, 30, 40, 50)) +
    theme(legend.text = element_text(size=18), legend.title = element_text(size = 20)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank())
  
  
  ggsave(paste(path4,
               paste("Map - 400m Slope of ",
                     file.name[i],
                     " 1992 to 2018.jpg",
                     sep = ""),
               sep = ""),
         plot=fre_map.plot,
         device = "jpg",
         width=9, height=5, dpi=600)
  
  print(i)
}



##### Density plots 

for (i in 1:length(type)) {
  
  if(i == 1) {
    mean.val <- mean(tmp.data$SLOPE_canopy.mean_400m, na.rm = TRUE)
    x <- tmp.data$SLOPE_canopy.mean_400m
  } else {
    if (i == 2) {
      mean.val <- mean(tmp.data$SLOPE_crop.cci_400m, na.rm = TRUE)
      x <- tmp.data$SLOPE_crop.cci_400m
    } else {
      if (i ==3) {
        mean.val <- mean(tmp.data$SLOPE_urban.gaia_400m, na.rm = TRUE)
        x <- tmp.data$SLOPE_urban.gaia_400m
      } else {
        if (i == 4) {
          mean.val <- mean(tmp.data$SLOPE_water.total_400m, na.rm = TRUE)
          x <- tmp.data$SLOPE_water.total_400m
        } else {
          if (i == 5) {
            mean.val <- mean(tmp.data$SLOPE_precip_400m, na.rm = TRUE)
            x <- tmp.data$SLOPE_precip_400m
          } else {
            if (i == 6) {
              mean.val <- mean(tmp.data$SLOPE_temp_400m, na.rm = TRUE)
              x <- tmp.data$SLOPE_temp_400m
            }
          }
        }
      }
    }
  }
  
  den.plot1 <- ggplot() +
    geom_density(aes(x=x, fill = "pred"),
                 alpha = 0.7, position = "identity", color = "grey3") +
    geom_vline(data = tmp.data[ , i],
               aes_string(xintercept = mean.val),
               linetype="dashed", color="purple4", linewidth = 2, alpha = 0.8) + # mean value
    # geom_density(data = master, aes_string(x=names(master)[i], fill = "purple4"),
    #              alpha = 0.7, position = "identity", color = "grey3") +
    # geom_vline(data = master,
    #            aes_string(xintercept=mean(names(master)[i], na.rm = TRUE)),
    #            linetype="dashed", color="purple4", size = 2, alpha = 0.8) + # mean value
    geom_vline(aes(xintercept = 0), linewidth = 1, color = "black") +
    theme(axis.text.x = element_text(size=16, color="black")) +
    theme(axis.text.y = element_text(size=16, color="black")) +
    theme(axis.title.x = element_text(size=22, face="bold", color="black")) +
    theme(axis.title.y = element_text(size=22, face="bold", color="black")) +
    scale_x_continuous(paste("\u0394", legend.title[i], "/Year", sep = "")) +
    scale_y_continuous("Density") +
    scale_fill_manual(name='', values = c('pred' = "purple4"),
                      labels = '') +
    theme(legend.position = "none")
  
  
  ggsave(paste(path4,
               paste("Density - 400m Slope of ",
                     file.name[i], " 1992 to 2018.jpg", sep = ""),
               sep = ""),
         plot=den.plot1,
         device = "jpg",
         width=7, height=5, dpi=600)
  
  print(i)
}



