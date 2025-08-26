#################################################################################################
########                                                                                 ########
########            Creating csv & map of routes that potentially                        ######## 
########          change surface water % in relation to data quality                     ########
#################################################################################################

# Author: Kimberly Thompson

# This script creates a csv and a map of routes whose total % of surface water were potentially
# driven by changes in landsat quality rather than real changes.

# Produces datasets:
# 'Routes with potentially problematic water trends.csv'



# We applied the Mann-Kendall (MK) test at each BBS site to time-series of surface water
# (i.e. total %) and Landsat data quality (i.e. number of months with data). 
# These data cover the period of 1992 and 2020, and were aggregated to a 10-km 
# resolution to capture regional changes surrounding the BBS sites.

# We tested for the presence of trends in either time-series, applying a significance
# threshold of p < 0.05. We then checked which survey sites had significant positive
# (or negative) trends in both time-series. We found 57 sites fitting this description.

# This analysis indicates that sites fitting the first class experienced gains 
# in surface water that are potentially driven by improvements in data quality. 


########## clean workspace and load required packages ####################

# clean workspace to improve efficiency: #
# rm(list = ls() ) 
# gc() #releases memory

library(sf)
library(viridis)
library(tidyverse) # Data organization
library(sp)


###############################################
###                                         ###
###             Data Loading                ###
###                                         ###
###############################################


# Read in the shapefile of the BBS routes
path <- "00_Data/processed/BBS and Surface Water/"
bbs.routes <- sf :: st_read(paste(path, "BBS Rte 20 yrs_MannKendall_1992to2018.shp", sep = ""))


# Define the coordinate system
albers = sp :: CRS("epsg:5070")

# Make sure bbs.routes are in albers
bbs.routes <- sf :: st_transform(bbs.routes, crs = albers)


# Load USA and Canada shapefile
path2 <- "00_Data/raw/"
usa.shape <- sf :: st_read(paste(path2, "USACANAB.shp", sep = ""))

# Define coordintate system as albers:
usa.shape <- sf :: st_set_crs( usa.shape, albers )

###############################################
###                                         ###
###           Data Preparation              ###
###                                         ###
###############################################

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
###             Make the map                ###
###                                         ###
###############################################

fre_map.plot <- ggplot() +
  geom_sf(data = usa.shape2) +
  geom_sf(data = bbs.routes[bbs.routes$trend == "agreeing positive trends (p < 0.05)", ],
          color = viridis(100)[75], size = 3) +
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
  theme(legend.position = "none")
  # scale_color_gradient(name = "Total Years", low = "#440154", high = "#fde725",
  #                      breaks = c(20, 30, 40, 50)) +
  # theme(legend.text = element_text(size=18), legend.title = element_text(size = 20)) 
# theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#       panel.background = element_blank())


###############################################
###                                         ###
###             Make the csv                ###
###                                         ###
###############################################

prob.routes <- data.frame(problem.routes =
                            bbs.routes$uniq_rt[bbs.routes$trend ==
                                                 "agreeing positive trends (p < 0.05)"])

# Save the CSV
write.csv(prob.routes, paste(path2,
                             "Routes with potentially problematic water trends.csv",
                             sep = ""),
          row.names = FALSE)




