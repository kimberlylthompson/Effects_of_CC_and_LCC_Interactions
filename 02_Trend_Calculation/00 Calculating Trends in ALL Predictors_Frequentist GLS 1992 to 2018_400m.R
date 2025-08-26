##################################################################################################
########                                                                                  ########
########          Calculating Trends (and Intercept) of Climate & Land Cover              ######## 
########   predictors for BBS Routes: LINEAR REGRESSION with temporal autocorrelation     ########
##################################################################################################

# Author: Kimberly Thompson


# Using nlme :: gls (generalized linear regression) with an AR1 autocorrelation structure,
# This code calculates the trend (slope), intercept, variance, and other parameters for 
# different climate and land cover predictors for each route in the North American 
# Breeding Bird Survey Dataset subsetted to routes with at least 20 years of surveys from
# 1992-2018.
# Predictors include:
# Precipitation (mm) from PRISM
# Mean May/June Temperature (c) from PRISM
# Urban cover from GAIA
# Canopy cover from iGFC
# Crop cover from CCI
# Permanent and Seasonal Water Cover from GSW

# Note that not all predictors have the same length of time points.


# Produces dataset:
# 'Trends_Climate and Land Cover Predictors_Frequentist_GLS 1992 to 2018_400m.csv'


# clean workspace to improve efficiency: #
# rm(list = ls() ) 
# gc() #releases memory

library(nlme)
library(tidyverse) # Data organization



###############################################
###                                         ###
###              Data Loading               ###
###                                         ###
###############################################

# Load the climate data (precip and temp)
path <- "01_Analysis/Predictor_Preparation/Merged Climate Raw Values from PRISM/"
clim <- read.csv(paste(path,
                       "Clim Raw Values for each BBS Route PRISM_400m.csv",
                       sep = ""),
                 header = TRUE)

# Load the urban cover data
path2 <- "01_Analysis/Predictor_Preparation/Merged Urban Cover Percentages GAIA/"
urban <- read.csv(paste(path2,
                        "Urban Percentage for each BBS Route GAIA_400m.csv",
                        sep = ""),
                  header = TRUE)

# Load the canopy cover
path3 <- "01_Analysis/Predictor_Preparation/Merged Canopy Cover Percentages iGFC/"
canopy <- read.csv(paste(path3,
                         "Canopy Percentage for each BBS Route iGFC_400m.csv",
                         sep = ""),
                   header = TRUE)

# Load the cropland data
path4 <- "01_Analysis/Predictor_Preparation/Merged Land Cover Proportions CCI/"
cropcci <- read.csv(paste(path4,
                          "LC Proportions for each BBS Route CCI Corrected_400m buffer.csv",
                          sep = ""),
                    header = TRUE)

# Load the water data
path5 <- "01_Analysis/Predictor_Preparation/Merged Surface Water Percentages GSW/"
water <- read.csv(paste(path5,
                        "Surface Water Percentage for each BBS Route GSW_400m.csv",
                        sep = ""),
                  header = TRUE)


###############################################
###                                         ###
###             Data Preparation            ###
###                                         ###
###############################################

### GENERAL ###
# Make a list of unique routes
routes <- unique(urban$unique_route)

# Subset each dataframe to be only from 1992 to 2018
# Not necessary for canopy or urban.gaia since they already have that time frame
clim <- clim[clim$Year >= 1992 & clim$Year <= 2018, ]
cropcci <- cropcci[cropcci$Year >= 1992 & cropcci$Year <= 2018, ]
water <- water[water$Year >= 1992 & water$Year <= 2018, ]


### CCI ###
# 10: Rainfed cropland
# 20: Cropland, irrigated or post-flooding
# 31: Mosaic cropland (>50%) / natural vegetation (tree, shrub, herbaceous cover) (<50%)
# 32: Mosaic cropland (>50%) / natural vegetation (tree, shrub, herbaceous cover) (<50%)
# 33: Mosaic cropland (>50%) / natural vegetation (tree, shrub, herbaceous cover) (<50%)
# 34: Mosaic cropland (>50%) / natural vegetation (tree, shrub, herbaceous cover) (<50%)
# 35: Mosaic cropland (>50%) / natural vegetation (tree, shrub, herbaceous cover) (<50%)

# Combine cropland classes into one: crop.rainfed, cropland irrigated or post-flooding,
# Mosaic cropland (>50%) wit natural vegetation <50%)
cropcci$crop.total <- rowSums(cbind(cropcci$crop.rainfed, cropcci$crop.irrigated,
                                    cropcci$crop.mosaic1, cropcci$crop.mosaic2,
                                    cropcci$crop.mosaic3, cropcci$crop.mosaic4,
                                    cropcci$crop.mosaic5))


### Water ###
# Convert to wide so that permanent and seasonal each have their own columns
water <- water %>%
  pivot_wider(names_from = Type, values_from = Mean, id_cols = c(Year, unique_route))

# Combine permanent and seasonal water columns into one
water$water.total <- rowSums(cbind(water$permanent, water$seasonal))

# Since it's a percentage confirm that the range of the total column is still bounded by 100
range(water$water.total, na.rm = TRUE)


### Land cover DFs
# Replace inf values with NA
canopy[] <- Map(function(x) replace(x, is.infinite(x), NA), canopy)
cropcci[] <- Map(function(x) replace(x, is.infinite(x), NA), cropcci)
urban[] <- Map(function(x) replace(x, is.infinite(x), NA), urban)
water[] <- Map(function(x) replace(x, is.infinite(x), NA), water)


###############################################
###                                         ###
###             Trend Function              ###
###                                         ###
###############################################

# Trend Based on GENERALIZED LINEAR MODEL WITH AUTOCORRELATION (AR1)
# x = dataframe, y = type of response (4 options)

trend.gls <- function(x, y) {
  
  # Include option for when all values are 0 (which happens in the case of land cover)
  # and causes gls to fail
  if(all(y == 0)) {
    
    slope <- NA
    slope.stderr <- NA
    direction <- NA
    intercept <- NA
    year1 <- min(x$Year)
    year2 <- max(x$Year)
    sample.length <- length(x$Year)
    
    # Create a dataframe
    data.frame(SLOPE = slope, SLOPE.STDERR = slope.stderr, DIRECTION = direction, INTERCEPT = intercept,
               YEAR1 = year1, YEAR2 = year2, SAMPLE.LENGTH = sample.length)
    
  } else {
    
    # Include option for when all values are the same but not 0 (which happens in the 
    # case of land cover) and causes gls to fail
    if(length(unique(round(y, digits = 4))) == 1) {
      
      slope <- 0
      slope.stderr <- 0
      direction <- NA
      intercept <- NA
      year1 <- min(x$Year)
      year2 <- max(x$Year)
      sample.length <- length(x$Year)
      
      # Create a dataframe
      data.frame(SLOPE = slope, SLOPE.STDERR = slope.stderr, DIRECTION = direction, INTERCEPT = intercept,
                 YEAR1 = year1, YEAR2 = year2, SAMPLE.LENGTH = sample.length)
      
    } else {
      
      
      # Run the linear regression
      m <- nlme :: gls(y ~ Year, data = x,
                       correlation = corAR1(form = ~ 1|Year),
                       na.action = na.omit)
      
      # Create the summary data
      l <- summary(m)
      
      # To access uncertainty data create a list of attributes
      uncertainty <- attributes(l$parAssign)
      
      # Extract the necessary values
      slope <- l$coefficients[2]
      slope.stderr <- uncertainty$varBetaFact[2,2]
      direction <- ordered(ifelse(slope > 0, "Increasing", "Decreasing"), levels = c("Increasing", 
                                                                                     "Decreasing"))
      intercept <- l$coefficients[1]
      year1 <- min(x$Year)
      year2 <- max(x$Year)
      sample.length <- length(x$Year)
      
      # Create a dataframe
      data.frame(SLOPE = slope, SLOPE.STDERR = slope.stderr, DIRECTION = direction, INTERCEPT = intercept,
                 YEAR1 = year1, YEAR2 = year2, SAMPLE.LENGTH = sample.length)
    }
  }
}


###############################################
###                                         ###
###         Set up Blank Dataframes         ###
###                                         ###
###############################################

#### GENERALIZED LINEAR REGRESSION WITH GLS ####
trend.precip <- data.frame()

trend.temp <- data.frame()

trend.urban.gaia <- data.frame()

trend.canopy <- data.frame()

trend.crop.cci <- data.frame()

trend.water.total <- data.frame()



###############################################
###                                         ###
###        Loops to Calculate Trend         ###
###          and other parameters           ###
###############################################


type <- c("precipitation", "temperature", "urban.gaia", "canopy", "crop.cci",
          "water.total")



for (m in 1:length(type)) {
  
  # Designate the dataframe to use in trend calculations
  if(m == 1 | m == 2) {
    master <- clim
  } else {
    if(m == 3) {
      master <- urban
    } else {
      if(m == 4) {
        master <- canopy
      } else {
        if(m == 5) {
          master <- cropcci
        } else {
          if(m == 6) {
            master <- water
          }
        }
      }
    }
  }

  
  for (i in 1:length(routes)) {
    
    # Subset data to each route
    x <- master[master$unique_route == routes[i], ]
    
    # Skip over routes that have all NAs (these are in Canada and will be removed later)
    # This will work for all dfs except for canopy because there is a type column
    # specifying whether that it's the mean value
    if(all(colSums(is.na(x[ , c(3:length(colnames(x)))])) == length(x$Year))) {
      next
    }
    
    # Bind calculated parameters to each summary dataframe
    if (m == 1) {
      
      y <- x$ppt.wtmean_raw_mo
      
      trend.precip <- rbind(trend.precip,
                            cbind(rep(routes[i], 1),
                                  trend.gls(x, y)))
    } else {
      
      if (m == 2) {
        
        y <- x$tmean.wtmean_raw_mo
        
        trend.temp <- rbind(trend.temp,
                            cbind(rep(routes[i], 1),
                                  trend.gls(x, y)))
        
      } else {
        
        if (m == 3) {
          
          y <- x$Mean
          
          trend.urban.gaia <- rbind(trend.urban.gaia,
                                    cbind(rep(routes[i], 1),
                                          trend.gls(x, y)))
          
        } else {
          
          if (m == 4) {
            
            x <- x[x$Type == "mean", ]
            y <- x$Mean
            
            # Next statement specific for canopy
            if(all(colSums(is.na(x[ , c(3:(length(colnames(x))-1))])) == length(x$Year))) {
              next
            }
            
            trend.canopy <- rbind(trend.canopy,
                                  cbind(rep(routes[i], 1),
                                        trend.gls(x, y)))
            
            
          } else {
            
            if (m == 5) {
              
              y <- x$crop.total
              
              trend.crop.cci <- rbind(trend.crop.cci,
                                      cbind(rep(routes[i], 1),
                                            trend.gls(x, y)))
              
            } else {
              
              if (m == 6) {
                
                # Change NAs to 0 (from above if all rowSums are NA the route would
                # be skipped)
                x[is.na(x)] <- 0
                
                y <- x$water.total
                
                trend.water.total <- rbind(trend.water.total,
                                           cbind(rep(routes[i], 1),
                                                 trend.gls(x, y)))
              }
            }
          }
        }
      }
    } # end of if/else

    
    print(paste("Route ", i, " completed for type ", type[m], sep = ""))
  } # end of i loop 
  
  print(m)
} # end m loop



###############################################
###                                         ###
###             Post-processing             ###
###                                         ###
###############################################

# Add a type column to the dataframes so they can be put together
trend.canopy$Type <- "canopy.mean"
trend.crop.cci$Type <- "crop.cci"
trend.precip$Type <- "precip"
trend.temp$Type <- "temp"
trend.urban.gaia$Type <- "urban.gaia"
trend.water.total$Type <- "water.total"


# Change the names of each df
names(trend.canopy)[1] <- "unique_route"
names(trend.crop.cci)[1] <- "unique_route"
names(trend.precip)[1] <- "unique_route"
names(trend.temp)[1] <- "unique_route"
names(trend.urban.gaia)[1] <- "unique_route"
names(trend.water.total)[1] <- "unique_route"



# Rbind trend dfs
overall.trend <- rbind(trend.canopy, trend.crop.cci, trend.precip, 
                       trend.temp, trend.urban.gaia, trend.water.total)


###############################################
###                                         ###
###            Write the CSV                ###
###                                         ###
###############################################

path7 <- "01_Analysis/Predictor_Preparation/Trends in Predictors/"
write.csv(overall.trend,
          paste(path7,
                "Trends_Climate and Land Cover Predictors_Frequentist_GLS 1992 to 2018_400m.csv",
                sep = ""),
          row.names = FALSE)





