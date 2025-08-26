##################################################################################################
########                                                                                  ########
########               Calculating Trends (and Intercept) of Biodiversity                 ######## 
########        for BBS Routes: LINEAR REGRESSION with temporal autocorrelation           ########
##################################################################################################

# Author: Kimberly Thompson

# Using nlme :: gls (generalized linear regression) with an AR1 autocorrelation structure,
# This code calculates the trend (slope), intercept, variance, and other parameters for 
# 4 different biodiversity responses: observed richness, individual-based (rarefied)
# richness, and abundance
# for each route in the North American Breeding Bird Survey 

# BUT ONLY ROUTES WITH 20+ YEARS DATA FOR THE PERIOD OF 1992 - 2018.


# Produces dataset:
# 'Trends_Biodiversity Metrics_Frequentist_GLS 1992 to 2018.csv'


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

# Load the observed richness data
path <- "01_Analysis/Biodiversity_Preparation/Biodiversity Base Metrics/"
observed <- read.csv(paste(path, "Hill Numbers_Diversity Estimates.csv", sep = ""),
                     header = TRUE)

# Load the rarefied richness data
rarefied <- read.csv(paste(path,
                           "Rarefied Richness for 60 individuals 1992 to 2018.csv",
                           sep = ""),
                     header = TRUE)

# Load the abundance data
path2 <- "00_Data/processed/BBS Data/"
abundance <- read.csv(paste(path2,
                            "Community Level Abundances for routes with 20 plus years.csv",
                            sep = ""),
                      header = TRUE)

# Load the raw survey data to obtain the routes to retain
raw <- read.csv(paste(path2,
                      "BBS Observations with 20 years plus from 1992 to 2018.csv",
                      sep = ""),
                header = TRUE)



###############################################
###                                         ###
###             Data Preparation            ###
###                                         ###
###############################################

# Generate a vector of routes to keep in each biodiversity dataset
routes_to_retain <- unique(raw$unique_route)


### observed ###

# Pare data to be only richness
observed <- observed[observed$Diversity == "Species Richness", ]


# Pare data to be only period of 1992 to 2018
observed <- observed[observed$Year >= 1992 & observed$Year <= 2018, ]

# Create a dataframe to store subsetted observations (same column names as originals)
observed.sub <- observed[0, ]

# Subset the observed richness df to include only the routes to retain
for (i in 1:length(routes_to_retain)) {
  
  # Temporary dataframe for the observations
  tmp <- observed %>%
    subset(observed$unique_route == routes_to_retain[i])
  
  # Bind observations together
  observed.sub <- rbind(observed.sub, tmp)
  
  print(i)
  
}

# Make a column for the log value of observed richness
observed.sub$Logval <- log(observed.sub$Observed)


### abundance ###

# Make a column for the log value of abundance
abundance$Logval <- log(abundance$Comm.Abundance)

# Create a dataframe to store subsetted observations (same column names as originals)
abundance.sub <- abundance[0, ]

# Subset the abundance df to include only the routes to retain
for (i in 1:length(routes_to_retain)) {
  
  # Temporary dataframe for the observations
  tmp <- abundance %>%
    subset(abundance$unique_route == routes_to_retain[i])
  
  # Bind observations together
  abundance.sub <- rbind(abundance.sub, tmp)
  
  print(i)
  
}

# Pare data to be only period of 1992 to 2018
abundance.sub <- abundance.sub[abundance.sub$Year >= 1992 & abundance.sub$Year <= 2018, ]



### rarefied ###
rarefied$Logval <- log(rarefied$Rarefied)

# Clean up workspace
rm(observed, abundance, tmp, raw, routes_to_retain)
gc()


### General ###

# Make a list of unique routes
routes <- unique(observed.sub$unique_route)



###############################################
###                                         ###
###             Trend Function              ###
###                                         ###
###############################################

# Trend Based on GENERALIZED LINEAR MODEL WITH AUTOCORRELATION (AR1)
# x = dataframe, y = type of response (4 options)

trend.gls <- function(x, y) {
  
  # Run the linear regression
  m <- nlme :: gls(y ~ Year, data = x,
                   correlation = corAR1(form = ~ 1|Year))
  
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
  data.frame(SLOPE = slope, SLOPE.STDERR = slope.stderr, DIRECTION = direction,
             INTERCEPT = intercept, YEAR1 = year1, YEAR2 = year2, SAMPLE.LENGTH = sample.length)
}


###############################################
###                                         ###
###         Set up Blank Dataframes         ###
###                                         ###
###############################################

#### GENERALIZED LINEAR REGRESSION WITH GLS ####

# Log Observed
trend.logobs <- data.frame(SLOPE = numeric(),  SLOPE.STDERR = numeric(), DIRECTION = character(),
                        INTERCEPT = numeric(), YEAR1 = integer(), YEAR2 = integer(),
                        SAMPLE.LENGTH = integer())

# Log Rarefied
trend.lograre <- data.frame(SLOPE = numeric(),  SLOPE.STDERR = numeric(), DIRECTION = character(),
                         INTERCEPT = numeric(), YEAR1 = integer(), YEAR2 = integer(),
                         SAMPLE.LENGTH = integer())

# Log Abundance
trend.logab <- data.frame(SLOPE = numeric(),  SLOPE.STDERR = numeric(), DIRECTION = character(),
                       INTERCEPT = numeric(), YEAR1 = integer(), YEAR2 = integer(),
                       SAMPLE.LENGTH = integer())




###############################################
###                                         ###
###        Loops to Calculate Trend         ###
###          and other parameters           ###
###############################################

# Define the types of responses
type <- c("logobs", "lograre", "logab")


for (m in 1:length(type)) {
  
  # Designate the dataframe to use in trend calculations
  if(m == 1) {
    master <- observed.sub
  } else {
    if(m == 2) {
      master <- rarefied
    } else {
      if(m == 3) {
        master <- abundance.sub
      }
    }
  }
  
  for (i in 1:length(routes)) {
    
    # TryCatch - for a few of the routes, the gls model fails to converge and so throws an error,
    # we can record this error but keep the loop going
    tryCatch({
      
      # Subset data to each route
      x <- master[master$unique_route == routes[i], ]
      
      # Define identifying info (this will work for all but turnover)
      statenum <- master$statenum[master$unique_route == routes[i]][1]
      Route <- master$Route[master$unique_route == routes[i]][1]
      countrynum <- master$countrynum[master$unique_route == routes[i]][1]
      
      # Bind calculated parameters to each summary dataframe
      
      if (m == 1) {
        
        y <- x$Logval
        
        trend.logobs <- rbind(trend.logobs,
                              cbind(rep(statenum, 1),
                                    rep(Route, 1),
                                    rep(countrynum, 1),
                                    rep(routes[i], 1),
                                    trend.gls(x, y)))
      } else {
        
        if (m == 2) {
          
          y <- x$Logval
          
          trend.lograre <- rbind(trend.lograre,
                                 cbind(rep(routes[i], 1),
                                       trend.gls(x, y)))
          
        } else {
          
          if (m == 3) {
            
            y <- x$Logval
            
            trend.logab <- rbind(trend.logab,
                                 cbind(rep(statenum, 1),
                                       rep(Route, 1),
                                       rep(countrynum, 1),
                                       rep(routes[i], 1),
                                       trend.gls(x, y)))
            
          }
        }
      }
        
      
      # This ends the try catch and prints the error
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
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
trend.logab$Type <- "LogAb"
trend.logobs$Type <- "LogObs"
trend.lograre$Type <- "LogRare"

# Isolate one of the dataframes that has the state, base route, and country numbers
# to have identifying info only
# It will be in the same order as the others so cbind it to the rarefied df
trend.lograre <- cbind(trend.lograre, trend.logobs[ , c(1:3)])

# Change the names of each df
names(trend.logobs)[1] <- "statenum"
names(trend.logobs)[2] <- "Route"
names(trend.logobs)[3] <- "countrynum"
names(trend.logobs)[4] <- "unique_route"

names(trend.lograre)[10] <- "statenum"
names(trend.lograre)[11] <- "Route"
names(trend.lograre)[12] <- "countrynum"
names(trend.lograre)[1] <- "unique_route"

names(trend.logab)[1] <- "statenum"
names(trend.logab)[2] <- "Route"
names(trend.logab)[3] <- "countrynum"
names(trend.logab)[4] <- "unique_route"

# Rbind trend dfs
overall.trend <- rbind(trend.logobs, trend.lograre,
                       trend.logab)


###############################################
###                                         ###
###            Write the CSV                ###
###                                         ###
###############################################

path3 <- "01_Analysis/Biodiversity_Preparation/Biodiversity Trends"
write.csv(overall.trend,
          paste(path3,
                "Trends_Biodiversity Metrics_Frequentist_GLS 1992 to 2018.csv",
                sep = ""),
          row.names = FALSE)





