##################################################################################################
########                                                                                  ########
########          Calculating Time Series of Biodiversity Indices for BBS Routes:         ######## 
########                                    Hill Numbers                                  ########
##################################################################################################

# Author: Kimberly Thompson


# This code calculates Hill numbers for each year and route in the North American Breeding
# Bird Survey Dataset (subsetted to routes with at least 20 years of surveys).
# But only for the period of 1992 to 2018.
# Some route-year combinations were not surveyed, so those will be missing values 
# (NAs will have to be added).

# Only data for q=0 (i.e., species richness) was used in the analysis.

# Produces datasets:
# 'Hill Numbers_Data Info.csv'
# 'Hill Numbers_Knot Estimates.csv'
# 'Hill Numbers_Diversity Estimates.csv' - contains species richness values


# iNEXT vignette
# https://cran.r-project.org/web/packages/iNEXT/vignettes/Introduction.html


########## clean workspace and load required packages ####################

# clean workspace to improve efficiency: #
# rm(list = ls() ) 
# gc() #releases memory


library(iNEXT) # Hill Numbers
library(ggplot2) # Plotting


library(tidyverse) # Data organization
library(lubridate) # Time values
library(data.table) # Data organization


###############################################
###                                         ###
###        Some notes about iNEXT           ###
###                                         ###
###############################################

# Function iNEXT, datatype = abundance, datainfo
# n (or T) = reference sample size
# S.obs = observed species richness
# U = total number of incidences (for incidence data)
# sc = sample coverage estimate
# f1-f10 = first ten frequency counts

# Function iNEXT, datatype = abundance, iNextest
# m = sample size (i.e. each of the knots, total number of knots is 2 * reference sample size, where reference
#      sample size just equals the total number of individuals observed at a site)
# method = interpolated, observed (midpoint), extrapolated (depending on whether the sample size m is less than
#          the reference sample size (--> interpolation), equal to (--> observed)), or greater than 
#          (--> extrapolation) the reference sample size
# order = diversity order (which Hill number... 0 = richness, 1 = Shannon Diversity, 2 = Simpson diversity)
# qD = diversity estimate of order q
# qD.LCL, qD.UCL = lower and upper 95% confidence limits of diversity
# sc = sample cover estimate
# sc.LCL, sc.UCL = lower and upper 95% confidence limits of sample cover
# The sample coverage estimates with CIs ar eused for plotting the sample completeness curve and coverage-based
# rarefaction/extrapolation curves

# Function iNEXT, datatype = abundance, AsyEst
# lists the observed diversity, asymptotic estimates, estimated bootstrap s.e., and 95% CIs for Hill numbers
# with q = 0, 1, and 2

# FYI - For species richness, the extrapolation method is reliable up to double the reference sample size
# However, for q=1 and q=2, the extrapolation can usually be safely extended to the asymptote if data are not 
# sparse.

# You can choose any number of knots between 1 and the endpoint, but a large number of knots will take a long
# time to run because of the boostrap method


###############################################
###                                         ###
###      Load the observation data          ###
###                                         ###
###############################################

# Data for routes where there are at least 20 years of data
path <- "00_Data/processed/BBS Data/"
df.obs <- read.csv(paste(path, "BBS Observations with 20 years plus from 1992 to 2018.csv", sep = ""), header = TRUE)

str(df.obs) # StateNum, Year, and Route are all seen as integers

# An important note is that some routes are duplicated in one year, i.e. 
# have been surveyed more than once. This is specified with the RPID column.
# 101 means that it's the first survey of the route, 102 means it's the second 
# time the route has been surveyed and so on up to 104.
# Remove these replicates so that the temporal biodiv functions do not fail.
df.obs <- df.obs[df.obs$RPID == "101", ]



###############################################
###                                         ###
###     Calculate Hill Numbers and CIs      ###
###                                         ###
###############################################

# Create 3 different blank dataframes in which to store the output
# (corresponding to the 3 outputs of the iNext function)

data.info <- data.frame(site = character(), n = integer(), S.obs = integer(), SC = numeric(), Q1 = integer(), Q2 = integer(),
                        Q3 = integer(), Q4 = integer(), Q5 = integer(), Q6 = integer(), Q7 = integer(),
                        Q8 = integer(), Q9 = integer(), Q10 = integer())

data.info <- cbind(df.obs[0, c(1:3, 6, 16)], data.info)


hill.estimate <- data.frame(m = integer(), method = character(), order = integer(), qD = numeric(),
                            qD.LCL = numeric(), qD.UCL = numeric(), SC = numeric(),
                            SC.LCL = numeric(), SC.UCL = numeric())

hill.estimate <- cbind(df.obs[0, c(1:3, 6, 16)], hill.estimate)


hill.asyest <- data.frame(Diversity = character(), Observed = numeric(), Estimator = numeric(),
                          s.e. = numeric(), LCL = numeric(), UCL = numeric())

hill.asyest <- cbind(df.obs[0, c(1:3, 6, 16)], hill.asyest)


# Create a vector of unique routes
routes <- unique(df.obs$unique_route)


###################################################
# Loop to calculate the data for each hill number #
###################################################

start.time <- Sys.time()

for (i in 1:length(routes)) {
  
  # First subset by route
  temp <- df.obs %>%
    subset(unique_route == routes[i])
  
  # Create a vector of unique years
  years <- unique(temp$Year)
  
  for (j in 1:length(years)) {
    
    # Subset the dataframe by year
    temp2 <- temp %>%
      subset(Year == years[j])
    
    # Also remove any duplicated combinations of unique_route and AOU (species ID)
    # if the vector of duplicated T/F is not all false (meaning sum of trues = 0)
    if(sum(duplicated(temp2[, c("unique_route", "AOU")])) != 0) {
      temp2 <- temp2[-which(duplicated(temp2[, c("unique_route", "AOU")])), ]
    } 
    
    # # Retain the identifying characteristics (Route and year will come from i and j indices)
    # StateNum <- temp2$StateNum[1]
    
    # Retain the identifying characteristics (unique_route and year will come from i and j indices)
    ID <- temp2[1, 1:3]
    
    # Remove the columns
    temp2 <- temp2[ , c("SpeciesTotal")]
    
    # Calculate the hill numbers
    out.raw <- iNEXT(temp2, q = c(0, 1, 2), datatype = "abundance")
    
    # Rbind the output to the blank dataframes
    data.info <- rbind(data.info,
                       cbind( statenum = rep(ID$statenum[1], 1),
                              Route = rep(ID$Route[1], 1),
                              countrynum = rep(ID$countrynum[1], 1),
                              Year = rep(years[j], 1),
                              unique_route = rep(routes[i], 1),
                              out.raw$DataInfo))
    
    hill.estimate <- rbind(hill.estimate,
                           cbind( statenum = rep(ID$statenum[1], length(out.raw$iNextEst$m)),
                                  Route = rep(ID$Route[1], length(out.raw$iNextEst$m)),
                                  countrynum = rep(ID$countrynum[1], length(out.raw$iNextEst$m)),
                                  Year = rep(years[j], length(out.raw$iNextEst$m)),
                                  unique_route = rep(routes[i], length(out.raw$iNextEst$m)),
                                  out.raw$iNextEst))
    
    hill.asyest <- rbind(hill.asyest, cbind( statenum = rep(ID$statenum[1], 3),
                                             Route = rep(ID$Route[1], 3),
                                             countrynum = rep(ID$countrynum[1], 3),
                                             Year = rep(years[j], 3),
                                             unique_route = rep(routes[i], 3),
                                             Diversity = row.names(as.data.frame(out.raw$AsyEst)),
                                             as.data.frame(out.raw$AsyEst)))
    
  } # end of j loop
  
  print(i)
  
} # end of i loop
  
end.time <- Sys.time()
total.time <- end.time - start.time




###############################################
###                                         ###
###            Post-processing              ###
###                                         ###
###############################################



# Delete unnecessary columns - site column can be removed from data.info
data.info <- data.info[ , -6]

# Sort by unique route and year
data.info <- dplyr :: arrange(data.info, unique_route, Year)


#### Write the CSVS
path2 <- "01_Analysis/Biodiversity_Preparation/Biodiversity Base Metrics/"

write.csv(data.info, paste(path2, "Hill Numbers_Data Info.csv", sep = ""), row.names = FALSE)

write.csv(hill.estimate, paste(path2, "Hill Numbers_Knot Estimates.csv", sep = ""), row.names = FALSE)

write.csv(hill.asyest, paste(path2, "Hill Numbers_Diversity Estimates.csv", sep = ""), row.names = FALSE)




#### Plotting ####

# Not necessary but good to have the code


#     # Create plots to understand how diversity estimates change as a function of the number of individuals
#     # and the sample coverage. There are three types of plots (type, 1, 2, and 3)
# for (i in 1:length(routes)) {
#   
#   # First subset by route
#   temp <- df.obs %>%
#     subset(unique_route == routes[i])
#   
#   # Create a vector of unique years
#   years <- unique(temp$Year)
#   
#   for (j in 1:length(years)) {
#     
#     # Type 1: Diversity as a function of # of individuals for each type of diversity 
#     # (richness, Shannon, Simpson)
#     type1.graph <- ggiNEXT(out.raw, type = 1, facet.var = "order") +
#       theme(legend.position = "none") +
#       labs(title = paste("Route.", routes[i], "_", years[j], sep = ""))
#     
#     setwd("I:/MAS/01_projects/iUpdate/01_Analysis/Graphs/Hill Number Exploratory Plots/Type 1")
#     ggsave(filename = paste("Route.", routes[i], "_", years[j], "_Type1.jpg", sep = ""), type1.graph,
#            device = "jpg", width=7, height=5, dpi=300)
#     
#     # Type 2: Plots the sample completeness curve
#     type2.graph <- ggiNEXT(out.raw, type = 2, facet.var = "none") +
#       theme(legend.position = "none") +
#       labs(title = paste("Route.", routes[i], "_", years[j], sep = ""))
#     
#     setwd("I:/MAS/01_projects/iUpdate/01_Analysis/Graphs/Hill Number Exploratory Plots/Type 2")
#     ggsave(filename = paste("Route.", routes[i], "_", years[j], "_Type2.jpg", sep = ""), type2.graph,
#            device = "jpg", width=7, height=5, dpi=300)
#     
#     # Type 3: Plots the coverage-based Rarefaction/Extrapolation curves for each type of diversity
#     # richness, Shannon, Simpson)
#     type3.graph <- ggiNEXT(out.raw, type = 3, facet.var = "order") +
#       theme(legend.position = "none") +
#       labs(title = paste("Route.", routes[i], "_", years[j], sep = ""))
#     
#     setwd("I:/MAS/01_projects/iUpdate/01_Analysis/Graphs/Hill Number Exploratory Plots/Type 3")
#     ggsave(filename = paste("Route.", routes[i], "_", years[j], "_Type3.jpg", sep = ""), type3.graph,
#            device = "jpg", width=7, height=5, dpi=300)
#     
#    
#     print(j) 
#     
#   }
#   
#   print(i)
# }





          

          


