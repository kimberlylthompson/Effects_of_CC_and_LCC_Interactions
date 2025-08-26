#################################################################################################
########                                                                                 ########
########                    Merging Rarefied Richness Values                             ######## 
########                        across routes and years                                  ########
#################################################################################################

# Author: Kimberly Thompson

# This code takes rarefied richness values calculated for each route and each year, and extracts 
# the richness value for a particular number of individuals.

# This number is 60, which is 2 x minimum number of individuals found across all sites and all years.

# (The number could be changed to any arbitrary value though).

# Produces dataset:
# 'Rarefied Richness for 60 individuals 1992 to 2018.csv'


########## clean workspace and load required packages ####################

# clean workspace to improve efficiency: #
# rm(list = ls() ) 
# gc() #releases memory


library(tidyverse) # Data organization



###############################################
###                                         ###
###      Extracting individual based        ###
###   rarefied richness for 60 individs     ###
###############################################

# Create a blank dataframe to store richness value for each year and route
rare.final <- data.frame(unique_route = character(), Year = integer(),
                          Richness = numeric(), Individuals = integer())

# List the rarefied richness dataframes
path <- "01_Analysis/Biodiversity_Preparation/Biodiversity Base Metrics/Individ Based Rarefied Richness"
dfs <- list.files(path)



# Fill in the dataframe
for (i in 1:length(dfs)) {
  
  year.file <- dfs[i]
  
  # Read in the richness table
  rich.data <- read.csv(paste(path, year.file, sep = ""), header = TRUE)
  
  # Extract the year
  year <- substr(year.file, 36, 39)
  
  for (j in 1:length(unique(rich.data$unique_route))) {
    
    # Separate one route
    tmp.df <- rich.data[rich.data$unique_route == unique(rich.data$unique_route)[j], ]
    
    # Extract the row where individs = 60 and add it to the final df
    rare.final <- rbind(rare.final, tmp.df[tmp.df$Individuals == 60, ])
    
    print(paste("Year ", i, " Route ", j, sep = ""))
    
  } # end of j loop
  
} # end of i loop



# Change richness column to rarefied
names(rare.final)[1] <- "Rarefied"


# Save resulting dataframe
path2 <- "01_Analysis/Biodiversity_Preparation/Biodiversity Base Metrics"
write.csv(rare.final, paste(path2, "Rarefied Richness for 60 individuals 1992 to 2018.csv", sep = ""),
          row.names = FALSE)

