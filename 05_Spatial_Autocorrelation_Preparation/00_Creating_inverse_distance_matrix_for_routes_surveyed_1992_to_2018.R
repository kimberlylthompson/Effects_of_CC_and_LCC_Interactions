#################################################################################################
########                                                                                 ########
########                Creating an inverse distance matrix of BBS Routes                ######## 
########                        to use for calculating Moran's I                         ########
#################################################################################################

# Author: Kimberly Thompson

# This code creates a distance matrix and an inverse distance matrix 
# between the 1758 BBS Routes used in the analysis
# to assess spatial autocorrelation by calculating Moran's I.

# If distance matrix already created, skip to line 68


# Produces datasets:
# 'BBS distance matrix_routes from 1992 to 2018.txt'
# 'BBS inverse distance matrix_1992 to 2018.txt'




########## clean workspace and load required packages ####################

# clean workspace to improve efficiency: #
# rm(list = ls() ) 
# gc() #releases memory


###############################################
###                                         ###
###             Data Loading                ###
###                                         ###
###############################################

# Load the linestring shapefile of BBS routes
path <- "00_Data/processed/BBS Data/"
bbs.routes <- sf :: st_read(paste(path, "BBS Rte 20 yrs_Linestring_1992to2018.shp", sep = ""))

# Define the coordinate system
albers = sp :: CRS("epsg:5070")

# Make sure bbs.routes are in albers
bbs.routes <- sf :: st_transform(bbs.routes, crs = albers)


###############################################
###                                         ###
###          Creating the spatial           ###
###            distance matrix              ###
###############################################

start.time <- Sys.time()
dist.matrix <- sf :: st_distance(bbs.routes3, which = "Euclidean")
end.time <- Sys.time()
end.time - start.time
# Takes around 6.5 hours on Rstudio server

# Save the dist.matrix
path2 <- "00_Data/processed/BBS Data/"

write.table(dist.matrix,
            file = paste(path2,
                         "BBS distance matrix_routes from 1992 to 2018.txt",
                         sep = ""),
            row.names = FALSE)




###############################################
###                                         ###
###             Data Loading 2              ###
###                                         ###
###############################################

# Read in the distance matrix between BBS routes
path2 <- "00_Data/processed/BBS Data/"
dist.mat <- read.table(paste(path2, "BBS distance matrix_routes from 1992 to 2018.txt",
                             sep = ""),
                       header = FALSE)



###############################################
###                                         ###
###           Data Manipulation             ###
###                                         ###
###############################################

# Remove first row
dist.mat <- dist.mat[-1, ]

# Change all columns to numeric
dist.mat <- sapply(dist.mat, as.numeric)

# check for infinity values
which(is.infinite(dist.mat)) # None

# Find the inverse of the matrix values
dist.mat_inv <- 1/dist.mat

# Replace the diagonal of the inverse matrix with 0
diag(dist.mat_inv) <- 0

# Check inverse matrix for infinity values
which(is.infinite(dist.mat_inv))

# Replace infinite values (which occur from dividing by 0) with 0
dist.mat_inv[is.infinite(dist.mat_inv) == TRUE] <- 0

###############################################
###                                         ###
###              Saving Data                ###
###                                         ###
###############################################

write.table(dist.mat_inv,
            paste(path2, file="BBS inverse distance matrix_1992 to 2018.txt",
                  sep = ""),
            row.names = FALSE)



