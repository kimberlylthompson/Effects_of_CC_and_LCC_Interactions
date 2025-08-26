# Effects_of_CC_and_LCC_Interactions
Code to reproduce analyses associated with 'The interacting effects of climate and land-cover change on bird communities in the United States'

Code author: Kimberly L. Thompson

Thompson, K.L; Chase, J.M; Remelgado, R. and Meyer, C. 2025. The interacting effects of climate and land-cover change on bird communities in the United States. Ecography.
DOI: http://www.doi.org/10.1002/ecog.07949

## Data
Data publicly available to reproduce results include:

North American Breeding Bird Survey data available from https://www.sciencebase.gov/catalog/item/5ea04e9a82cefae35a129d65 or by using the download script ’00 BBS State level Download and Initial Cleaning.R’ provided.

North American Breeding Bird Survey route coordinates available from https://www.mbr-pwrc.usgs.gov/bbs/geographic_information/Instructions_trend_route.htm

Temperature and precipitation data available from https://www.prism.oregonstate.edu/

European Space Agency’s Climate Change Initiative land cover data, used for agricultural cover, available from https://cds.climate.copernicus.eu/cdsapp#!/dataset/satellite-land-cover?tab=overview

Urban / impervious cover data available from https://data-starcloud.pcl.ac.cn/

Surface-water cover data available from https://global-surface-water.appspot.com/download

Tree-canopy cover data available from https://zenodo.org/records/7901290


### Intermediate output data provided includes:

**Effects_of_CC_and_LCC_Interactions/Output Data/Cleaned_GLS Master_Trends of Responses and Predictors_1992 to 2018.csv** Data used in models, produced by script folders 00_Biodiversity Data Cleaning through 03_Model_Data Preparation. Includes slopes of log(observed richness), log(rarefied richness), log(abundance), May/June temperature, May/June precipitation, tree-canopy cover, cropland cover, urban cover, and water cover for each North American Breeding Bird Survey route surveyed at least 20 times within the 1992-2018 period. These data are needed to run the models in the script folder 04_Modelling and to create Figures 2a-f, S3, and S4.  

**Effects_of_CC_and_LCC_Interactions/Output Data/BBS Rte 20 yrs_MannKendall_1992to2018.shp** Data showing the results of the Mann Kendall test examining the relationship between time series of surface-water cover and time series of Landsat data quality in which the Mann Kendall test yielded significant results for 57 Breeding Bird Survey Routes. These data were used to pare down the Cleaned_GLS Master_Trends of Responses and Predictors_1992 to 2018.csv data and create the model found in the script ‘00_BRMS Model – CC and LUCC in only minus 57 water routes.R’

**FOLDER: Effects_of_CC_and_LCC_Interactions/Output Data/Model_Objects Folder** containing data produced by the script folder 04_Modelling as .rds files. There is a separate .rds file for each model (main model, lower standard error of predictors, upper standard error of predictors, minus 57 potentially problematic water routes) and each biodiversity response (log of observed richness, rarefied richness, and abundance) for a total of 12 .rds datasets. These .rds files are used to create Figures 3a, 3b, 4, 5, S5, S6, S7, and S8. 


## R Scripts
R Script folders, which are listed below, are numbered and listed in the order they should be used. Within each folder, scripts are also numbered in the order they should be used. Several scripts within the 00_Predictor_Data Cleaning, 01_Biodiversity Preparation, and 01_Predictor Preparation are run on a high-performance computing cluster to reduce computation time, which means that this workflow can not be continuously run on a local machine. However, the code from the scripts that use the HPC cluster can be used pedagogically or adapted to run on a local machine. Note however, that they are computationally intensive.

**00_Biodiversity_Data Cleaning** The scripts in this folder downloads and cleans the North American Breeding Bird Survey raw and spatial data, subsets downloaded data, and calculates route-level abundance. Downloaded data and the data resulting from these processing scripts are not included.

**00_Predictor_Data Cleaning** The scripts in this folder download (in the case of climate data) and do initial cleaning (e.g., cropping to the US, getting data in the correct projection) predictor data including climate (temperature and precipitation), cropland (CCI), urban (GAIA), surface water (GSW), and tree-canopy (iGFC) cover. Original data associated with this code not provided, but are publicly available via the links provided above.

**01_Biodiversity_Preparation** The scripts in this folder calculate individual-based rarefied richness and Hill numbers. Note however that only the data associated with Hill number q=0, so observed richness, were used in the analysis. Original data associated with this code not provided, but are publicly available via the links provided above.

**01_Predictor_Preparation** The scripts in this folder prepare the predictor data for trend analyses by first calculating the mean May/June temperature and precipitation, and then extracting the predictor values (including all climate and land cover predictors) specific to each Breeding Bird Survey Route. Original data associated with this code not provided.

**02_Trend_Calculation** The scripts in this folder calculate trends for each biodiversity response and each predictor. This data output is not provided.

**03_Model_Data Preparation** The scripts in this folder combine the trend calculations, perform a final cleaning (e.g. removing duplicate routes), and by defining routes with potentially problematic surface-water trends (used to examine the sensitivity of model results to the inclusion of these routes). This data output is provided in 00_Data/Cleaned_GLS Master_Trends of Responses and Predictors_1992 to 2018.csv.

**04_Modelling** The scripts in this folder run Bayesian models in the main analysis, and for sensitivity analysis (i.e. models that use the upper or lower standard error of the predictors and models that exclude routes with potentially problematic surface water trends). Additionally, there is also a script to extract the model residuals, which is used to test for the presence of spatial correlation later on in ’03_Model_Data_Preparation/06_Figures/Figure S2_Calculating Morans I.R’. The data output of ‘00_BRMS_Model…’ scripts are provided in ‘01_Analysis/Modeling_BRMS/Model_Objects/CC and LUCC interactions only’.

**05_Spatial_Autocorrelation_Preparation** The script in this folder creates an inverse distance matrix that is used to test for the presence of spatial correlation later on in ’03_Model_Data_Preparation/06_Figures/Figure S2_Calculating Morans I.R’. The data output is not provided.

**06_Figures** The scripts in this folder create main manuscript figures (Figures 2-5) and supplemental figures (Figures S2 – S8). The data output is not provided.
