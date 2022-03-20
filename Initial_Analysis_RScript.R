#----------------------------------- ALY6015 FinalProject Initial Analysis -------------------------------#

# Declaring the names of packages to be imported
packageList <- c("tidyverse", "vtable", "RColorBrewer", "corrplot", "car", "psych", "stargazer", "scales")

for (package in packageList) {
  if (!package %in% rownames(installed.packages())) 
  { install.packages(package) }
  
  # Import the package
  library(package, character.only = TRUE)
}

# Import/Load the data set
location_harshit <- "~/Documents/Northeastern University/MPS Analytics/ALY 6015/Final Project/Presidential_Election_2016_Classification/"
location_akash <- "C:/Users/rajak/OneDrive/Desktop/Study/University/ALY6015 - Intermediate Analytics/Final Project/Presidential_Election_2016_Classification/"

ElectionData <- read.csv(paste0(ifelse(!dir.exists(paths = location_harshit), location_akash, location_harshit), "Data/2016Election.csv"))

# Get a Glimpse/View of the data set
glimpse(ElectionData)


# ------------------------------------- Merging External Data sets ----------------------------------- #
# Reading new external data set
population <- read.csv(paste0(location, "Data/USPopulationByCounty.csv"))
election2020 <- read.csv(paste0(location, "Data/2020ElectionResults.csv"))
region <- read.csv(paste0(location, "Data/Regions.csv"))


# Combining the external data sets with the original election data sets
ElectionData <- ElectionData %>% inner_join(population, by = c("county", "state"))

election2020$county_fips <- as.factor(election2020$county_fips)
ElectionData$c_fips <- as.factor(ElectionData$c_fips)
ElectionData <- ElectionData %>% inner_join(election2020, by = c("c_fips" = "county_fips"))

ElectionData <- ElectionData %>% inner_join(region, by = c("state" = "state.code"))


