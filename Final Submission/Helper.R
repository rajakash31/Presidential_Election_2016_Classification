#----------------------------------- ALY6015_FinalProject_DataCleaning -------------------------------#

# Declaring and Importing the Packages
declareAndImportPackages <- function() {
  
  # Declaring the names of packages to be imported
  packageList <- c("tidyverse", "vtable", "RColorBrewer", "corrplot", "car", "psych", "stargazer", 
                   "scales", "glmnet", "Metrics", "caret", "leaps", "MASS", "flextable", "readxl")
  
  for (package in packageList) {
    if (!package %in% rownames(installed.packages())) 
    { install.packages(package) }
    
    # Import the package
    library(package, character.only = TRUE)
  } 
}

# Function to load the data set
loadData <- function(location) {
  
  return (read.csv(paste0(location, "Data/2016Election.csv")))
}

# Function to save table as 3 line format
threeLineTable <- function(df, title, footer, file_name){
  
  ft <- flextable(df)
  ft <- theme_booktabs(ft)
  ft <- add_footer_lines(ft, footer)
  ft <- flextable::color(ft, part = "footer", color = "#666666")
  ft <- set_caption(ft, caption = title)
  save_as_docx(ft, path =  paste0(location, "/Tables/", file_name, ".docx")) 
}

# Function to import external data
externalDataImport <- function() {
  
  return (
    list(
      read.csv(paste0(location, "Data/USPopulationByCounty.csv")),
      read.csv(paste0(location, "Data/2020ElectionResults.csv")),
      read.csv(paste0(location, "Data/Regions.csv")),
      read_excel(paste0(location, "Data/Additional Data.xlsx"))
    )
  )
}


# Function to create 3 Line Tables for the External Data
threeLineTableForExternalData <- function () {
  
  threeLineTable(population[1:5, 2:8], "Table 2: Snapshot of external population data", "Snapshot of population data sample", "Population Snapshot")
  threeLineTable(election2020[1:5, ], "Table 3: Snapshot of external 2020 election results data", "Snapshot of 2020 election results data", "Election 2020 Snapshot")
  threeLineTable(region[1:5, ], "Table 4: Sample of additional region variable created", "", "Region Snapshot")
  threeLineTable(ElectionData[1:5, 157:168], "Table 2: New variables added to the master data", "", "Appended data")
  threeLineTable(ElectionData[1:5, 157:168], "Table 2: New variables added to the master data", "", "Appended data")
  
}



