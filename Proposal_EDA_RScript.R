#-------- ALY6015_FinalProject_Proposal --------#

# Declaring the names of packages to be imported
packageList <- c("tidyverse", "vtable", "RColorBrewer", "corrplot", "car", "psych", "stargazer")

for (package in packageList) {
  if (!package %in% rownames(installed.packages())) 
  { install.packages(package) }
  
  # Import the package
  library(package, character.only = TRUE)
}

# Import/Load the data set
ElectionData <- read.csv("~/Documents/Northeastern University/MPS Analytics/ALY 6015/Final Project/Presidential_Election_2016_Classification/Data/2016Election.csv")

# Get a Glimpse/View of the data set
glimpse(ElectionData)

# Descriptive Statistics of 'Total Votes' in 2008, 2012, 2016
totalVotes <- ElectionData %>% 
  select(v2008, v2012, v2016)


View(describe(totalVotes))
stargazer(totalVotes, type = "text", title = "Table 1: Total Votes", out = "total_votes_table.txt")




