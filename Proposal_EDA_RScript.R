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
totalVotesStats <- ElectionData %>% 
  select(v2008, v2012, v2016)
# Stargazer Method
stargazer(totalVotesStats, type = "text", title = "Table 1: Descriptive Statistics for Total Votes", 
          out = "total_votes_table.txt", iqr = TRUE, median = TRUE, style = "jpam")
# Write Table Method
write.table(totalVotesStats, file = "Documents/Northeastern University/MPS Analytics/ALY 6015/Final Project/Presidential_Election_2016_Classification/total_votes_table_doc.doc", sep = ",")

# Kable Classic Method
totalVotesStats <-  totalVotesStats %>% 
  describe(quant = c(.25, .75), IQR = TRUE) %>% 
  mutate(year = c(2008, 2012, 2016)) %>% 
  relocate(year)
round(totalVotesStats, 2) %>% 
  kbl(caption = "Table 1: Descriptive Statistics for Total Votes") %>% 
  kable_classic(html_font = "Cambria")


# Descriptive Statistics of 'Total Democratic Votes' in 2008, 2012, 2016
totalDemocraticVotesStats <- ElectionData %>% 
  select(vd2008, vd2012, vd2016) %>% 
  describe(quant = c(.25, .75), IQR = TRUE) %>% 
  mutate(year = c(2008, 2012, 2016)) %>% 
  relocate(year)
# Kable Classic Method
round(totalDemocraticVotesStats, 2) %>% 
  kbl(caption = "Table 2: Descriptive Statistics for Total Democratic Votes") %>% 
  kable_classic(html_font = "Cambria")


# Descriptive Statistics of 'Total Republican Votes' in 2008, 2012, 2016
totalRepublicanVotes <- ElectionData %>% 
  select(vg2008, vg2012, vg2016) %>% 
  describe(quant = c(.25, .75), IQR = TRUE) %>% 
  mutate(year = c(2008, 2012, 2016)) %>% 
  relocate(year)
# Kable Classic Method
round(totalRepublicanVotes, 2) %>% 
  kbl(caption = "Table 3: Descriptive Statistics for Total Republican Votes") %>% 
  kable_classic(html_font = "Cambria")


# Descriptive Statistics of 'Total Unemployment Rate' in 2011, 2012, 2013, 2014, 2015
unemploymentRate <- ElectionData %>% 
  select(unemp11, unemp12, unemp13, unemp14, unemp15) %>% 
  describe(quant = c(.25, .75), IQR = TRUE) %>% 
  mutate(year = c(2011, 2012, 2013, 2014, 2015)) %>% 
  relocate(year)
# Kable Classic Method
round(unemploymentRate, 2) %>% 
  kbl(caption = "Table 4: Descriptive Statistics for Total Unemployment Rate") %>% 
  kable_classic(html_font = "Cambria")



