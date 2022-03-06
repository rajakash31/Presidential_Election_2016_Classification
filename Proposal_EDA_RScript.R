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
#location <- "~/Documents/Northeastern University/MPS Analytics/ALY 6015/"
location <- "C:/Users/rajak/OneDrive/Desktop/Study/University/ALY6015 - Intermediate Analytics/"
ElectionData <- read.csv(paste0(location, "Final Project/Presidential_Election_2016_Classification/Data/2016Election.csv"))

# Get a Glimpse/View of the data set
glimpse(ElectionData)

# Descriptive Statistics of 'Total Votes' in 2008, 2012, 2016
totalVotes <- ElectionData %>% 
  select(v2008, v2012, v2016)


View(describe(totalVotes))
stargazer(totalVotes, type = "text", title = "Table 1: Total Votes", out = "total_votes_table.txt")



# line chart of outcome variable 'Total votes' by state and year
totalVotesL <- ElectionData %>% 
  select(state, v2008, v2012, v2016) %>% group_by(state) %>% summarise('2008' = sum(v2008, na.rm = TRUE),
                                                                       '2012' = sum(v2012, na.rm = TRUE),
                                                                       '2016' = sum(v2016, na.rm = TRUE)) %>%
  gather(year, tVotes, c('2008', '2012', '2016'))

ggplot(data = totalVotesL, mapping = aes(x = reorder(factor(state), tVotes, function(x) -1*sum(x)), y = tVotes, fill = year)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "Total Votes by State & Year") + 
  scale_x_discrete(name ="States") + 
  scale_y_continuous(name = "Total Votes", labels = label_number(suffix = " M", scale = 1e-6)) +
  theme_bw()


totalDVotesL <- ElectionData %>% 
  select(state, vd2008, vd2012, vd2016) %>% group_by(state) %>% summarise('2008' = sum(vd2008, na.rm = TRUE),
                                                                       '2012' = sum(vd2012, na.rm = TRUE),
                                                                       '2016' = sum(vd2016, na.rm = TRUE)) %>%
  gather(year, tdVotes, c('2008', '2012', '2016'))

ggplot(data = totalDVotesL, mapping = aes(x = reorder(factor(state), tdVotes, function(x) -1*sum(x)), y = tdVotes, fill = year)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "Total Democrat Votes by State & Year") + 
  scale_x_discrete(name ="States") + 
  scale_y_continuous(name = "Total Democrat Votes", labels = label_number(suffix = " M", scale = 1e-6)) +
  theme_bw()


totalRVotesL <- ElectionData %>% 
  select(state, vg2008, vg2012, vg2016) %>% group_by(state) %>% summarise('2008' = sum(vg2008, na.rm = TRUE),
                                                                          '2012' = sum(vg2012, na.rm = TRUE),
                                                                          '2016' = sum(vg2016, na.rm = TRUE)) %>%
  gather(year, tgVotes, c('2008', '2012', '2016'))

ggplot(data = totalRVotesL, mapping = aes(x = reorder(factor(state), tgVotes, function(x) -1*sum(x)), y = tgVotes, fill = year)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "Total Republican Votes by State & Year") + 
  scale_x_discrete(name ="States") + 
  scale_y_continuous(name = "Total Republican Votes", labels = label_number(suffix = " M", scale = 1e-6)) +
  theme_bw()


           