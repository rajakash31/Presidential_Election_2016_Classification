#-------- ALY6015_FinalProject_Proposal --------#

# Declaring the names of packages to be imported
packageList <- c("tidyverse", "vtable", "RColorBrewer", "corrplot", "car", "psych", "stargazer", "scales")

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


# ------------------------ Descriptive Statistics ------------------------ #
# Descriptive Statistics of 'Total Votes' in 2008, 2012, 2016
totalVotesStats <- ElectionData %>% 
  select(v2008, v2012, v2016)
# # Stargazer Method
# stargazer(totalVotesStats, type = "text", title = "Table 1: Descriptive Statistics for Total Votes", 
#           out = "total_votes_table.txt", iqr = TRUE, median = TRUE, style = "jpam")
# # Write Table Method
# write.table(totalVotesStats, file = "Documents/Northeastern University/MPS Analytics/ALY 6015/Final Project/Presidential_Election_2016_Classification/total_votes_table_doc.doc", sep = ",")

# Kable Classic Method
vars <- c(1,3,4,5,6,9,10,11,15,16,17)
totalVotesStats <-  totalVotesStats %>% 
  describe(quant = c(.25, .75), IQR = TRUE) %>% 
  mutate(year = c(2008, 2012, 2016)) %>% 
  relocate(year)
round(totalVotesStats[vars], 2) %>% 
  kbl(caption = "Table 1: Descriptive Statistics for Total Votes") %>% 
  kable_classic(html_font = "Cambria")


# Descriptive Statistics of 'Total Democratic Votes' in 2008, 2012, 2016
totalDemocraticVotesStats <- ElectionData %>% 
  select(vd2008, vd2012, vd2016) %>% 
  describe(quant = c(.25, .75), IQR = TRUE) %>% 
  mutate(year = c(2008, 2012, 2016)) %>% 
  relocate(year)
# Kable Classic Method
round(totalDemocraticVotesStats[vars], 2) %>% 
  kbl(caption = "Table 2: Descriptive Statistics for Total Democratic Votes") %>% 
  kable_classic(html_font = "Cambria")


# Descriptive Statistics of 'Total Republican Votes' in 2008, 2012, 2016
totalRepublicanVotes <- ElectionData %>% 
  select(vg2008, vg2012, vg2016) %>% 
  describe(quant = c(.25, .75), IQR = TRUE) %>% 
  mutate(year = c(2008, 2012, 2016)) %>% 
  relocate(year)
# Kable Classic Method
round(totalRepublicanVotes[vars], 2) %>% 
  kbl(caption = "Table 3: Descriptive Statistics for Total Republican Votes") %>% 
  kable_classic(html_font = "Cambria")


# Descriptive Statistics of 'Total Unemployment Rate' in 2011, 2012, 2013, 2014, 2015
unemploymentRate <- ElectionData %>% 
  select(unemp11, unemp12, unemp13, unemp14, unemp15) %>% 
  describe(quant = c(.25, .75), IQR = TRUE) %>% 
  mutate(year = c(2011, 2012, 2013, 2014, 2015)) %>% 
  relocate(year)
# Kable Classic Method
round(unemploymentRate[vars], 2) %>% 
  kbl(caption = "Table 4: Descriptive Statistics for Total Unemployment Rate") %>% 
  kable_classic(html_font = "Cambria")


# ------------------------ Exploratory Data Analysis ------------------------ #
# Plot of outcome variable 'Total Votes' by state and year
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


# Plot of outcome variable 'Total Democratic Votes' by state and year
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


# Plot of outcome variable 'Total Republican Votes' by state and year
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


# ------------------------ Subgroup Analysis ------------------------ #
# Boxplot of total votes by year
ggplot(data = totalVotesL) +
  geom_boxplot(mapping = aes(tVotes, fill = year), notch = TRUE) + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
  labs(title = "Boxplot of Total Votes by Year") + 
  scale_x_continuous(name = "Total Votes", labels = label_number(suffix = " M", scale = 1e-6)) +
  theme_bw()

# Boxplot of total democrat votes by year
ggplot(data = totalDVotesL) +
  geom_boxplot(mapping = aes(tdVotes, fill = year), notch = TRUE) + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
  labs(title = "Boxplot of Total Democrat Votes by Year") + 
  scale_x_continuous(name = "Total Votes", labels = label_number(suffix = " M", scale = 1e-6)) +
  theme_bw()

# Boxplot of total republican votes by year
ggplot(data = totalRVotesL) +
  geom_boxplot(mapping = aes(tgVotes, fill = year), notch = TRUE) + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
  labs(title = "Boxplot of Total Republican Votes by Year") + 
  scale_x_continuous(name = "Total Votes", labels = label_number(suffix = " M", scale = 1e-6)) +
  theme_bw()
