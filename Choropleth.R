devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr) # For map
library(ggplot2)  # For map
library(dplyr)    # For summarizing
library(tidyr)    # For reshaping
library(stringr)  # For padding leading zeros


# Import/Load the data set
location_harshit <- "~/Documents/Northeastern University/MPS Analytics/ALY 6015/Final Project/Presidential_Election_2016_Classification/"
location_akash <- "C:/Users/rajak/OneDrive/Desktop/Study/University/ALY6015 - Intermediate Analytics/Final Project/Presidential_Election_2016_Classification/"

location <- ifelse(!dir.exists(paths = location_harshit), location_akash, location_harshit)
ElectionData <- read.csv(paste0(location, "Data/2016Election.csv"))

ElectionDataMap <- ElectionData %>% select(c_fips, county, state, pd2008)
ElectionDataMap$c_fips <- ifelse(nchar(ElectionDataMap$c_fips) == 4, paste0("0",ElectionDataMap$c_fips), ElectionDataMap$c_fips)

# Obtain map data for counties (to link with covid data) and states (for showing borders)
states_sf <- get_urbn_map(map = "states", sf = TRUE)
counties_sf <- get_urbn_map(map = "counties", sf = TRUE)

# Merge county map with total cases of cov
counties_pd <- inner_join(counties_sf, ElectionDataMap, by=c("county_fips"="c_fips"))

counties_pd %>%
  ggplot() +
  geom_sf(mapping = aes(fill = pd2008), color = NA) +
  geom_sf(data = states_sf, fill = NA, color = "black", size = 0.25) +
  coord_sf(datum = NA) +   
  scale_fill_gradient(name = "Percentage Democratic Votes", trans = "log", low='red', high='steelblue', 
                      na.value="white", breaks=c(1, max(counties_pd$pd2008))) +
  theme_bw() + theme(legend.position="bottom", panel.border = element_blank())


unique(counties_pd$state_abbv)

a <- counties_sf[which(counties_sf$state_abbv == "CA"), "county_fips"]
a$county_fips

ElectionDataMap[which(ElectionDataMap$state == "CA"), "c_fips"]

write.csv(counties_pd[, c("county_fips", "state_abbv")], "chro.csv")

