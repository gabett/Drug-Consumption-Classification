# Dataset available at: https://archive.ics.uci.edu/ml/datasets/adult

# Libraries
library(tibble)
library(data.table)
library(dplyr)
library(ggplot2)
library(ggsci)
library(GGally)
library(gridExtra)
library(maps)
library(mapdata)
library(tidyr)
library(RColorBrewer)
library(pals)
library(rw)
library(rworldmap)

# Global Variables 
dir = "C:\\Users\\ettag\\Documents\\GitHub\\Stastistical-Learning-Project\\data"
setwd(dir = dir)

# Load
data.dir = paste(dir, "\\drug_data_clean.RData", sep = "")
load(data.dir)

images.dir = "C:\\Users\\ettag\\Documents\\GitHub\\Stastistical-Learning-Project\\images\\EDA\\CategoryOverview\\"
setwd(dir = images.dir)

# Preparing data ####
drug.users.per.country = drugs.clean %>% group_by(Country, has_taken_illegal_drugs) %>% summarise(user_count = n())

drug.users.percentage.by.country = tibble()
for(country in unique(drug.users.per.country$Country))
{
  drug.users = drug.users.per.country[Country == drug.users.per.country & has_taken_illegal_drugs == TRUE, user_count]
  non.drug.users = drug.users.per.country[Country == drug.users.per.country & has_taken_illegal_drugs == FALSE, user_count]
  
  
}

# Countries to plot
countries = c(
  "Australia", "New Zealand", "UK", "Ireland", "USA", "Canada"
)

# Retrieve the map data
countries.maps <- map_data("world", region = countries)

# Compute the centroid as the mean longitude and lattitude
# Used as label coordinate for country's names
region.lab.data <- countries.maps %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

# Join with data
drug.users.per.country = drug.users.per.country %>% left_join(region.lab.data, by = c("Country" = "region"))

rm(list = ls())
