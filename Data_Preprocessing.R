# Dataset available at: https://archive.ics.uci.edu/ml/datasets/adult

# Libraries
library(tibble)
library(data.table)
library(dplyr)
library(ggplot2)

# Global Variables 
dir = "Put your directory here"
setwd(dir = dir)

# Loading the dataset
adult.csv.directory = paste(dir, "\\adult.csv", sep = "", )

adult.data = read.csv(adult.csv.directory, sep = ",")
adult.data = as.data.table(adult.data)

summary(adult.data)  # 48842 rows
# We can see how NA occupations and countries are represented with a '?' character

# Drop it!
na.occupations = adult.data %>% filter(occupation == "?")
na.country = adult.data %>% filter(native.country == "?")

adult.data = adult.data %>% anti_join(na.occupations)
adult.data = adult.data %>% anti_join(na.country)

# Sanity check: rows of new dataset + rows with NA must be equal to 48842
dim(adult.data)[1] + dim(na.occupations)[1] + dim(na.country)[1] == 48842

save(adult.data, file = ".\\adult_data.RData")



