# Dataset available at: https://archive.ics.uci.edu/ml/datasets/Drug+consumption+%28quantified%29

# Libraries
library(tibble)
library(data.table)
library(dplyr)
library(ggplot2)
library(ggsci)
library(GGally)
library(gridExtra)
library(maps)
library(tidyr)
library(RColorBrewer)
library(pals)

rm(list = ls())

# Global Variables 
dir = "C:\\Users\\ettag\\Documents\\GitHub\\Stastistical-Learning-Project\\data"
setwd(dir = dir)

# Load
data.dir = paste(dir, "\\drug_data_clean.RData", sep = "")
load(data.dir)

images.dir = "C:\\Users\\ettag\\Documents\\GitHub\\Stastistical-Learning-Project\\images\\EDA\\CategoryOverview\\"
setwd(dir = images.dir)

drugs = drugs.clean[, 13:31]

drugs.count = drugs %>%
  summarise(Alcohol = sum(Alcohol == TRUE),
            Amphet  = sum(Amphet == TRUE),
            Amyl    = sum(Amyl == TRUE),
            Benzos   = sum(Benzos == TRUE),
            Caffeine = sum(Caff == TRUE),
            Cannabis = sum(Cannabis == TRUE),
            Cocaine  = sum(Coke == TRUE),
            Crack   = sum(Crack == TRUE),
            Ecstasy = sum(Ecstasy == TRUE),
            Heroin = sum(Heroin == TRUE),
            Ketamine = sum(Ketamine == TRUE),
            LegalH = sum(Legalh == TRUE),
            LSD = sum(LSD == TRUE),
            Meth = sum(Meth == TRUE),
            Mushrooms = sum(Mushrooms == TRUE),
            Nicotine = sum(Nicotine == TRUE),
            Semeron = sum(Semer == TRUE),
            VSA = sum(VSA == TRUE))

drug.names = c(  "Alcohol",
            "Amphet",
            "Amyl",
            "Benzos",   
            "Caffeine",
            "Cannabis", 
            "Cocaine",  
            "Crack",
            "Ecstasy",
            "Heroin", 
            "Ketamine", 
            "LegalH", 
            "LSD",
            "Meth",
            "Mushrooms",
            "Nicotine", 
            "Semeron",
            "VSA")

drugs.count = t(drugs.count)
drugs.df = tibble(Drug = drug.names, Total = drugs.count)


ggplot(drugs.df) + 
  geom_bar(aes(x = reorder(Drug, -Total), y = Total, fill = Drug), stat = "identity") +
  theme_minimal() +
  labs(y = "Count", x = "Drug", fill = "Drugs", title = "") +
  theme(legend.position = "left",
        aspect.ratio = 1, 
        title = element_text(size= 25),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size= 15),
        axis.text.y = element_text(size= 25),
        axis.title.y = element_text(size= 25),
        axis.title.x = element_text(size= 25),
        legend.key = element_blank(), legend.key.size = unit(1,"line"),
        legend.title=element_text(size= 25)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

           