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
library(tidyr)
# Global Variables 
dir = "C:\\Users\\ettag\\Documents\\GitHub\\Stastistical-Learning-Project\\data"
setwd(dir = dir)

# Load
data.dir = paste(dir, "\\drug_data_clean.RData", sep = "")
load(data.dir)

images.dir = "C:\\Users\\ettag\\Documents\\GitHub\\Stastistical-Learning-Project\\images\\EDA"
setwd(dir = images.dir)
"
Alcohol   
Amphet  
Amyl    
Benzos  
Caff    
Cannabis
Choc    
Coke    
Crack   
Ecstasy 
Heroin  
Ketamine
Legalh  
LSD     
Meth    
Mushrooms
Nicotine
Semer   
VSA     
"


# How drug usage is distributed across different ages? #### 

users.for.drug = tibble()

# Alcohol
tmp.user.drug = as.data.table(drugs.clean)[Alcohol == 1, .N, by = .(Age, Alcohol)]
tmp.user.drug$Alcohol = "Alcohol" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Alcohol)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Amphet
tmp.user.drug = as.data.table(drugs.clean)[Amphet == 1, .N, by = .(Age, Amphet)]
tmp.user.drug$Amphet = "Amphet" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Amphet)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Amphet
tmp.user.drug = as.data.table(drugs.clean)[Amphet == 1, .N, by = .(Age, Amphet)]
tmp.user.drug$Amphet = "Amphet" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Amphet)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

hist.age = ggplot(data = drugs.clean, aes(x = Age)) + 
  geom_histogram(stat = "count", fill = "darkred") + 
  theme_classic() +
  labs(y = "Count") + 
  theme(legend.position = "bottom",
        aspect.ratio = 1, 
        title = element_text(size = 20),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.key = element_blank(), legend.key.size = unit(3,"line"),
        legend.title=element_text(size=20)) 
hist.age
ggsave(paste(images.dir, "hist_age.pdf", sep = ""), hist.age, device = "pdf", width = 20, height = 20)

# 
# # How drug usage is distributed around countries?
# drug.usage.world = drug.clean %>% group_by(Country) %>% summarise(users_by_country = n())
# drug.usage.world
# 
# countries = drug.usage.world$Country
# drugs.map = map_data("world", region = countries)
# 
# region.label.data <- drugs.map %>%
#   group_by(region) %>%
#   summarise(long = mean(long), lat = mean(lat))
# 
# ggplot(drugs.map, aes(x = long, y = lat)) +
#   geom_polygon(aes( group = group, fill = region))+
#   geom_text(aes(label = region), data = region.label.data,  size = 3, hjust = 0.5)+
#   scale_fill_viridis_d()+
#   theme_void()+
#   theme(legend.position = "none")

rm(list = ls())
