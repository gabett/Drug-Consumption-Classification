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
library(RColorBrewer)
library(pals)
# Global Variables 
dir = "C:\\Users\\ettag\\Documents\\GitHub\\Stastistical-Learning-Project\\data"
setwd(dir = dir)

# Load
data.dir = paste(dir, "\\drug_data_clean.RData", sep = "")
load(data.dir)

images.dir = "C:\\Users\\ettag\\Documents\\GitHub\\Stastistical-Learning-Project\\images\\EDA\\DrogUsageByCategory\\"
setwd(dir = images.dir)

# How is drug usage distributed across different ages? #### 

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

# Amyl
tmp.user.drug = as.data.table(drugs.clean)[Amyl == 1, .N, by = .(Age, Amyl)]
tmp.user.drug$Amyl = "Amyl" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Amyl)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Benzos  
tmp.user.drug = as.data.table(drugs.clean)[Benzos == 1, .N, by = .(Age, Benzos)]
tmp.user.drug$Benzos = "Benzos" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Benzos)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Caff      
tmp.user.drug = as.data.table(drugs.clean)[Caff == 1, .N, by = .(Age, Caff)]
tmp.user.drug$Caff = "Caffeine" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Caff)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Cannabis      
tmp.user.drug = as.data.table(drugs.clean)[Cannabis == 1, .N, by = .(Age, Cannabis)]
tmp.user.drug$Cannabis = "Cannabis" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Cannabis)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Choc          
tmp.user.drug = as.data.table(drugs.clean)[Choc == 1, .N, by = .(Age, Choc)]
tmp.user.drug$Choc = "Chocolate" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Choc)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Coke              
tmp.user.drug = as.data.table(drugs.clean)[Coke == 1, .N, by = .(Age, Coke)]
tmp.user.drug$Coke = "Cocaine" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Coke)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Crack
tmp.user.drug = as.data.table(drugs.clean)[Crack == 1, .N, by = .(Age, Crack)]
tmp.user.drug$Crack = "Crack" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Crack)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Ecstasy
tmp.user.drug = as.data.table(drugs.clean)[Ecstasy == 1, .N, by = .(Age, Ecstasy)]
tmp.user.drug$Ecstasy = "Ecstasy" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Ecstasy)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Heroin
tmp.user.drug = as.data.table(drugs.clean)[Heroin == 1, .N, by = .(Age, Heroin)]
tmp.user.drug$Heroin = "Heroin" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Heroin)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Ketamine
tmp.user.drug = as.data.table(drugs.clean)[Ketamine == 1, .N, by = .(Age, Ketamine)]
tmp.user.drug$Ketamine = "Ketamine" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Ketamine)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Legalh  
tmp.user.drug = as.data.table(drugs.clean)[Legalh == 1, .N, by = .(Age, Legalh)]
tmp.user.drug$Legalh = "Legalh" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Legalh)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# LSD
tmp.user.drug = as.data.table(drugs.clean)[LSD == 1, .N, by = .(Age, LSD)]
tmp.user.drug$LSD = "LSD" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = LSD)
users.for.drug = rbind(users.for.drug, tmp.user.drug)  

# Meth
tmp.user.drug = as.data.table(drugs.clean)[Meth == 1, .N, by = .(Age, Meth)]
tmp.user.drug$Meth = "Meth" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Meth)
users.for.drug = rbind(users.for.drug, tmp.user.drug)  

# Mushrooms
tmp.user.drug = as.data.table(drugs.clean)[Mushrooms == 1, .N, by = .(Age, Mushrooms)]
tmp.user.drug$Mushrooms = "Mushrooms" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Mushrooms)
users.for.drug = rbind(users.for.drug, tmp.user.drug) 

# Nicotine
tmp.user.drug = as.data.table(drugs.clean)[Nicotine == 1, .N, by = .(Age, Nicotine)]
tmp.user.drug$Nicotine = "Nicotine" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Nicotine)
users.for.drug = rbind(users.for.drug, tmp.user.drug) 

# Semer 
tmp.user.drug = as.data.table(drugs.clean)[Semer == 1, .N, by = .(Age, Semer)]
tmp.user.drug$Nicotine = "Semeron " 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Semer)
users.for.drug = rbind(users.for.drug, tmp.user.drug) 

# VSA   
tmp.user.drug = as.data.table(drugs.clean)[VSA == 1, .N, by = .(Age, VSA)]
tmp.user.drug$VSA = "VSA" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = VSA)
users.for.drug = rbind(users.for.drug, tmp.user.drug) 

# Plot Drug Users By Age and Type ####
nb.cols = 18
cbPalette = c(
  "#264653","#e9c46a","#b97113","#14213d","#e63946","#faf0ca","#163c37","#a23216","#606c38","#327e75",
  "#220901","#621708","#941b0c","#bc3908","#f6aa1c", "#F4978E", "#D9D9D9", "#7678ED")
hist.age = ggplot(arrange(users.for.drug, N, Drug)) + 
  geom_bar(aes(x = Age, y = N, fill = reorder(Drug, -N)), size = 4, alpha = 1, stat = "identity") + 
  scale_fill_manual(values = cbPalette)+
  theme_minimal() +
  labs(y = "Count", fill = "Drugs") + 
  theme(legend.position = "left",
        aspect.ratio = 1, 
        title = element_text(size = 20),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.key = element_blank(), legend.key.size = unit(1,"line"),
        legend.title=element_text(size=20)) 
hist.age
ggsave(paste(images.dir, "hist_age.pdf", sep = ""), hist.age, device = "pdf", width = 20, height = 20)

# How is drug usage distributed across gender? #### 

users.for.drug = tibble()

# Alcohol
tmp.user.drug = as.data.table(drugs.clean)[Alcohol == 1, .N, by = .(Gender, Alcohol)]
tmp.user.drug$Alcohol = "Alcohol" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Alcohol)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Amphet
tmp.user.drug = as.data.table(drugs.clean)[Amphet == 1, .N, by = .(Gender, Amphet)]
tmp.user.drug$Amphet = "Amphet" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Amphet)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Amyl
tmp.user.drug = as.data.table(drugs.clean)[Amyl == 1, .N, by = .(Gender, Amyl)]
tmp.user.drug$Amyl = "Amyl" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Amyl)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Benzos  
tmp.user.drug = as.data.table(drugs.clean)[Benzos == 1, .N, by = .(Gender, Benzos)]
tmp.user.drug$Benzos = "Benzos" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Benzos)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Caff      
tmp.user.drug = as.data.table(drugs.clean)[Caff == 1, .N, by = .(Gender, Caff)]
tmp.user.drug$Caff = "Caffeine" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Caff)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Cannabis      
tmp.user.drug = as.data.table(drugs.clean)[Cannabis == 1, .N, by = .(Gender, Cannabis)]
tmp.user.drug$Cannabis = "Cannabis" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Cannabis)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Choc          
tmp.user.drug = as.data.table(drugs.clean)[Choc == 1, .N, by = .(Gender, Choc)]
tmp.user.drug$Choc = "Chocolate" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Choc)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Coke              
tmp.user.drug = as.data.table(drugs.clean)[Coke == 1, .N, by = .(Gender, Coke)]
tmp.user.drug$Coke = "Cocaine" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Coke)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Crack
tmp.user.drug = as.data.table(drugs.clean)[Crack == 1, .N, by = .(Gender, Crack)]
tmp.user.drug$Crack = "Crack" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Crack)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Ecstasy
tmp.user.drug = as.data.table(drugs.clean)[Ecstasy == 1, .N, by = .(Gender, Ecstasy)]
tmp.user.drug$Ecstasy = "Ecstasy" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Ecstasy)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Heroin
tmp.user.drug = as.data.table(drugs.clean)[Heroin == 1, .N, by = .(Gender, Heroin)]
tmp.user.drug$Heroin = "Heroin" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Heroin)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Ketamine
tmp.user.drug = as.data.table(drugs.clean)[Ketamine == 1, .N, by = .(Gender, Ketamine)]
tmp.user.drug$Ketamine = "Ketamine" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Ketamine)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Legalh  
tmp.user.drug = as.data.table(drugs.clean)[Legalh == 1, .N, by = .(Gender, Legalh)]
tmp.user.drug$Legalh = "Legalh" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Legalh)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# LSD
tmp.user.drug = as.data.table(drugs.clean)[LSD == 1, .N, by = .(Gender, LSD)]
tmp.user.drug$LSD = "LSD" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = LSD)
users.for.drug = rbind(users.for.drug, tmp.user.drug)  

# Meth
tmp.user.drug = as.data.table(drugs.clean)[Meth == 1, .N, by = .(Gender, Meth)]
tmp.user.drug$Meth = "Meth" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Meth)
users.for.drug = rbind(users.for.drug, tmp.user.drug)  

# Mushrooms
tmp.user.drug = as.data.table(drugs.clean)[Mushrooms == 1, .N, by = .(Gender, Mushrooms)]
tmp.user.drug$Mushrooms = "Mushrooms" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Mushrooms)
users.for.drug = rbind(users.for.drug, tmp.user.drug) 

# Nicotine
tmp.user.drug = as.data.table(drugs.clean)[Nicotine == 1, .N, by = .(Gender, Nicotine)]
tmp.user.drug$Nicotine = "Nicotine" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Nicotine)
users.for.drug = rbind(users.for.drug, tmp.user.drug) 

# Semer 
tmp.user.drug = as.data.table(drugs.clean)[Semer == 1, .N, by = .(Gender, Semer)]
tmp.user.drug$Nicotine = "Semeron " 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Semer)
users.for.drug = rbind(users.for.drug, tmp.user.drug) 

# VSA   
tmp.user.drug = as.data.table(drugs.clean)[VSA == 1, .N, by = .(Gender, VSA)]
tmp.user.drug$VSA = "VSA" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = VSA)
users.for.drug = rbind(users.for.drug, tmp.user.drug) 

# Plot Drug Users By Gender and Type ####
cbPalette = c(
  "#264653","#e9c46a","#b97113","#14213d","#e63946","#faf0ca","#163c37","#a23216","#606c38","#327e75",
  "#220901","#621708","#941b0c","#bc3908","#f6aa1c", "#F4978E", "#D9D9D9", "#7678ED")
hist.gender = ggplot(arrange(users.for.drug, N, Drug)) + 
  geom_bar(aes(x = Gender, y = N, fill = reorder(Drug, -N)), size = 4, alpha = 1, stat = "identity") + 
  scale_fill_manual(values = cbPalette)+
  theme_minimal() +
  labs(y = "Count", fill = "Drugs") + 
  theme(legend.position = "left",
        aspect.ratio = 1, 
        title = element_text(size = 20),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.key = element_blank(), legend.key.size = unit(1,"line"),
        legend.title=element_text(size=20)) 
hist.gender
ggsave(paste(images.dir, "hist_.pdf", sep = ""), hist.gender, device = "pdf", width = 20, height = 20)

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

# How is drug usage distributed across education? #### 

users.for.drug = tibble()

# Alcohol
tmp.user.drug = as.data.table(drugs.clean)[Alcohol == 1, .N, by = .(Education, Alcohol)]
tmp.user.drug$Alcohol = "Alcohol" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Alcohol)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Amphet
tmp.user.drug = as.data.table(drugs.clean)[Amphet == 1, .N, by = .(Education, Amphet)]
tmp.user.drug$Amphet = "Amphet" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Amphet)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Amyl
tmp.user.drug = as.data.table(drugs.clean)[Amyl == 1, .N, by = .(Education, Amyl)]
tmp.user.drug$Amyl = "Amyl" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Amyl)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Benzos  
tmp.user.drug = as.data.table(drugs.clean)[Benzos == 1, .N, by = .(Education, Benzos)]
tmp.user.drug$Benzos = "Benzos" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Benzos)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Caff      
tmp.user.drug = as.data.table(drugs.clean)[Caff == 1, .N, by = .(Education, Caff)]
tmp.user.drug$Caff = "Caffeine" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Caff)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Cannabis      
tmp.user.drug = as.data.table(drugs.clean)[Cannabis == 1, .N, by = .(Education, Cannabis)]
tmp.user.drug$Cannabis = "Cannabis" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Cannabis)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Choc          
tmp.user.drug = as.data.table(drugs.clean)[Choc == 1, .N, by = .(Education, Choc)]
tmp.user.drug$Choc = "Chocolate" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Choc)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Coke              
tmp.user.drug = as.data.table(drugs.clean)[Coke == 1, .N, by = .(Education, Coke)]
tmp.user.drug$Coke = "Cocaine" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Coke)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Crack
tmp.user.drug = as.data.table(drugs.clean)[Crack == 1, .N, by = .(Education, Crack)]
tmp.user.drug$Crack = "Crack" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Crack)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Ecstasy
tmp.user.drug = as.data.table(drugs.clean)[Ecstasy == 1, .N, by = .(Education, Ecstasy)]
tmp.user.drug$Ecstasy = "Ecstasy" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Ecstasy)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Heroin
tmp.user.drug = as.data.table(drugs.clean)[Heroin == 1, .N, by = .(Education, Heroin)]
tmp.user.drug$Heroin = "Heroin" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Heroin)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Ketamine
tmp.user.drug = as.data.table(drugs.clean)[Ketamine == 1, .N, by = .(Education, Ketamine)]
tmp.user.drug$Ketamine = "Ketamine" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Ketamine)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Legalh  
tmp.user.drug = as.data.table(drugs.clean)[Legalh == 1, .N, by = .(Education, Legalh)]
tmp.user.drug$Legalh = "Legalh" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Legalh)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# LSD
tmp.user.drug = as.data.table(drugs.clean)[LSD == 1, .N, by = .(Education, LSD)]
tmp.user.drug$LSD = "LSD" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = LSD)
users.for.drug = rbind(users.for.drug, tmp.user.drug)  

# Meth
tmp.user.drug = as.data.table(drugs.clean)[Meth == 1, .N, by = .(Education, Meth)]
tmp.user.drug$Meth = "Meth" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Meth)
users.for.drug = rbind(users.for.drug, tmp.user.drug)  

# Mushrooms
tmp.user.drug = as.data.table(drugs.clean)[Mushrooms == 1, .N, by = .(Education, Mushrooms)]
tmp.user.drug$Mushrooms = "Mushrooms" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Mushrooms)
users.for.drug = rbind(users.for.drug, tmp.user.drug) 

# Nicotine
tmp.user.drug = as.data.table(drugs.clean)[Nicotine == 1, .N, by = .(Education, Nicotine)]
tmp.user.drug$Nicotine = "Nicotine" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Nicotine)
users.for.drug = rbind(users.for.drug, tmp.user.drug) 

# Semer 
tmp.user.drug = as.data.table(drugs.clean)[Semer == 1, .N, by = .(Education, Semer)]
tmp.user.drug$Nicotine = "Semeron " 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Semer)
users.for.drug = rbind(users.for.drug, tmp.user.drug) 

# VSA   
tmp.user.drug = as.data.table(drugs.clean)[VSA == 1, .N, by = .(Education, VSA)]
tmp.user.drug$VSA = "VSA" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = VSA)
users.for.drug = rbind(users.for.drug, tmp.user.drug) 

# Plot Drug Users By Education and Type ####
nb.cols = 18
cbPalette = c(
  "#264653","#e9c46a","#b97113","#14213d","#e63946","#faf0ca","#163c37","#a23216","#606c38","#327e75",
  "#220901","#621708","#941b0c","#bc3908","#f6aa1c", "#F4978E", "#D9D9D9", "#7678ED")
hist.education = ggplot(arrange(users.for.drug, N, Drug)) + 
  geom_bar(aes(x = Education, y = N, fill = reorder(Drug, -N)), size = 4, alpha = 1, stat = "identity") + 
  scale_fill_manual(values = cbPalette) +
  scale_x_discrete(labels = c("< 16", "16", "17", "18", "College", "Diploma", "BS", "MS", "PhD")) +
  theme_minimal() +
  labs(y = "Count", fill = "Drugs") + 
  theme(legend.position = "left",
        aspect.ratio = 1, 
        title = element_text(size = 20),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 20, angle = 90, hjust = 1),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.key = element_blank(), legend.key.size = unit(1,"line"),
        legend.title=element_text(size=20)) 
hist.education
ggsave(paste(images.dir, "hist_education.pdf", sep = ""), hist.education, device = "pdf", width = 20, height = 20)

# How is drug usage distributed across countries? #### 

users.for.drug = tibble()

# Alcohol
tmp.user.drug = as.data.table(drugs.clean)[Alcohol == 1, .N, by = .(Country, Alcohol)]
tmp.user.drug$Alcohol = "Alcohol" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Alcohol)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Amphet
tmp.user.drug = as.data.table(drugs.clean)[Amphet == 1, .N, by = .(Country, Amphet)]
tmp.user.drug$Amphet = "Amphet" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Amphet)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Amyl
tmp.user.drug = as.data.table(drugs.clean)[Amyl == 1, .N, by = .(Country, Amyl)]
tmp.user.drug$Amyl = "Amyl" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Amyl)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Benzos  
tmp.user.drug = as.data.table(drugs.clean)[Benzos == 1, .N, by = .(Country, Benzos)]
tmp.user.drug$Benzos = "Benzos" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Benzos)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Caff      
tmp.user.drug = as.data.table(drugs.clean)[Caff == 1, .N, by = .(Country, Caff)]
tmp.user.drug$Caff = "Caffeine" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Caff)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Cannabis      
tmp.user.drug = as.data.table(drugs.clean)[Cannabis == 1, .N, by = .(Country, Cannabis)]
tmp.user.drug$Cannabis = "Cannabis" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Cannabis)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Choc          
tmp.user.drug = as.data.table(drugs.clean)[Choc == 1, .N, by = .(Country, Choc)]
tmp.user.drug$Choc = "Chocolate" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Choc)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Coke              
tmp.user.drug = as.data.table(drugs.clean)[Coke == 1, .N, by = .(Country, Coke)]
tmp.user.drug$Coke = "Cocaine" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Coke)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Crack
tmp.user.drug = as.data.table(drugs.clean)[Crack == 1, .N, by = .(Country, Crack)]
tmp.user.drug$Crack = "Crack" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Crack)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Ecstasy
tmp.user.drug = as.data.table(drugs.clean)[Ecstasy == 1, .N, by = .(Country, Ecstasy)]
tmp.user.drug$Ecstasy = "Ecstasy" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Ecstasy)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Heroin
tmp.user.drug = as.data.table(drugs.clean)[Heroin == 1, .N, by = .(Country, Heroin)]
tmp.user.drug$Heroin = "Heroin" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Heroin)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Ketamine
tmp.user.drug = as.data.table(drugs.clean)[Ketamine == 1, .N, by = .(Country, Ketamine)]
tmp.user.drug$Ketamine = "Ketamine" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Ketamine)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Legalh  
tmp.user.drug = as.data.table(drugs.clean)[Legalh == 1, .N, by = .(Country, Legalh)]
tmp.user.drug$Legalh = "Legalh" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Legalh)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# LSD
tmp.user.drug = as.data.table(drugs.clean)[LSD == 1, .N, by = .(Country, LSD)]
tmp.user.drug$LSD = "LSD" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = LSD)
users.for.drug = rbind(users.for.drug, tmp.user.drug)  

# Meth
tmp.user.drug = as.data.table(drugs.clean)[Meth == 1, .N, by = .(Country, Meth)]
tmp.user.drug$Meth = "Meth" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Meth)
users.for.drug = rbind(users.for.drug, tmp.user.drug)  

# Mushrooms
tmp.user.drug = as.data.table(drugs.clean)[Mushrooms == 1, .N, by = .(Country, Mushrooms)]
tmp.user.drug$Mushrooms = "Mushrooms" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Mushrooms)
users.for.drug = rbind(users.for.drug, tmp.user.drug) 

# Nicotine
tmp.user.drug = as.data.table(drugs.clean)[Nicotine == 1, .N, by = .(Country, Nicotine)]
tmp.user.drug$Nicotine = "Nicotine" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Nicotine)
users.for.drug = rbind(users.for.drug, tmp.user.drug) 

# Semer 
tmp.user.drug = as.data.table(drugs.clean)[Semer == 1, .N, by = .(Country, Semer)]
tmp.user.drug$Nicotine = "Semeron " 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Semer)
users.for.drug = rbind(users.for.drug, tmp.user.drug) 

# VSA   
tmp.user.drug = as.data.table(drugs.clean)[VSA == 1, .N, by = .(Country, VSA)]
tmp.user.drug$VSA = "VSA" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = VSA)
users.for.drug = rbind(users.for.drug, tmp.user.drug) 

# Plot Drug Users By Country and Type ####
nb.cols = 18
cbPalette = c(
  "#264653","#e9c46a","#b97113","#14213d","#e63946","#faf0ca","#163c37","#a23216","#606c38","#327e75",
  "#220901","#621708","#941b0c","#bc3908","#f6aa1c", "#F4978E", "#D9D9D9", "#7678ED")
hist.countries = ggplot(arrange(users.for.drug, N, Drug)) + 
  geom_bar(aes(x = Country, y = N, fill = reorder(Drug, -N)), size = 4, alpha = 1, stat = "identity") + 
  scale_fill_manual(values = cbPalette) +
  theme_minimal() +
  labs(y = "Count", fill = "Drugs") + 
  theme(legend.position = "left",
        aspect.ratio = 1, 
        title = element_text(size = 20),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 20, angle = 90, hjust = 1),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.key = element_blank(), legend.key.size = unit(1,"line"),
        legend.title=element_text(size=20)) 
hist.countries
ggsave(paste(images.dir, "hist_countries.pdf", sep = ""), hist.countries, device = "pdf", width = 20, height = 20)


# How is drug usage distributed across countries? #### 

users.for.drug = tibble()

# Alcohol
tmp.user.drug = as.data.table(drugs.clean)[Alcohol == 1, .N, by = .(Ethnicity, Alcohol)]
tmp.user.drug$Alcohol = "Alcohol" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Alcohol)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Amphet
tmp.user.drug = as.data.table(drugs.clean)[Amphet == 1, .N, by = .(Ethnicity, Amphet)]
tmp.user.drug$Amphet = "Amphet" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Amphet)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Amyl
tmp.user.drug = as.data.table(drugs.clean)[Amyl == 1, .N, by = .(Ethnicity, Amyl)]
tmp.user.drug$Amyl = "Amyl" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Amyl)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Benzos  
tmp.user.drug = as.data.table(drugs.clean)[Benzos == 1, .N, by = .(Ethnicity, Benzos)]
tmp.user.drug$Benzos = "Benzos" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Benzos)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Caff      
tmp.user.drug = as.data.table(drugs.clean)[Caff == 1, .N, by = .(Ethnicity, Caff)]
tmp.user.drug$Caff = "Caffeine" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Caff)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Cannabis      
tmp.user.drug = as.data.table(drugs.clean)[Cannabis == 1, .N, by = .(Ethnicity, Cannabis)]
tmp.user.drug$Cannabis = "Cannabis" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Cannabis)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Choc          
tmp.user.drug = as.data.table(drugs.clean)[Choc == 1, .N, by = .(Ethnicity, Choc)]
tmp.user.drug$Choc = "Chocolate" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Choc)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Coke              
tmp.user.drug = as.data.table(drugs.clean)[Coke == 1, .N, by = .(Ethnicity, Coke)]
tmp.user.drug$Coke = "Cocaine" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Coke)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Crack
tmp.user.drug = as.data.table(drugs.clean)[Crack == 1, .N, by = .(Ethnicity, Crack)]
tmp.user.drug$Crack = "Crack" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Crack)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Ecstasy
tmp.user.drug = as.data.table(drugs.clean)[Ecstasy == 1, .N, by = .(Ethnicity, Ecstasy)]
tmp.user.drug$Ecstasy = "Ecstasy" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Ecstasy)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Heroin
tmp.user.drug = as.data.table(drugs.clean)[Heroin == 1, .N, by = .(Ethnicity, Heroin)]
tmp.user.drug$Heroin = "Heroin" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Heroin)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Ketamine
tmp.user.drug = as.data.table(drugs.clean)[Ketamine == 1, .N, by = .(Ethnicity, Ketamine)]
tmp.user.drug$Ketamine = "Ketamine" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Ketamine)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# Legalh  
tmp.user.drug = as.data.table(drugs.clean)[Legalh == 1, .N, by = .(Ethnicity, Legalh)]
tmp.user.drug$Legalh = "Legalh" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Legalh)
users.for.drug = rbind(users.for.drug, tmp.user.drug)

# LSD
tmp.user.drug = as.data.table(drugs.clean)[LSD == 1, .N, by = .(Ethnicity, LSD)]
tmp.user.drug$LSD = "LSD" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = LSD)
users.for.drug = rbind(users.for.drug, tmp.user.drug)  

# Meth
tmp.user.drug = as.data.table(drugs.clean)[Meth == 1, .N, by = .(Ethnicity, Meth)]
tmp.user.drug$Meth = "Meth" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Meth)
users.for.drug = rbind(users.for.drug, tmp.user.drug)  

# Mushrooms
tmp.user.drug = as.data.table(drugs.clean)[Mushrooms == 1, .N, by = .(Ethnicity, Mushrooms)]
tmp.user.drug$Mushrooms = "Mushrooms" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Mushrooms)
users.for.drug = rbind(users.for.drug, tmp.user.drug) 

# Nicotine
tmp.user.drug = as.data.table(drugs.clean)[Nicotine == 1, .N, by = .(Ethnicity, Nicotine)]
tmp.user.drug$Nicotine = "Nicotine" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Nicotine)
users.for.drug = rbind(users.for.drug, tmp.user.drug) 

# Semer 
tmp.user.drug = as.data.table(drugs.clean)[Semer == 1, .N, by = .(Ethnicity, Semer)]
tmp.user.drug$Nicotine = "Semeron " 
tmp.user.drug = tmp.user.drug %>% rename(Drug = Semer)
users.for.drug = rbind(users.for.drug, tmp.user.drug) 

# VSA   
tmp.user.drug = as.data.table(drugs.clean)[VSA == 1, .N, by = .(Ethnicity, VSA)]
tmp.user.drug$VSA = "VSA" 
tmp.user.drug = tmp.user.drug %>% rename(Drug = VSA)
users.for.drug = rbind(users.for.drug, tmp.user.drug) 

# Plot Drug Users By Ethnicity and Type ####
cbPalette = c(
  "#264653","#e9c46a","#b97113","#14213d","#e63946","#faf0ca","#163c37","#a23216","#606c38","#327e75",
  "#220901","#621708","#941b0c","#bc3908","#f6aa1c", "#F4978E", "#D9D9D9", "#7678ED")
hist.ethnicity = ggplot(arrange(users.for.drug, N, Drug)) + 
  geom_bar(aes(x = Ethnicity, y = N, fill = reorder(Drug, -N)), size = 4, alpha = 1, stat = "identity") + 
  scale_fill_manual(values = cbPalette) +
  theme_minimal() +
  labs(y = "Count", fill = "Drugs") + 
  theme(legend.position = "left",
        aspect.ratio = 1, 
        title = element_text(size = 20),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 20, angle = 90, hjust = 1),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.key = element_blank(), legend.key.size = unit(1,"line"),
        legend.title=element_text(size=20)) 
hist.ethnicity
ggsave(paste(images.dir, "hist_ethnicity.pdf", sep = ""), hist.ethnicity, device = "pdf", width = 20, height = 20)

# Remove everything ####
rm(list = ls())
