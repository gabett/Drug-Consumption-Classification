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

# Global Variables 
dir = "C:\\Users\\ettag\\Documents\\GitHub\\Stastistical-Learning-Project\\data"
setwd(dir = dir)

# Load
data.dir = paste(dir, "\\drug_data_clean.RData", sep = "")
load(data.dir)

images.dir = "C:\\Users\\ettag\\Documents\\GitHub\\Stastistical-Learning-Project\\images\\EDA\\CategoryOverview\\"
setwd(dir = images.dir)

cbPalette = c(
  "#264653","#e9c46a","#b97113","#14213d","#e63946","#faf0ca","#163c37","#a23216","#606c38","#327e75",
  "#220901","#621708","#941b0c","#bc3908","#f6aa1c", "#F4978E", "#D9D9D9", "#7678ED")

# Age histogram ####
hist.age = ggplot(drugs.clean) + 
  geom_histogram(aes(x = Age, fill = Age), alpha = 1, stat = "count", show.legend = F) + 
  theme_minimal() +
  labs(y = "Count", fill = "Drugs", title = "Age Distribution") + 
  scale_fill_manual(values = c("#264653","#e9c46a","#b97113","#14213d","#e63946", "#606c38"))+
  theme(legend.position = "left",
        aspect.ratio = 1, 
        title = element_text(size= 12),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size= 12),
        axis.text.y = element_text(size= 12),
        axis.title.y = element_text(size= 12),
        axis.title.x = element_text(size= 12),
        legend.key = element_blank(), legend.key.size = unit(1,"line"),
        legend.title=element_text(size= 12))
hist.age
ggsave(paste(images.dir, "hist_age.pdf", sep = ""), hist.age, device = "pdf", width= 15, height= 15)

# Users vs Non users Histograms ####
hist.illegal.usage = ggplot(drugs.clean) + 
  geom_histogram(aes(x = has_taken_illegal_drugs, fill = has_taken_illegal_drugs), alpha = 1, stat = "count", show.legend = F) + 
  theme_minimal() +
  labs(x = "Has ever taken illegal drugs?", y = "Count", fill = "Drugs", title = "Drug User Distribution") +
  scale_fill_manual(values = c("#264653","#e63946"))+
  theme(legend.position = "none",
        aspect.ratio = 1, 
        title = element_text(size= 15),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size= 15),
        axis.text.y = element_text(size= 15),
        axis.title.y = element_text(size= 15),
        axis.title.x = element_text(size= 15),
        legend.key = element_blank(), legend.key.size = unit(1,"line"),
        legend.title=element_text(size= 15))
hist.illegal.usage
ggsave(paste(images.dir, "hist_illegal_usage.pdf", sep = ""), hist.illegal.usage, device = "pdf", width= 15, height= 15)


# Gender Histogram ####
hist.gender = ggplot(drugs.clean) + 
  geom_histogram(aes(x = Gender, fill = Gender), alpha = 1, stat = "count", show.legend = F) + 
  theme_minimal() +
  labs(y = "Count", fill = "Drugs", title = "Gender Distribution") + 
  scale_fill_manual(values = c("#264653","#e63946"))+
  theme(legend.position = "left",
        aspect.ratio = 1, 
        title = element_text(size= 15),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size= 15),
        axis.text.y = element_text(size= 15),
        axis.title.y = element_text(size= 15),
        axis.title.x = element_text(size= 15),
        legend.key = element_blank(), legend.key.size = unit(1,"line"),
        legend.title=element_text(size= 15))
hist.gender
ggsave(paste(images.dir, "hist_gender.pdf", sep = ""), hist.gender, device = "pdf", width= 15, height= 15)


# Ethnicity histogram ####
hist.ethnicity = ggplot(drugs.clean) + 
  geom_histogram(aes(x = Ethnicity, fill = Ethnicity), alpha = 1, stat = "count", show.legend = F) + 
  theme_minimal() +
  labs(y = "Count", fill = "Drugs", title = "Ethnicity Distribution") + 
  scale_fill_manual(values = c("#264653","#e9c46a","#b97113","#14213d","#e63946", "#606c38", "#F4978E"))+
  theme(legend.position = "left",
        aspect.ratio = 1, 
        title = element_text(size= 15),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size= 15, hjust = 1, angle = 90),
        axis.text.y = element_text(size= 15),
        axis.title.y = element_text(size= 15),
        axis.title.x = element_text(size= 15),
        legend.key = element_blank(), legend.key.size = unit(1,"line"),
        legend.title=element_text(size= 15))
hist.ethnicity
ggsave(paste(images.dir, "hist_ethnicity.pdf", sep = ""), hist.ethnicity, device = "pdf", width= 15, height= 15)

# Education histogram ####
hist.education = ggplot(drugs.clean) + 
  geom_histogram(aes(x = Education, fill = Education), alpha = 1, stat = "count", show.legend = F) + 
  theme_minimal() +
  labs(y = "Count", fill = "Drugs", title = "Education Distribution") + 
  scale_fill_manual(values = c("#264653","#e9c46a","#b97113","#14213d","#e63946", "#606c38", "#F4978E", "#bc3908","#f6aa1c"))+
  scale_x_discrete(labels = c("< 16", "16", "17", "18", "College", "Diploma", "BS", "MS", "PhD")) +
  theme(legend.position = "none",
        aspect.ratio = 1, 
        title = element_text(size= 15),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size= 15, hjust = 1, angle = 90),
        axis.text.y = element_text(size= 15),
        axis.title.y = element_text(size= 15),
        axis.title.x = element_text(size= 15),
        legend.key = element_blank(), legend.key.size = unit(1,"line"),
        legend.title=element_text(size= 15))
hist.education
ggsave(paste(images.dir, "hist_education.pdf", sep = ""), hist.ducation, device = "pdf", width= 15, height= 15)

# Country Histogram
hist.country = ggplot(drugs.clean) + 
geom_histogram(aes(x = Country, fill = Country), alpha = 1, stat = "count", show.legend = F) + 
theme_minimal() +
labs(y = "Count", fill = "Drugs", title = "Country Distribution") + 
scale_fill_manual(values = c("#264653","#e9c46a","#b97113","#14213d","#e63946", "#606c38", "#F4978E", "#bc3908","#f6aa1c"))+
scale_x_discrete(labels = c("USA", "NZ", "Others", "AUS", "Ireland", "Canada", "UK")) +
theme(legend.position = "none",
        aspect.ratio = 1, 
        title = element_text(size= 15),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size= 15, hjust = 1, angle = 90),
        axis.text.y = element_text(size= 15),
        axis.title.y = element_text(size= 15),
        axis.title.x = element_text(size= 15),
        legend.key = element_blank(), legend.key.size = unit(1,"line"),
        legend.title=element_text(size= 15))
hist.country
ggsave(paste(images.dir, "hist_country.pdf", sep = ""), hist.country, device = "pdf", width= 15, height= 15)

# Plot all histograms together ####

hist.compound = grid.arrange(hist.illegal.usage, hist.gender, hist.age, hist.ethnicity, hist.education, hist.country, nrow = 2, ncol = 3)
ggsave(paste(images.dir, "hist_compound.pdf", sep = ""), hist.compound, device = "pdf", width= 15, height= 15)

