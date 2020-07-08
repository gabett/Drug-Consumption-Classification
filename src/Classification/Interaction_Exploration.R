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
library(reshape2)
# Global Variables 
dir = "C:\\Users\\ettag\\Documents\\GitHub\\Stastistical-Learning-Project\\data"
setwd(dir = dir)

# Load
data.dir = paste(dir, "\\drug_data_clean.RData", sep = "")
load(data.dir)

images.dir = "C:\\Users\\ettag\\Documents\\GitHub\\Stastistical-Learning-Project\\images\\Classification\\"
setwd(dir = images.dir)

# Continuous variables exploration ####

continuous.variables = drugs.clean[, c("Nscore","Escore", "Oscore", "Ascore" ,"Cscore", "Impulsive","SS")]
cormat <- round(cor(continuous.variables),2)
melted_cormat <- melt(cormat)
heatmap.corr.cont = ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + 
  geom_text(aes(label = round(value, 1))) +
  scale_fill_gradient2(low = "darkred", mid = "white", high = "darkgreen") +
  labs(x = "", y = "", fill = "Correlation") + 
  theme(legend.position = "left",
        aspect.ratio = 1, 
        title = element_text(size= 15),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size= 15),
        axis.text.y = element_text(size= 15),
        axis.title.y = element_text(size= 15),
        axis.title.x = element_text(size= 15),
        legend.key = element_blank(), legend.key.size = unit(2,"line"),
        legend.title=element_text(size= 15))
heatmap.corr.cont
ggsave(paste(images.dir, "heatmap_corr_cont.pdf", sep = ""), heatmap.corr.cont, device = "pdf", width = 15, height = 15)

# The only meaningful relationship seems to be between Impulsive and SS.

pairs(continuous.variables)

# Personal categorical variable
personal.categorical.variables = drugs.clean[, c("Age", "Gender", "Education", "Country", "Ethnicity")]
chisq.test(personal.categorical.variables$Age, personal.categorical.variables$Gender) # Dependent
chisq.test(personal.categorical.variables$Age, personal.categorical.variables$Education) # Dependent
chisq.test(personal.categorical.variables$Age, personal.categorical.variables$Country) # Dependent
chisq.test(personal.categorical.variables$Age, personal.categorical.variables$Ethnicity) # Independent

chisq.test(personal.categorical.variables$Gender, personal.categorical.variables$Education) # Dependent
chisq.test(personal.categorical.variables$Gender, personal.categorical.variables$Education) # Dependent
chisq.test(personal.categorical.variables$Gender, personal.categorical.variables$Country) # Dependent
chisq.test(personal.categorical.variables$Gender, personal.categorical.variables$Ethnicity) # Independent

chisq.test(personal.categorical.variables$Education, personal.categorical.variables$Country) # Dependent
chisq.test(personal.categorical.variables$Education, personal.categorical.variables$Ethnicity) # Independent

chisq.test(personal.categorical.variables$Country, personal.categorical.variables$Ethnicity) # Dependent

# Clean
rm(list = ls())
