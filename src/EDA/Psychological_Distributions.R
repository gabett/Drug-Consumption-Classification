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
library(reshape2)
library(patchwork)
rm(list = ls())

# Global Variables 
dir = "C:\\Users\\ettag\\Documents\\GitHub\\Stastistical-Learning-Project\\data"
setwd(dir = dir)

# Load
data.dir = paste(dir, "\\drug_data_clean.RData", sep = "")
load(data.dir)

images.dir = "C:\\Users\\ettag\\Documents\\GitHub\\Stastistical-Learning-Project\\images\\Classification\\"
setwd(dir = images.dir)

ascore_plot = ggplot(data = drugs.clean, aes(x = Ascore)) + 
  geom_histogram() + 
  theme_minimal() + 
  labs(y = "Count") + 
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
ascore_plot

cscore_plot = ggplot(data = drugs.clean, aes(x = Cscore)) + 
  geom_histogram() + 
  theme_minimal() + 
  labs(y = "Count") + 
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
cscore_plot

escore_plot = ggplot(data = drugs.clean, aes(x = Escore)) + 
  geom_histogram() + 
  theme_minimal() + 
  labs(y = "Count") + 
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
escore_plot

nscore_plot = ggplot(data = drugs.clean, aes(x = Nscore)) + 
  geom_histogram() + 
  theme_minimal() + 
  labs(y = "Count") + 
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
nscore_plot

oscore_plot = ggplot(data = drugs.clean, aes(x = Oscore)) + 
  geom_histogram() + 
  theme_minimal() + 
  labs(y = "Count") + 
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
oscore_plot

impulsive_plot = ggplot(data = drugs.clean, aes(x = Impulsive)) + 
  geom_histogram() + 
  theme_minimal() + 
  labs(y = "Count") + 
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
impulsive_plot

ss_plot = ggplot(data = drugs.clean, aes(x = SS)) + 
  geom_histogram() + 
  theme_minimal() + 
  labs(y = "Count") + 
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
ss_plot
(ascore_plot + escore_plot + cscore_plot) / (oscore_plot + nscore_plot) 
(impulsive_plot  + ss_plot)
