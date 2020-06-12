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

images.dir = "C:\\Users\\ettag\\Documents\\GitHub\\Stastistical-Learning-Project\\images\\EDA\\CategoryOverview\\"
setwd(dir = images.dir)

cbPalette = c(
  "#264653","#e9c46a","#b97113","#14213d","#e63946","#faf0ca","#163c37","#a23216","#606c38","#327e75",
  "#220901","#621708","#941b0c","#bc3908","#f6aa1c", "#F4978E", "#D9D9D9", "#7678ED")

# Gender histogram ####
hist.age = ggplot(drugs.clean) + 
  geom_histogram(aes(x = Age, fill = Age), alpha = 1, stat = "count") + 
  theme_minimal() +
  labs(y = "Count", fill = "Drugs") + 
  scale_fill_manual(values = c("#264653","#e9c46a","#b97113","#14213d","#e63946", "#606c38"))+
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