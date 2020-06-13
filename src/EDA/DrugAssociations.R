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
library(ggnetwork)
library(igraph)

# Global Variables 
dir = "C:\\Users\\ettag\\Documents\\GitHub\\Stastistical-Learning-Project\\data"
setwd(dir = dir)

# Load
data.dir = paste(dir, "\\drug_data_clean.RData", sep = "")
load(data.dir)

images.dir = "C:\\Users\\ettag\\Documents\\GitHub\\Stastistical-Learning-Project\\images\\EDA\\DrugAssociations\\"
setwd(dir = images.dir)

# Graph Creation ####
drug.categoricals <- drugs.clean %>%
  select(14:31)

for(col.name in colnames(drug.categoricals))
{
  drug.column = drug.categoricals[col.name]
  drug.column[drug.column == 1] = col.name
  
  drug.categoricals[col.name] = drug.column
}

drug.graph <- graph.data.frame(as.matrix(drug.categoricals), directed = T)
V(drug.graph)$degree = degree(drug.graph) 

# Graph Visualization ####
forgraph <- ggnetwork(drug.graph)

graph.plot = ggplot(forgraph,aes(x=x,y=y,xend=xend,yend=yend))+
  geom_edges(color = "gray", show.legend = FALSE) + 
  geom_nodes(size = 10, color = "#900001", show.legend = FALSE) +
  theme_blank()+
  theme(legend.position = "right",
        aspect.ratio = 1, 
        legend.key = element_blank(), legend.key.size = unit(1,"line"),
        legend.title=element_text(size=18), legend.text=element_text(size=18))
graph.plot

ggsave(paste(images.dir, "drug_association.pdf", sep = ""), graph.plot, width = 15, height = 15, device = "pdf")

rm(list = ls())