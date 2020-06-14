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
library(arules)       # Association analysis
library(arulesViz)    # Visualizing for association analysis

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

# For each patient, we create ad edge for each drug who has been taken (value equal to 1).
edge.list = tibble()
for(i in seq(1:nrow(drug.categoricals)))
{
  row.tmp = drug.categoricals[i, ] 
  
  if(length(colnames(row.tmp)[row.tmp == TRUE]) > 1)
  {
    edges.tmp = t(combn(colnames(row.tmp)[row.tmp == TRUE], 2))
    edge.list = rbind(edge.list, edges.tmp)
  }
}

grouped.edge.list <- edge.list %>% group_by(V1,V2) %>% count() %>% rename(weight=n)

drug.graph <- graph.data.frame(grouped.edge.list, directed = F)

# Edge betweenness clustering
eb <- cluster_edge_betweenness(drug.graph)
membership(eb)
communities(eb)

V(drug.graph)$degree = degree(drug.graph)
V(drug.graph)$name = names(V(drug.graph))
E(drug.graph)$weight = grouped.edge.list$weight
V(drug.graph)$membership = eb$membership

membership.colors = eb$membership
membership.colors[membership.colors == 1] = "#ffd166"
membership.colors[membership.colors == 2] = "#02c39a"
membership.colors[membership.colors == 3] = "#e63946"
V(drug.graph)$membership.colors =  membership.colors

# Graph Visualization ####
forgraph <- ggnetwork(drug.graph, layout = layout.davidson.harel(drug.graph))

graph.plot = ggplot(forgraph,aes(x=x,y=y,xend=xend,yend=yend))+
  geom_edges(size = 1.5, color = "gray", show.legend = FALSE, alpha = 0.3) + 
  geom_nodes(size = 25, color = membership.colors, show.legend = FALSE) +
  theme_blank()+
  geom_nodetext_repel(aes(label = name), fontface = "bold", color = "#003049", size = 10) + 
  theme(legend.position = "right",
        aspect.ratio = 1, 
        legend.key = element_blank(), legend.key.size = unit(1,"line"),
        legend.title=element_text(size=18), legend.text=element_text(size=18))
graph.plot


ggsave(paste(images.dir, "drug_association.pdf", sep = ""), graph.plot, width = 15, height = 15, device = "pdf")

rm(list = ls())

