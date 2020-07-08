# Dataset available at: https://archive.ics.uci.edu/ml/datasets/adult
rm(list = ls())

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
library(glmnet)
library(pRoc)
library(caret)
library(leaps)
library(fastDummies)
library(scales)
# Global Variables ####
dir = "C:\\Users\\ettag\\Documents\\GitHub\\Stastistical-Learning-Project\\data"
setwd(dir = dir)

# Load ####
data.dir = paste(dir, "\\deaths-substance-disorders-age.csv", sep = "")
load(data.dir)

data = read.csv(data.dir)

images.dir = "C:\\Users\\ettag\\Documents\\GitHub\\Stastistical-Learning-Project\\images\\EDA\\"
setwd(dir = images.dir)

data = data %>% 
  mutate(deaths = rowSums(.[4:8]))

data.by.country.year = data %>% 
  filter(Entity %in% c('United States', 'United Kingdom', 'Australia', 'Canada', 'Ireland'))

deaths.by.country = ggplot(data.by.country.year, aes(x=Year, y=deaths)) +
  geom_line(aes(color = Entity), show.legend = FALSE) + 
  geom_area(aes(fill = Entity, group = Entity)) +
  facet_wrap(vars(Entity), scales = "free") + 
  theme_minimal() +
  scale_fill_npg() +
  scale_color_npg() + 
  theme(legend.position = "bottom",
        aspect.ratio = 1,
        title = element_text(size= 20),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size= 20, hjust = 1, angle = 90),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size= 20),
        axis.title.x = element_text(size= 20),
        legend.key = element_blank(10), legend.key.size = unit(1,"line"),
        legend.title=element_text(size = 20),
        legend.text = element_text(size = 15),
        strip.text = element_text(size=20)) +
  labs(fill = "Countries", x = "Year", y = "Number of deaths")
deaths.by.country

ggsave(paste(images.dir, "deaths_by_country.pdf", sep = ""), deaths.by.country, device = "pdf", width= 20, height= 20)

