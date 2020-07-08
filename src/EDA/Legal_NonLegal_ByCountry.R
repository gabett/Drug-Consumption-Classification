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
library(mapdata)
library(tidyr)
library(RColorBrewer)
library(pals)
library(rw)

# Global Variables 
dir = "C:\\Users\\ettag\\Documents\\GitHub\\Stastistical-Learning-Project\\data"
setwd(dir = dir)

# Load
data.dir = paste(dir, "\\drug_data_clean.RData", sep = "")
load(data.dir)

images.dir = "C:\\Users\\ettag\\Documents\\GitHub\\Stastistical-Learning-Project\\images\\EDA\\CategoryOverview\\"
setwd(dir = images.dir)

# Preparing data ####
drug.users.per.country = drugs.clean %>% group_by(Country, has_taken_illegal_drugs) %>% summarise(user_count = n())
drug.users.per.country = drug.users.per.country %>% ungroup()

# Calculates the illegal users percentage by country
drug.user.percentage.by.country = tibble()
for(country in unique(drug.users.per.country$Country))
{
  drug.users = as.data.table(drug.users.per.country)[Country == country & has_taken_illegal_drugs == "Yes", user_count]
  non.drug.users = as.data.table(drug.users.per.country)[Country == country & has_taken_illegal_drugs == "No", user_count]
  
  drug.user.percentage = drug.users / (drug.users + non.drug.users)
  tmp.df.drug = tibble(Country = country, Percentage = drug.user.percentage)
  
  drug.user.percentage.by.country = rbind(drug.user.percentage.by.country, tmp.df.drug)
}

# Countries to plot

countries = c(
  "Australia", "New Zealand", "UK", "Ireland", "USA", "Canada"
)

countries.maps <- map_data("world", region = countries)

# Compute the centroid as the mean longitude and lattitude
# Used as label coordinate for country's names
region.lab.data <- countries.maps %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat)) 

# Join with data
drug.users.per.country = drug.user.percentage.by.country %>% left_join(region.lab.data, by = c("Country" = "region"))

# Prepare the legend ####

drug.user.percentage.by.country = rbind(drug.user.percentage.by.country, 
                                        tibble(Country = "Bottom", Percentage = 0))

drug.user.percentage.by.country = rbind(drug.user.percentage.by.country, 
                                        tibble(Country = "Top", Percentage = 1))

legend.plot <- ggplot(drug.user.percentage.by.country, aes(Country, fill = Percentage)) + 
  geom_bar() + 
  scale_fill_gradientn(colours = c("darkblue", "#dc2f02", "darkred"), breaks = c(0, 0.5, 1), limits = c(0,1), 
                       labels= c("0", "50", "100")) + 
  theme(legend.position = "bottom", legend.key = element_blank(), legend.key.size = unit(2,"line"),
        legend.title=element_text(size=20))
legend <- cowplot::get_legend(legend.plot)

grid.newpage()
grid.draw(legend)

# Australia Map ####
auz.map <- map_data("world", region = "Australia")
auz.region = as.data.table(region.lab.data)[region.lab.data$region == "Australia"]
auz.percentage.plot = ggplot(auz.map, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), fill = "#83070D", show.legend = F) +
  geom_text(aes(label = region), color = "white", data = auz.region, size = 15, hjust = 0.5, show.legend = FALSE)+
  scale_fill_gradientn(colours = c("darkblue", "darkred"), values = c(0, 1)) +
  theme(legend.position = "none",
        aspect.ratio = 1, 
        title = element_text(size = 20),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.key = element_blank(), legend.key.size = unit(1,"line"),
        legend.title=element_text(size=20)) +
  theme_void()
auz.percentage.plot
ggsave(paste(images.dir, "auz_percentage.pdf", sep = ""), auz.percentage.plot, device = "pdf", width = 20, height = 20)


# UK Map ####
uk.map <- map_data("world", region = "UK")
uk.region = as.data.table(region.lab.data)[region.lab.data$region == "UK"]
uk.percentage.plot = ggplot(uk.map, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), fill = "#5D394A", show.legend = F) +
  geom_text(aes(label = region), color = "white", data = uk.region, size = 15, vjust = 2, hjust = -1, show.legend = FALSE)+
  scale_fill_gradientn(colours = c("darkblue", "darkred"), values = c(0, 1)) +
  theme(legend.position = "none",
        #aspect.ratio = 1, 
        title = element_text(size = 20),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.key = element_blank(), legend.key.size = unit(1,"line"),
        legend.title=element_text(size=20)) +
  theme_void()
uk.percentage.plot

ggsave(paste(images.dir, "uk_percentage.pdf", sep = ""), uk.percentage.plot, device = "pdf", width = 20, height = 20)


# USA Map ####
usa.map <- map_data("usa")
usa.region = as.data.table(region.lab.data)[region.lab.data$region == "USA"]
usa.percentage.plot = ggplot(usa.map, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), fill = "#900001", show.legend = F) +
  geom_text(aes(label = region), color = "white", data = usa.region, size = 15, hjust = -0.8, vjust = 3, show.legend = FALSE) + 
  theme(legend.position = "none",
        aspect.ratio = 1, 
        title = element_text(size = 20),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.key = element_blank(), legend.key.size = unit(1,"line"),
        legend.title=element_text(size=20)) +
  theme_void()
usa.percentage.plot

ggsave(paste(images.dir, "usa_percentage.pdf", sep = ""), usa.percentage.plot, device = "pdf", width = 20, height = 20)

# Ireland Map ####
ireland.map <- map_data("world", region = "Ireland")
ireland.region = as.data.table(region.lab.data)[region.lab.data$region == "Ireland"]
ireland.percentage.plot = ggplot(ireland.map, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), fill = "#A91001", show.legend = F) +
  geom_text(aes(label = region), color = "white", data = ireland.region, size = 15, hjust = 0, vjust = 1, show.legend = FALSE) + 
  theme(legend.position = "none",
        aspect.ratio = 1, 
        title = element_text(size = 20),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.key = element_blank(), legend.key.size = unit(1,"line"),
        legend.title=element_text(size=20)) +
  theme_void()
ireland.percentage.plot

ggsave(paste(images.dir, "ireland_percentage.pdf", sep = ""), ireland.percentage.plot, device = "pdf", width = 20, height = 20)

# Canada Map ####
canada.map <- map_data("world", region = "Canada")
canada.region = as.data.table(region.lab.data)[region.lab.data$region == "Canada"]
canada.percentage.plot = ggplot(canada.map, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), fill = "#B61802", show.legend = F) +
  geom_text(aes(label = region), color = "white", data = canada.region, size = 15, hjust = 1.2, vjust = 1, show.legend = FALSE) + 
  theme(legend.position = "none",
        aspect.ratio = 1, 
        title = element_text(size = 20),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.key = element_blank(), legend.key.size = unit(1,"line"),
        legend.title=element_text(size=20)) +
  theme_void()
canada.percentage.plot

ggsave(paste(images.dir, "canada_percentage.pdf", sep = ""), canada.percentage.plot, device = "pdf", width = 20, height = 20)


percentage.by.country.compound = grid.arrange(auz.percentage.plot, uk.percentage.plot, usa.percentage.plot, ireland.percentage.plot, canada.percentage.plot, legend, nrow = 3, ncol = 2)
ggsave(paste(images.dir, "countries_percentage.pdf", sep = ""), percentage.by.country.compound, device = "pdf", width = 20, height = 20)

rm(list = ls())
