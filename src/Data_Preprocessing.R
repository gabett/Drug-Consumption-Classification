# Dataset available at: https://archive.ics.uci.edu/ml/datasets/Drug+consumption+%28quantified%29

# Libraries
library(tibble)
library(data.table)
library(dplyr)
library(ggplot2)
library(gridExtra)
# Global Variables 
dir = "C:\\Users\\ettag\\Documents\\GitHub\\Stastistical-Learning-Project\\data"

dir.images = "C:\\Users\\ettag\\Documents\\GitHub\\Stastistical-Learning-Project\\images\\Preprocessing\\"

show_plot = TRUE
setwd(dir = dir)

# Loading the dataset
drug.csv.directory = paste(dir, "\\drug_consumption.csv", sep = "")

drugs.data = read.csv(drug.csv.directory, header = T, sep = ",", stringsAsFactors = FALSE)
drugs.data = as.data.table(drugs.data)

dim(drugs.data)[1]  # 1885 rows

summary(drugs.data)

# Rename id column
drugs.data = drugs.data %>% rename(id = ï..ID)

sum(is.na(drugs.data)) # No NA

# Checking for normality of continuous variables (by using QQplots) ####

  str(drugs.data)
    
  # Age
  qqplot.age = ggplot(drugs.data, aes(sample = Age)) + 
    stat_qq() +
    stat_qq_line() + 
    theme_classic() +
    labs(y = "Age") + 
    theme(legend.position = "right",
          aspect.ratio = 1, 
          title = element_text(size = 28),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(size = 28),
          axis.text.y = element_text(size = 28),
          axis.title.y = element_text(size = 28),
          axis.title.x = element_text(size = 28),
          legend.key = element_blank(), legend.key.size = unit(3,"line"),
          legend.title=element_text(size=28))
  qqplot.age
  ggsave(paste(dir.images, "age_qqplot.pdf", sep = ""), qqplot.age, device = "pdf", width = 20, height = 20)
  
  # Gender
  qqplot.gender = ggplot(drugs.data, aes(sample = Gender)) + 
    stat_qq() +
    stat_qq_line() + 
    theme_classic() +
    labs(y = "Gender") + 
    theme(legend.position = "right",
          aspect.ratio = 1, 
          title = element_text(size = 28),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(size = 28),
          axis.text.y = element_text(size = 28),
          axis.title.y = element_text(size = 28),
          axis.title.x = element_text(size = 28),
          legend.key = element_blank(), legend.key.size = unit(3,"line"),
          legend.title=element_text(size=28))
  qqplot.gender
  ggsave(paste(dir.images, "gender_qqplot.pdf", sep = ""), qqplot.gender, device = "pdf", width = 20, height = 20)
  
  # Education
  qqplot.education = ggplot(drugs.data, aes(sample = Education)) + 
    stat_qq() +
    stat_qq_line() + 
    theme_classic() +
    labs(y = "Education") + 
    theme(legend.position = "right",
          aspect.ratio = 1, 
          title = element_text(size = 28),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(size = 28),
          axis.text.y = element_text(size = 28),
          axis.title.y = element_text(size = 28),
          axis.title.x = element_text(size = 28),
          legend.key = element_blank(), legend.key.size = unit(3,"line"),
          legend.title=element_text(size=28))
  qqplot.education
  ggsave(paste(dir.images, "education_qqplot.pdf", sep = ""), qqplot.education, device = "pdf", width = 20, height = 20)
  
  # Country
  qqplot.country = ggplot(drugs.data, aes(sample = Country)) + 
    stat_qq() +
    stat_qq_line() + 
    theme_classic() +
    labs(y = "Country") + 
    theme(legend.position = "right",
          aspect.ratio = 1, 
          title = element_text(size = 28),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(size = 28),
          axis.text.y = element_text(size = 28),
          axis.title.y = element_text(size = 28),
          axis.title.x = element_text(size = 28),
          legend.key = element_blank(), legend.key.size = unit(3,"line"),
          legend.title=element_text(size=28))
  qqplot.country
  ggsave(paste(dir.images, "country_qqplot.pdf", sep = ""), qqplot.country, device = "pdf", width = 20, height = 20)
  
  # Ethnicity
  qqplot.ethnicity = ggplot(drugs.data, aes(sample = Ethnicity)) + 
    stat_qq() +
    stat_qq_line() + 
    theme_classic() +
    labs(y = "Ethnicity") + 
    theme(legend.position = "right",
          aspect.ratio = 1, 
          title = element_text(size = 28),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(size = 28),
          axis.text.y = element_text(size = 28),
          axis.title.y = element_text(size = 28),
          axis.title.x = element_text(size = 28),
          legend.key = element_blank(), legend.key.size = unit(3,"line"),
          legend.title=element_text(size=28))
  qqplot.ethnicity
  ggsave(paste(dir.images, "ethnicity_qqplot.pdf", sep = ""), qqplot.ethnicity, device = "pdf", width = 20, height = 20)
  
  Oscore   
  Ascore   
  Cscore   
  Impulsive
  SS  
  
  # Nscore
  qqplot.nscore = ggplot(drugs.data, aes(sample = Nscore)) + 
    stat_qq() +
    stat_qq_line() + 
    theme_classic() +
    labs(y = "Nscore quantiles", x = "N quantiles") + 
    theme(legend.position = "right",
          aspect.ratio = 1, 
          title = element_text(size = 28),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(size = 28),
          axis.text.y = element_text(size = 28),
          axis.title.y = element_text(size = 28),
          axis.title.x = element_text(size = 28),
          legend.key = element_blank(), legend.key.size = unit(3,"line"),
          legend.title=element_text(size=28))
  qqplot.nscore
  ggsave(paste(dir.images, "nscore_qqplot.pdf", sep = ""), qqplot.nscore, device = "pdf", width = 20, height = 20)
  
  # Escore
  qqplot.escore = ggplot(drugs.data, aes(sample = Escore)) + 
    stat_qq() +
    stat_qq_line() + 
    theme_classic() +
    labs(y = "Escore quantiles", x = "N quantiles") + 
    theme(legend.position = "right",
          aspect.ratio = 1, 
          title = element_text(size = 28),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(size = 28),
          axis.text.y = element_text(size = 28),
          axis.title.y = element_text(size = 28),
          axis.title.x = element_text(size = 28),
          legend.key = element_blank(), legend.key.size = unit(3,"line"),
          legend.title=element_text(size=28))
  qqplot.escore
  ggsave(paste(dir.images, "escore_qqplot.pdf", sep = ""), qqplot.escore, device = "pdf", width = 20, height = 20)
  
  # Oscore
  qqplot.oscore = ggplot(drugs.data, aes(sample = Oscore)) + 
    stat_qq() +
    stat_qq_line() + 
    theme_classic() +
    labs(y = "Oscore quantiles", x = "N quantiles") + 
    theme(legend.position = "right",
          aspect.ratio = 1, 
          title = element_text(size = 28),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(size = 28),
          axis.text.y = element_text(size = 28),
          axis.title.y = element_text(size = 28),
          axis.title.x = element_text(size = 28),
          legend.key = element_blank(), legend.key.size = unit(3,"line"),
          legend.title=element_text(size=28))
  qqplot.oscore
  ggsave(paste(dir.images, "oscore_qqplot.pdf", sep = ""), qqplot.oscore, device = "pdf", width = 20, height = 20)
  
  # Ascore
  qqplot.ascore = ggplot(drugs.data, aes(sample = Ascore)) + 
    stat_qq() +
    stat_qq_line() + 
    theme_classic() +
    labs(y = "Ascore quantiles", x = "N quantiles") + 
    theme(legend.position = "right",
          aspect.ratio = 1, 
          title = element_text(size = 28),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(size = 28),
          axis.text.y = element_text(size = 28),
          axis.title.y = element_text(size = 28),
          axis.title.x = element_text(size = 28),
          legend.key = element_blank(), legend.key.size = unit(3,"line"),
          legend.title=element_text(size=28))
  qqplot.ascore
  ggsave(paste(dir.images, "ascore_qqplot.pdf", sep = ""), qqplot.ascore, device = "pdf", width = 20, height = 20)
  
  # Cscore
  qqplot.cscore = ggplot(drugs.data, aes(sample = Cscore)) + 
    stat_qq() +
    stat_qq_line() + 
    theme_classic() +
    labs(y = "Cscore quantiles", x = "N quantiles") + 
    theme(legend.position = "right",
          aspect.ratio = 1, 
          title = element_text(size = 28),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(size = 28),
          axis.text.y = element_text(size = 28),
          axis.title.y = element_text(size = 28),
          axis.title.x = element_text(size = 28),
          legend.key = element_blank(), legend.key.size = unit(3,"line"),
          legend.title=element_text(size=28))
  qqplot.cscore
  ggsave(paste(dir.images, "cscore_qqplot.pdf", sep = ""), qqplot.cscore, device = "pdf", width = 20, height = 20)
  
  # Impulsive
  qqplot.impulsive = ggplot(drugs.data, aes(sample = Impulsive)) + 
    stat_qq() +
    stat_qq_line() + 
    theme_classic() +
    labs(y = "Impulsive") + 
    theme(legend.position = "right",
          aspect.ratio = 1, 
          title = element_text(size = 28),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(size = 28),
          axis.text.y = element_text(size = 28),
          axis.title.y = element_text(size = 28),
          axis.title.x = element_text(size = 28),
          legend.key = element_blank(), legend.key.size = unit(3,"line"),
          legend.title=element_text(size=28))
  qqplot.impulsive
  ggsave(paste(dir.images, "impulsive_qqplot.pdf", sep = ""), qqplot.impulsive, device = "pdf", width = 20, height = 20)
  
  hist.impulsive = ggplot(drugs.data, aes(x = Impulsive)) + 
    geom_histogram() + 
    theme_classic() +
    labs(y = "Impulsive") + 
    theme(legend.position = "right",
          aspect.ratio = 1, 
          title = element_text(size = 28),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(size = 28),
          axis.text.y = element_text(size = 28),
          axis.title.y = element_text(size = 28),
          axis.title.x = element_text(size = 28),
          legend.key = element_blank(), legend.key.size = unit(3,"line"),
          legend.title=element_text(size=28))
  hist.impulsive
  ggsave(paste(dir.images, "impulsive_hist.pdf", sep = ""), hist.impulsive, device = "pdf", width = 20, height = 20)
  
  # SS
  qqplot.ss = ggplot(drugs.data, aes(sample = SS)) + 
    stat_qq() +
    stat_qq_line() + 
    theme_classic() +
    labs(y = "SS") + 
    theme(legend.position = "right",
          aspect.ratio = 1, 
          title = element_text(size = 28),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(size = 28),
          axis.text.y = element_text(size = 28),
          axis.title.y = element_text(size = 28),
          axis.title.x = element_text(size = 28),
          legend.key = element_blank(), legend.key.size = unit(3,"line"),
          legend.title=element_text(size=28))
  qqplot.ss
  ggsave(paste(dir.images, "ss_qqplot.pdf", sep = ""), qqplot.ss, device = "pdf", width = 20, height = 20)
  
  hist.ss = ggplot(drugs.data, aes(x = SS)) + 
    geom_histogram() + 
    theme_classic() +
    labs(y = "SS") + 
    theme(legend.position = "right",
          aspect.ratio = 1, 
          title = element_text(size = 28),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(size = 28),
          axis.text.y = element_text(size = 28),
          axis.title.y = element_text(size = 28),
          axis.title.x = element_text(size = 28),
          legend.key = element_blank(), legend.key.size = unit(3,"line"),
          legend.title=element_text(size=28))
  hist.ss
  ggsave(paste(dir.images, "impulsive_ss.pdf", sep = ""), hist.ss, device = "pdf", width = 20, height = 20)

  grid.cont.qqplots = grid.arrange(qqplot.ascore, qqplot.cscore, qqplot.cscore, qqplot.nscore,
             qqplot.oscore, qqplot.ss, qqplot.impulsive, nrow = 2, ncol = 4)

  ggsave(paste(dir.images, "cont_qqplots.pdf", sep = ""), grid.cont.qqplots, device = "pdf", width = 20, height = 20)
  
# Variable conversion ####
# Labels are taken from the metadata description at: https://archive.ics.uci.edu/ml/datasets/Drug+consumption+%28quantified%29

# We are not interested (for now) about how much time passed since the last
# use of a drug. Instead, we focus on whether it has been taken on not recently.

# Never used or used over a decade ago: never used
drugs.data[drugs.data == "CL0"] = 0
drugs.data[drugs.data == "CL1"] = 0

# Used in last decade to last day: used
drugs.data[drugs.data == "CL2"] = 1
drugs.data[drugs.data == "CL3"] = 1
drugs.data[drugs.data == "CL4"] = 1
drugs.data[drugs.data == "CL5"] = 1
drugs.data[drugs.data == "CL6"] = 1

# Creating has_taken_drugs column. It is equal to 0 if a user has never tried any drug, 1 otherwise.
drugs.data$has_taken_drugs = 0
for(i in seq(1:dim(drugs.data)[1]))
{
  drugs.row = drugs.data[i, 14:32]
  row.unique.values = unique(unlist(drugs.row))
  if(1 %in% row.unique.values)
  {
    drugs.data[i]$has_taken_drugs = 1
  }
}

# Converting categorical labels to a more readible format. 
drugs.data$Age = as.factor(drugs.data$Age)
drugs.clean <- drugs.data %>%
  mutate(Age = factor(Age, labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"))) %>%
  mutate(Gender = factor(Gender, labels = c('Male', 'Female'))) %>%
  mutate(Education = factor(Education, labels = c("< 16", "16", "17", "18", "College/University", "ProfessionalCert/Diploma", "BS", "MS", "PhD"))) %>%
  mutate(Country = factor(Country, labels = c("USA", "New Zeland", "Other", "Australia", "Ireland","Canada","UK"))) %>%
  mutate(Ethnicity = factor(Ethnicity, labels = c("Black", "Asian", "White", "White/Black", "Other", "White/Asian", "Black/Asian"))) %>%
  mutate(has_taken_drugs = factor(has_taken_drugs, labels = c("No", "Yes"))) %>%
  mutate_at(vars(Alcohol:has_taken_drugs), funs(as.factor)) %>%
  select(-id)

save(drugs.clean, file = paste(dir, "\\drug_data_clean.RData", sep = ""))
rm(list = ls())
