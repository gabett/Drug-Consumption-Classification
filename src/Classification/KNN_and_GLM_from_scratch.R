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
library(patchwork)
library(leaps)
library(caret)
rm(list = ls())

# Global Variables 
dir = "C:\\Users\\ettag\\Documents\\GitHub\\Stastistical-Learning-Project\\data"
setwd(dir = dir)

images.dir = "C:\\Users\\ettag\\Documents\\GitHub\\Stastistical-Learning-Project\\images\\Classification\\"
setwd(dir = images.dir)

# Load
data.dir = paste(dir, "\\drug_data_clean.RData", sep = "")
load(data.dir)

# Divide dataset in train and test set ####


psyco.cols = c("Nscore","Escore", "Oscore", "Ascore", "Cscore" )

target.col = "has_taken_illegal_drugs"
perc_train =0.80

set.seed(42)

#target = ifelse(drugs.clean[,target.col] == "Yes",1,0)
target = drugs.clean[,target.col] 
index_training <- createDataPartition(y = target ,p = perc_train,list = FALSE)

#preapre train and test data sets
train_data = drugs.clean[index_training, psyco.cols]
train_target = target[index_training]

test_data = drugs.clean[-index_training, psyco.cols]
test_target = target[-index_training]


#verify target distribution
prop.table(table(target))
prop.table(table(train_target))
prop.table(table(test_target))

# knn basic model ####

control = trainControl(method="repeatedcv",repeats = 3,classProbs=TRUE, summaryFunction=twoClassSummary, returnResamp ="all")
knn_model <- train(x=train_data, y=train_target, method = "knn", trControl = control,  tuneLength = 40)

plot(knn_model, xlab = "Number of Neighbors", ylab = "AUC")

knn_prediction = predict(knn_model, newdata=test_data)
knn_matrix = confusionMatrix(knn_prediction, test_target, positive="Yes" )

knn_basic_pROC = pROC::roc(ifelse(test_target == "Yes",1,0),predict(knn_model, newdata=test_data, type="prob")[,"Yes"],
                 plot=TRUE, legacy.axes=TRUE, print.auc=TRUE)
knn_basic_pROC

knn_basic_roc = pROC::ggroc(knn_basic_pROC) +xlab("Specificity") + ylab("Sensitivity") + 
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="darkgrey", linetype="dashed") +
  annotate("text", x = 0.5, y = 0.5, size = 20, label = "AUC == 0.8421", parse = TRUE) +   
  theme_minimal() +
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
knn_basic_roc
ggsave(paste(images.dir, "knn_basic_roc.pdf", sep = ""), knn_basic_roc, device = "pdf", width = 15, height = 15)


#glm comparison
control = trainControl(method="repeatedcv",repeats = 3,classProbs=TRUE, summaryFunction=twoClassSummary, returnResamp ="all")
glm_model <- train(x=train_data, y=train_target, method = "glm", trControl = control,  tuneLength = 20)
glm_prediction = predict(glm_model, newdata=test_data)
glm_matrix = confusionMatrix(glm_prediction, test_target, positive="Yes" )

glm_basic_pROC = pROC::roc(ifelse(test_target == "Yes",1,0),predict(glm_model, newdata=test_data, type="prob")[,"Yes"],
                plot=TRUE, legacy.axes=TRUE, print.auc=TRUE)


glm_basic_roc = pROC::ggroc(glm_basic_pROC) +xlab("Specificity") + ylab("Sensitivity") + 
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="darkgrey", linetype="dashed") +
  annotate("text", x = 0.5, y = 0.5, size = 20, label = "AUC == 0.8434", parse = TRUE) +   
  theme_minimal() +
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
glm_basic_roc
ggsave(paste(images.dir, "glm_basic_roc.pdf", sep = ""), glm_basic_roc, device = "pdf", width = 15, height = 15)


#glm with legal drugs ####


legal_drugs = c("Alcohol",  "Caff", "Choc","Nicotine")
legal_drugs_data = drugs.clean[, legal_drugs]


for (i in legal_drugs){
  legal_drugs_data[,i] = ifelse(drugs.clean[,i]==TRUE,1,0)  
}


legal_train_data = cbind(train_data, legal_drugs_data[index_training,])
legal_test_data = cbind(test_data, legal_drugs_data[-index_training,])


control = trainControl(method="repeatedcv",repeats = 3,classProbs=TRUE, summaryFunction=twoClassSummary, returnResamp ="all")
legal_glm_model <- train(x=legal_train_data, y=train_target, method = "glm", trControl = control)

legal_glm_prediction = predict(legal_glm_model, newdata=legal_test_data)
legal_glm_matrix = confusionMatrix(legal_glm_prediction, test_target, positive="Yes" )
legal_glm_matrix
glm_legal_pROC = pROC::roc(ifelse(test_target == "Yes",1,0),predict(legal_glm_model, newdata=legal_test_data, type="prob")[,"Yes"],
                           plot=TRUE, legacy.axes=TRUE, print.auc=TRUE)


glm_legal_roc = pROC::ggroc(glm_legal_pROC) +xlab("Specificity") + ylab("Sensitivity") + 
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="darkgrey", linetype="dashed") +
  annotate("text", x = 0.5, y = 0.5, size = 20, label = "AUC == 0.8955", parse = TRUE) +   
  theme_minimal() +
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
glm_legal_roc
ggsave(paste(images.dir, "glm_legal_roc.pdf", sep = ""), glm_legal_roc, device = "pdf", width = 15, height = 15)


# GLM with best selection ####
legal_drugs = c("Alcohol",  "Caff", "Choc","Nicotine")
legal_drugs_data = drugs.clean[, legal_drugs]


for (i in legal_drugs){
  legal_drugs_data[,i] = ifelse(drugs.clean[,i]==TRUE,1,0)  
}


legal_train_data = cbind(train_data, legal_drugs_data[index_training,])
legal_test_data = cbind(test_data, legal_drugs_data[-index_training,])

personal_cols = c("Age", "Gender", "Education", "Country", "Ethnicity")
personal_data = drugs.clean[, personal_cols]
# Performing best subset selection
variables = 15
complete_train_data = cbind(legal_train_data, personal_data[index_training,])
complete_test_data = cbind(legal_test_data, personal_data[-index_training,])
best.subset.selection = regsubsets(train_target ~., complete_train_data, really.big = T, nvmax = variables)
best.subset.summary = summary(best.subset.selection)

# Plot RSS, adjusted r-square, Cp, BIC for all the models at once
par(mfrow = c(2, 2))

# RSS Plot
plot(best.subset.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")

# Adjusted RSq plot
plot(best.subset.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", 
     type = "l")
which.max(best.subset.summary$adjr2) # 12
points(12, best.subset.summary$adjr2[12], col = "red", cex = 2, pch = 20)

# Cp
plot(best.subset.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
which.min(best.subset.summary$cp) # 12
points(12, best.subset.summary$cp[12], col = "red", cex = 2, pch = 20)

# BIC
plot(best.subset.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
which.min(best.subset.summary$bic) # 9
points(9, best.subset.summary$bic[9], col = "red", cex = 2, pch = 20)

# Plotting regsubsets result
par(mfrow = c(2,2))
# Plotting built into regsubsets()
plot(best.subset.selection, scale = "r2")
plot(best.subset.selection, scale = "adjr2")
plot(best.subset.selection, scale = "Cp")
plot(best.subset.selection, scale = "bic")

par(mfrow = c(1,1))

# Choosing the model with the lowest BIC
coef(best.subset.selection, 9)

# Extending the dataset by unfolding its dummy in order to choose the
# specific categories found from best subset selection model.
complete_train_data.unfolded = fastDummies::dummy_cols(complete_train_data)
complete_test_data.unfolded = fastDummies::dummy_cols(complete_test_data)

# Filtering training data based on the variables found in the model with the lowest BIC
complete_train_data.unfolded = complete_train_data.unfolded %>% 
  select(c("Oscore", "Ascore", "Cscore", "Nicotine", "Age_45-54", "Age_55-64", "Age_65+", 
           "Education_MS", "Country_UK"))
glm_model <- train(x=complete_train_data.unfolded, y=train_target, method = "glm", trControl = control,  tuneLength = 20)
glm_prediction = predict(glm_model, newdata=complete_test_data.unfolded)
glm_matrix = confusionMatrix(glm_prediction, test_target, positive="Yes", mode = "everything")

glm_complete_pROC = pROC::roc(ifelse(test_target == "Yes",1,0),predict(glm_model, newdata=complete_test_data.unfolded, type="prob")[,"Yes"],
                           plot=TRUE, legacy.axes=TRUE, print.auc=TRUE)


glm_complete_roc = pROC::ggroc(glm_complete_pROC) +xlab("Specificity") + ylab("Sensitivity") + 
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="darkgrey", linetype="dashed") +
  annotate("text", x = 0.5, y = 0.5, size = 20, label = "AUC == 0.911", parse = TRUE) +   
  theme_minimal() +
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
glm_complete_roc
ggsave(paste(images.dir, "glm_complete_roc.pdf", sep = ""), glm_complete_roc, device = "pdf", width = 15, height = 15)

# GLM with every covariate ####
glm_model <- train(x=complete_train_data, y=train_target, method = "glm", trControl = control,  tuneLength = 20)
glm_prediction = predict(glm_model, newdata=complete_test_data)
glm_matrix = confusionMatrix(glm_prediction, test_target, positive="Yes" )
glm_matrix
glm_all_pROC = pROC::roc(ifelse(test_target == "Yes",1,0),predict(glm_model, newdata=complete_test_data.unfolded, type="prob")[,"Yes"],
                           plot=TRUE, legacy.axes=TRUE, print.auc=TRUE)


glm_all_roc = pROC::ggroc(glm_all_pROC) +xlab("Specificity") + ylab("Sensitivity") + 
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="darkgrey", linetype="dashed") +
  annotate("text", x = 0.5, y = 0.5, size = 20, label = "AUC == 0.911", parse = TRUE) +   
  theme_minimal() +
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
glm_all_roc

confusionMatrix(glm_all_roc)

t_glm <- data.frame(glm_matrix$table)

library(dplyr)
library(patchwork)

plotTable_glm <- t_glm %>%
  mutate(goodbad = ifelse(t_glm$Prediction == t_glm$Reference, "good", "bad")) %>%
  group_by(Reference) %>%
  mutate(prop = Freq/sum(Freq))

plot_glm = ggplot(data = plotTable_glm, mapping = aes(x = Reference, y = Prediction, fill = goodbad, alpha = prop )) +
  geom_tile(show.legend = FALSE) +
  geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
  scale_fill_manual(values = c(good = "green", bad = "red")) +
  theme_bw() +
  xlim(rev(levels(t_glm$Reference)))+ggtitle('GLM from best subset selection')

plot_glm

ggsave(paste(images.dir, "glm_all_covariates_roc.pdf", sep = ""), glm_all_roc, device = "pdf", width = 15, height = 15)


#legal knn ####
legal_knn_prediction = predict(legal_knn_model, newdata=legal_test_data)
control = trainControl(method="repeatedcv",repeats = 3,classProbs=TRUE, summaryFunction=twoClassSummary, returnResamp ="all")
legal_knn_model <- train(x=legal_train_data, y=train_target, method = "knn", trControl = control,  tuneLength = 40)


plot(legal_knn_model, xlab = "Number of Neighbors", ylab = "AUC")
legal_knn_prediction = predict(legal_knn_model, newdata=legal_test_data)
legal_knn_matrix = confusionMatrix(legal_knn_prediction, test_target, positive="Yes" )


knn_legal_pROC = pROC::roc(ifelse(test_target == "Yes",1,0),predict(legal_knn_model, newdata=legal_test_data, type="prob")[,"Yes"],
    plot=TRUE, legacy.axes=TRUE, print.auc=TRUE)
knn_legal_pROC

knn_legal_roc = pROC::ggroc(knn_legal_pROC) +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="darkgrey", linetype="dashed") +
  annotate("text", x = 0.5, y = 0.5, size = 20, label = "AUC == 0.8943", parse = TRUE) +   
  theme_minimal() +
  theme(legend.position = "left",
        aspect.ratio = 1, 
        title = element_text(size = 20),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.key = element_blank(), legend.key.size = unit(1,"line"),
        legend.title=element_text(size=20)) +
    labs(x = "Specificity", y = "Sensitivity")
knn_legal_roc
ggsave(paste(images.dir, "knn_legal_roc.pdf", sep = ""), knn_legal_roc, device = "pdf", width = 15, height = 15)

