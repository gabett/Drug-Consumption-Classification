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


#compute and evaluate knn model 

#control = trainControl(method="repeatedcv", number=10, repeats=5)
#knn_model <- train(x=train_data, y=train_target, method = "knn", trControl = control, tuneLength = 20)

control = trainControl(method="repeatedcv",repeats = 3,classProbs=TRUE, summaryFunction=twoClassSummary, returnResamp ="all")
knn_model <- train(x=train_data, y=train_target, method = "knn", trControl = control,  tuneLength = 40)
plot(knn_model)
knn_prediction = predict(knn_model, newdata=test_data)
knn_matrix = confusionMatrix(knn_prediction, test_target, positive="Yes" )

#glm comparison
control = trainControl(method="repeatedcv",repeats = 3,classProbs=TRUE, summaryFunction=twoClassSummary, returnResamp ="all")
glm_model <- train(x=train_data, y=train_target, method = "glm", trControl = control,  tuneLength = 20)
glm_prediction = predict(glm_model, newdata=test_data)
glm_matrix = confusionMatrix(glm_prediction, test_target, positive="Yes" )

glm_basic_pROC = pROC::roc(ifelse(test_target == "Yes",1,0),predict(glm_model, newdata=test_data, type="prob")[,"Yes"],
                plot=TRUE, legacy.axes=TRUE, print.auc=TRUE)


glm_basic_roc = pROC::ggroc(glm_basic_pROC) +xlab("Specificity") + ylab("Sensitivity") + 
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="darkgrey", linetype="dashed") +
  annotate("text", x = 0.5, y = 0.5, size = 5, label = "AUC == 0.8434", parse = TRUE) +   
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


#glm with legal drugs


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

glm_legal_pROC = pROC::roc(ifelse(test_target == "Yes",1,0),predict(legal_glm_model, newdata=legal_test_data, type="prob")[,"Yes"],
                           plot=TRUE, legacy.axes=TRUE, print.auc=TRUE)


glm_legal_roc = pROC::ggroc(glm_legal_pROC) +xlab("Specificity") + ylab("Sensitivity") + 
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="darkgrey", linetype="dashed") +
  annotate("text", x = 0.5, y = 0.5, size = 5, label = "AUC == 0.8955", parse = TRUE) +   
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


#legal knn
control = trainControl(method="repeatedcv",repeats = 3,classProbs=TRUE, summaryFunction=twoClassSummary, returnResamp ="all")
legal_knn_model <- train(x=legal_train_data, y=train_target, method = "knn", trControl = control,  tuneLength = 40)


plot(legal_knn_model, xlab = "Number of Neighbors", ylab = "AUC")
legal_knn_prediction = predict(legal_knn_model, newdata=legal_test_data)
legal_knn_matrix = confusionMatrix(legal_knn_prediction, test_target, positive="Yes" )


knn = pROC::roc(ifelse(test_target == "Yes",1,0),predict(legal_knn_model, newdata=legal_test_data, type="prob")[,"Yes"],
    plot=TRUE, legacy.axes=TRUE, print.auc=TRUE)
knn

knn_roc = pROC::ggroc(knn) +xlab("Specificity") + ylab("Sensitivity") + 
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="darkgrey", linetype="dashed") +
  annotate("text", x = 0.5, y = 0.5, size = 5, label = "AUC == 0.8953", parse = TRUE) +   
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
knn_roc
ggsave(paste(images.dir, "knn_roc.pdf", sep = ""), knn_roc, device = "pdf", width = 15, height = 15)

