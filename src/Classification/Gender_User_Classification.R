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
library(glmnet)
library(pRoc)
library(caret)

# Global Variables ####
dir = "C:\\Users\\ettag\\Documents\\GitHub\\Stastistical-Learning-Project\\data"
setwd(dir = dir)

# Load ####
data.dir = paste(dir, "\\drug_data_clean.RData", sep = "")
load(data.dir)

images.dir = "C:\\Users\\ettag\\Documents\\GitHub\\Stastistical-Learning-Project\\images\\Predictions"
setwd(dir = images.dir)

drug.cols = c("has_taken_illegal_drugs", "has_taken_drugs", "has_taken_synthetic_drugs")

drugs.clean = drugs.clean %>% select(-drug.cols)

# Divide dataset in train and test set ####
train.rows = round(dim(drugs.clean)[1] * 0.60)
test.rows = dim(drugs.clean)[1] - train.rows

set.seed(42)
train = sample(1:nrow(drugs.clean), train.rows)

train.ds = drugs.clean[train, ]
test.ds = drugs.clean[-train,]

# Build model with LASSO and 5 Fold CV
lambda_seq <- 10^seq(2, -2, by = -.1)
grid = grid=10^seq(10,-2,length=100)

# glmnet wants a matrix, so we prepare data accordingly.
x = model.matrix(Gender~., train.ds)
y = ifelse(train.ds$Gender == "Female",1,0)
cv_output <- cv.glmnet(x,y,
                       alpha = 1,
                       nfolds = 5, family = "binomial")

plot(cv_output)

# Looking for the best lambda.
best_lam = cv_output$lambda.min 
lambda_1se = cv_output$lambda.1se

# Which coefficients we obtained?
coef(cv_output,s=lambda_1se)

# Test
x_test = model.matrix(Gender ~.,test.ds)
lasso_prob = predict(cv_output, newx = x_test, s=lambda_1se, type= "response")

lasso_predict = rep("Male",nrow(test.ds))
lasso_predict[lasso_prob>.5] <- "Female"

# Confusion matrix
classification.table = table(pred = lasso_predict, true = test.ds$Gender)
classification.table

# Error rate
(112 + 120) / (112 + 270 + 252 + 120)

# confusionMatrix(classification.table)

# Accuracy
mean(lasso_predict==test.ds$Gender)

# Roc Curve
numeric_predict = ifelse(lasso_predict == "Female",1,0)
numeric_real = ifelse(test.ds$Gender == "Female",1,0)

roc.out <- pROC::roc(numeric_predict, numeric_real, levels=c(0, 1))
plot.roc = plot(roc.out,  
                print.auc=TRUE, 
                main = "Gender Drug / Non Drug Users Classification",
                legacy.axes=TRUE, 
                xlab="False positive rate", ylab="True positive rate")
plot.roc

# Clean ####
rm(list = ls())
