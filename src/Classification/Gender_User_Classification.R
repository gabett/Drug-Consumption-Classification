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
train.rows = round(dim(drugs.clean)[1] * 0.70)
test.rows = dim(drugs.clean)[1] - train.rows

set.seed(42)
train = sample(1:nrow(drugs.clean), train.rows)

train.ds = drugs.clean[train, ]
test.ds = drugs.clean[-train,]

# Performing best subset selection
variables = 12
best.subset.selection = regsubsets(Gender ~., train.ds, really.big = T, nvmax = variables)
best.subset.summary = summary(best.subset.selection)

# Plot RSS, adjusted r-square, Cp, BIC for all the models at once
par(mfrow = c(2, 2))

# RSS Plot
plot(best.subset.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")

# Adjusted RSq plot
plot(best.subset.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", 
     type = "l")
which.max(best.subset.summary$adjr2) # 11
points(11, best.subset.summary$adjr2[11], col = "red", cex = 2, pch = 20)

# Cp
plot(best.subset.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
which.min(best.subset.summary$cp) # 11
points(11, best.subset.summary$cp[11], col = "red", cex = 2, pch = 20)

# BIC
plot(best.subset.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
which.min(best.subset.summary$bic) # 10
points(10, best.subset.summary$bic[10], col = "red", cex = 2, pch = 20)

# Plotting regsubsets result
par(mfrow = c(2,2))
# Plotting built into regsubsets()
plot(best.subset.selection, scale = "r2")
plot(best.subset.selection, scale = "adjr2")
plot(best.subset.selection, scale = "Cp")
plot(best.subset.selection, scale = "bic")

par(mfrow = c(1,1))

# Choosing the model with the lowest BIC
coef(best.subset.selection, 10)

# Extending the dataset by unfolding its dummy in order to choose the
# specific categories found from best subset selection model.
train.ds = fastDummies::dummy_cols(train.ds)

# Checking if specificity and sensivity holds through K fold CV.

# Randomly shuffle the data
train.ds.shuffled = train.ds[sample(nrow(train.ds)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(train.ds.shuffled)), breaks=10, labels=FALSE)

#Perform 10 fold cross validation
specificity.cv = rep(0, 10)
sensitivity.cv = rep(0, 10)
i = 1
for(i in 1:10){
  
  testIndexes = which(folds==i,arr.ind=TRUE)
  
  testData = train.ds.shuffled[testIndexes, ]
  
  trainData = train.ds.shuffled[-testIndexes, ]
  
  model.train = glm(Gender ~ `Age_25-34` + Education_BS + Education_MS + Country_UK + Nscore + Ascore + Cscore + 
                      Amyl_TRUE + Legalh_TRUE + LSD_TRUE, data = train.ds, family = "binomial")
  predictions.cv = predict.glm(model.train, newdata = testData, type = "response")
  
  # Confusion matrix
  predicted.categories = ifelse(predictions.cv > 0.5, "Male", "Female")
  #classification.table = table(pred = predicted.categories, true = testData$Gender)
  conf.matrix = confusionMatrix(table(predicted.categories, testData$Gender)
  
  specificity.cv[i] = specificity(conf.matrix)
  sensitivity.cv[i] = sensitivity.cv(conf.matrix)
}





# Creating 10 folds per CV
train.cv = trainControl(method = "repeatedcv", number = 10, repeats = 3)

# Train the model
train.model <- train(Gender ~ `Age_25-34` + Education_BS + Education_MS + Country_UK + Nscore + Ascore + Cscore + 
                 Amyl_TRUE + Legalh_TRUE + LSD_TRUE, data = train.ds, method = "glm",
               trControl = train.cv)

# Extending for test dataset too.
test.ds = fastDummies::dummy_cols(test.ds)
predictions = predict.glm(train.model, newdata = test.ds, type = "response")

# Confusion matrix
predicted.categories = ifelse(predictions > 0.5, "Male", "Female")
classification.table = table(pred = predicted.categories, true = test.ds$Gender)
classification.table

# Error rate
(112 + 120) / (112 + 270 + 252 + 120)
confusionMatrix(classification.table)

# Roc Curve
numeric_real = ifelse(test.ds$Gender == "Male",1,0)

roc.out <- pROC::roc(numeric_real~predictions)
plot(roc.out,  
                print.auc=TRUE, 
                plot = TRUE,
                main = "Gender Drug / Non Drug Users Classification",
                xlab="False positive rate", ylab="True positive rate")

