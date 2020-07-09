# Dataset available at: https://archive.ics.uci.edu/ml/datasets/Drug+consumption+%28quantified%29





library(caret)
library(pROC)

# Load ####

load("data\\drug_data_clean.RData")



# Divide dataset in train and test set ####

drug.cols = c("Alcohol", "Amphet", "Amyl", "Benzos", "Caff", "Cannabis", "Choc", "Coke", 
              "Crack", "Ecstasy", "Heroin", "Ketamine","Legalh", "LSD", "Meth", "Mushrooms", 
              "Nicotine", "Semer", "VSA", "has_taken_drugs", "has_taken_synthetic_drugs")

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





#glm with legal drugs


legal_drugs = c("Alcohol", "Caff",  "Choc","Nicotine")
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



#legal knn
control = trainControl(method="repeatedcv",repeats = 3,classProbs=TRUE, summaryFunction=twoClassSummary, returnResamp ="all")
legal_knn_model <- train(x=legal_train_data, y=train_target, method = "knn", trControl = control,  tuneLength = 40)


plot(legal_knn_model)
legal_knn_prediction = predict(legal_knn_model, newdata=legal_test_data)
legal_knn_matrix = confusionMatrix(legal_knn_prediction, test_target, positive="Yes" )


roc(ifelse(test_target == "Yes",1,0),predict(legal_knn_model, newdata=legal_test_data, type="prob")[,"Yes"],
    plot=TRUE, legacy.axes=TRUE, print.auc=TRUE, )


library(dplyr)
library(patchwork)


t_knn <- data.frame(knn_matrix$table)
t_glm <- data.frame(glm_matrix$table)


plotTable_knn <- t_knn %>%
  mutate(goodbad = ifelse(t$Prediction == t$Reference, "good", "bad")) %>%
  group_by(Reference) %>%
  mutate(prop = Freq/sum(Freq))

plotTable_glm <- t_glm %>%
  mutate(goodbad = ifelse(t$Prediction == t$Reference, "good", "bad")) %>%
  group_by(Reference) %>%
  mutate(prop = Freq/sum(Freq))

plot_knn = ggplot(data = plotTable_knn, mapping = aes(x = Reference, y = Prediction, fill = goodbad, alpha = prop )) +
  geom_tile(show.legend = FALSE) +
  geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
  scale_fill_manual(values = c(good = "green", bad = "red")) +
  theme_bw() +
  xlim(rev(levels(t_glm$Reference)))+ggtitle('KNN')


plot_glm = ggplot(data = plotTable_glm, mapping = aes(x = Reference, y = Prediction, fill = goodbad, alpha = prop )) +
  geom_tile(show.legend = FALSE) +
  geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
  scale_fill_manual(values = c(good = "green", bad = "red")) +
  theme_bw() +
  xlim(rev(levels(t_glm$Reference)))+ggtitle('GLM')

plot_glm+plot_knn





coef(legal_glm_model$finalModel)
t_knn <- data.frame(legal_knn_matrix$table)
t_glm <- data.frame(legal_glm_matrix$table)


plotTable_knn <- t_knn %>%
  mutate(goodbad = ifelse(t$Prediction == t$Reference, "good", "bad")) %>%
  group_by(Reference) %>%
  mutate(prop = Freq/sum(Freq))

plotTable_glm <- t_glm %>%
  mutate(goodbad = ifelse(t$Prediction == t$Reference, "good", "bad")) %>%
  group_by(Reference) %>%
  mutate(prop = Freq/sum(Freq))

plot_knn = ggplot(data = plotTable_knn, mapping = aes(x = Reference, y = Prediction, fill = goodbad, alpha = prop )) +
  geom_tile(show.legend = FALSE) +
  geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
  scale_fill_manual(values = c(good = "green", bad = "red")) +
  theme_bw() +
  xlim(rev(levels(t_glm$Reference)))+ggtitle('KNN')


plot_glm = ggplot(data = plotTable_glm, mapping = aes(x = Reference, y = Prediction, fill = goodbad, alpha = prop )) +
  geom_tile(show.legend = FALSE) +
  geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
  scale_fill_manual(values = c(good = "green", bad = "red")) +
  theme_bw() +
  xlim(rev(levels(t_glm$Reference)))+ggtitle('GLM')

plot_glm+plot_knn




