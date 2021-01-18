library("dplyr")
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
train <- read.csv("C:/Users/User/OneDrive/Desktop/New folder/train.csv/train.csv")
test <- read.csv("C:/Users/User/OneDrive/Desktop/New folder/test.csv/test.csv")
train$target=as.factor(train$target)
train_s<-train[,-1]%>%mutate_if(is.numeric,scale)
test_s<-test[,-1]%>%mutate_if(is.numeric,scale)


best_param = list()
best_seednumber = 1234
best_logloss = Inf
best_logloss_index = 0

for (iter in 1:100){
  param <- list(objective = "multi:softprob",
                eval_metric = "mlogloss",
                num_class = 9,
                max_depth = sample(6:15, 1),
                eta = runif(1, .01, .3),
                gamma = runif(1, 0.0, 0.2), 
                subsample = runif(1, .6, .9),
                colsample_bytree = runif(1, .5, .8), 
                min_child_weight = sample(1:40, 1),
                max_delta_step = sample(1:10, 1)
  )
  cv.nround = 1000
  cv.nfold = 5
  seed.number = sample.int(10000, 1)[[1]]
  set.seed(seed.number)
  mdcv <- xgb.cv(data=data.matrix(train_s[,-94]),label=lab, params = param, nthread=6,nfold=cv.nfold,early_stopping_rounds=5,nrounds=cv.nround,verbose = T, maximize=FALSE)
  min_logloss = min(mdcv$evaluation_log[,test_mlogloss_mean])
  min_logloss_index=which.min(mdcv$evaluation_log[,test_mlogloss_mean])
  if(min_logloss < best_logloss) {
    best_logloss = min_logloss
    best_logloss_index = min_logloss_index
    best_seednumber = seed.number
    best_param = param
  }
}

nround = best_logloss_index
set.seed(best_seednumber)


###bag 10 xgboost models using the best parameters

labels=as.numeric(train_s[,94])
lab=labels-1
xgb_1 <- xgboost(
  data = data.matrix(train_s[,-c(94)]), 
  label = lab, 
  booster="gbtree",
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=9,
  eta=0.1776858,
  max_depth=14,
  gamma=0.1417733,
  subsample=0.82314,
  colsample_bytree=0.6828399,
  min_child_weight=33,
  max_delta_step=7,
  nround=270
)

#2

labels=as.numeric(train_s[,94])
lab=labels-1
xgb_2 <- xgboost(
  data = data.matrix(train_s[,-c(94)]), 
  label = lab, 
  booster="gbtree",
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=9,
  eta=0.1776858,
  max_depth=14,
  gamma=0.1417733,
  subsample=0.82314,
  colsample_bytree=0.6828399,
  min_child_weight=33,
  max_delta_step=7,
  nround=270
)


##3
labels=as.numeric(train_s[,94])
lab=labels-1
xgb_3 <- xgboost(
  data = data.matrix(train_s[,-c(94)]), 
  label = lab, 
  booster="gbtree",
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=9,
  eta=0.1776858,
  max_depth=14,
  gamma=0.1417733,
  subsample=0.82314,
  colsample_bytree=0.6828399,
  min_child_weight=33,
  max_delta_step=7,
  nround=270
)



##4

labels=as.numeric(train_s[,94])
lab=labels-1
xgb_4 <- xgboost(
  data = data.matrix(train_s[,-c(94)]), 
  label = lab, 
  booster="gbtree",
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=9,
  eta=0.1776858,
  max_depth=14,
  gamma=0.1417733,
  subsample=0.82314,
  colsample_bytree=0.6828399,
  min_child_weight=33,
  max_delta_step=7,
  nround=270
)


##5

labels=as.numeric(train_s[,94])
lab=labels-1
xgb_5 <- xgboost(
  data = data.matrix(train_s[,-c(94)]), 
  label = lab, 
  booster="gbtree",
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=9,
  eta=0.1776858,
  max_depth=14,
  gamma=0.1417733,
  subsample=0.82314,
  colsample_bytree=0.6828399,
  min_child_weight=33,
  max_delta_step=7,
  nround=270
)

####6
labels=as.numeric(train_s[,94])
lab=labels-1
xgb_6 <- xgboost(
  data = data.matrix(train_s[,-c(94)]), 
  label = lab, 
  booster="gbtree",
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=9,
  eta=0.1776858,
  max_depth=14,
  gamma=0.1417733,
  subsample=0.82314,
  colsample_bytree=0.6828399,
  min_child_weight=33,
  max_delta_step=7,
  nround=270
)
##7
labels=as.numeric(train_s[,94])
lab=labels-1
xgb_7 <- xgboost(
  data = data.matrix(train_s[,-c(94)]), 
  label = lab, 
  booster="gbtree",
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=9,
  eta=0.1776858,
  max_depth=14,
  gamma=0.1417733,
  subsample=0.82314,
  colsample_bytree=0.6828399,
  min_child_weight=33,
  max_delta_step=7,
  nround=270
)


##8
labels=as.numeric(train_s[,94])
lab=labels-1
xgb_8 <- xgboost(
  data = data.matrix(train_s[,-c(94)]), 
  label = lab, 
  booster="gbtree",
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=9,
  eta=0.1776858,
  max_depth=14,
  gamma=0.1417733,
  subsample=0.82314,
  colsample_bytree=0.6828399,
  min_child_weight=33,
  max_delta_step=7,
  nround=270
)

###9
labels=as.numeric(train_s[,94])
lab=labels-1
xgb_9 <- xgboost(
  data = data.matrix(train_s[,-c(94)]), 
  label = lab, 
  booster="gbtree",
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=9,
  eta=0.1776858,
  max_depth=14,
  gamma=0.1417733,
  subsample=0.82314,
  colsample_bytree=0.6828399,
  min_child_weight=33,
  max_delta_step=7,
  nround=270
)
##10##

labels=as.numeric(train_s[,94])
lab=labels-1
xgb_10 <- xgboost(
  data = data.matrix(train_s[,-c(94)]), 
  label = lab, 
  booster="gbtree",
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=9,
  eta=0.1776858,
  max_depth=14,
  gamma=0.1417733,
  subsample=0.82314,
  colsample_bytree=0.6828399,
  min_child_weight=33,
  max_delta_step=7,
  nround=270
)


####
pred_1=predict(xgb_1, data.matrix(test_s))
pred_2=predict(xgb_2, data.matrix(test_s))
pred_3=predict(xgb_3, data.matrix(test_s))
pred_4=predict(xgb_4, data.matrix(test_s))
pred_5=predict(xgb_5, data.matrix(test_s))
pred_6=predict(xgb_6, data.matrix(test_s))
pred_7=predict(xgb_7, data.matrix(test_s))
pred_8=predict(xgb_8, data.matrix(test_s))
pred_9=predict(xgb_9, data.matrix(test_s))
pred_10=predict(xgb_10, data.matrix(test_s))

a1=data.frame(matrix(unlist(pred_1), nrow=144368 ,ncol=9, byrow=T))
a2=data.frame(matrix(unlist(pred_2), nrow=144368 ,ncol=9, byrow=T))
a3=data.frame(matrix(unlist(pred_3), nrow=144368 ,ncol=9, byrow=T))
a4=data.frame(matrix(unlist(pred_4), nrow=144368 ,ncol=9, byrow=T))
a5=data.frame(matrix(unlist(pred_5), nrow=144368 ,ncol=9, byrow=T))
a6=data.frame(matrix(unlist(pred_6), nrow=144368 ,ncol=9, byrow=T))
a7=data.frame(matrix(unlist(pred_7), nrow=144368 ,ncol=9, byrow=T))
a8=data.frame(matrix(unlist(pred_8), nrow=144368 ,ncol=9, byrow=T))
a9=data.frame(matrix(unlist(pred_9), nrow=144368 ,ncol=9, byrow=T))
a10=data.frame(matrix(unlist(pred_10), nrow=144368 ,ncol=9, byrow=T))



mean_xg=((a1+a2+a3+a4+a5+a6+a7+a8+a9+a10)/10)

View(mean_xg)

names(mean_xg)=c("Class_1","Class_2","Class_3","Class_4","Class_5","Class_6","Class_7","Class_8","Class_9")
write.csv(data.frame(mean_xg),"xg_bag10.csv",row.names=TRUE)

####train_features

pred_1=predict(xgb_1, data.matrix(train_s[,-94]))
pred_2=predict(xgb_2, data.matrix(train_s[,-94]))
pred_3=predict(xgb_3, data.matrix(train_s[,-94]))
pred_4=predict(xgb_4, data.matrix(train_s[,-94]))
pred_5=predict(xgb_5, data.matrix(train_s[,-94]))
pred_6=predict(xgb_6, data.matrix(train_s[,-94]))
pred_7=predict(xgb_7, data.matrix(train_s[,-94]))
pred_8=predict(xgb_8, data.matrix(train_s[,-94]))
pred_9=predict(xgb_9, data.matrix(train_s[,-94]))
pred_10=predict(xgb_10, data.matrix(train_s[,-94]))

a1=data.frame(matrix(unlist(pred_1), nrow=61878 ,ncol=9, byrow=T))
a2=data.frame(matrix(unlist(pred_2), nrow=61878 ,ncol=9, byrow=T))
a3=data.frame(matrix(unlist(pred_3), nrow=61878 ,ncol=9, byrow=T))
a4=data.frame(matrix(unlist(pred_4), nrow=61878 ,ncol=9, byrow=T))
a5=data.frame(matrix(unlist(pred_5), nrow=61878 ,ncol=9, byrow=T))
a6=data.frame(matrix(unlist(pred_6), nrow=61878 ,ncol=9, byrow=T))
a7=data.frame(matrix(unlist(pred_7), nrow=61878 ,ncol=9, byrow=T))
a8=data.frame(matrix(unlist(pred_8), nrow=61878 ,ncol=9, byrow=T))
a9=data.frame(matrix(unlist(pred_9), nrow=61878 ,ncol=9, byrow=T))
a10=data.frame(matrix(unlist(pred_10), nrow=61878 ,ncol=9, byrow=T))



mean_xg=((a1+a2+a3+a4+a5+a6+a7+a8+a9+a10)/10)

View(mean_xg)

names(mean_xg)=c("Class_1","Class_2","Class_3","Class_4","Class_5","Class_6","Class_7","Class_8","Class_9")
write.csv(data.frame(mean_xg),"xg_bag10_train.csv",row.names=TRUE)

####

NN_test_bag25 <- read.csv("C:/Users/User/OneDrive/Desktop/New folder/Predictions/NN_test_bag25.csv")
NN_test_bag25=NN_test_bag25[,-1]
xg_bag10 <- read.csv("C:/Users/User/OneDrive/Desktop/New folder/Predictions/xg_bag10.csv")
xg_bag10=xg_bag10[,-1]


nnxgmean=((NN_test_bag25+xg_bag10)/2)
names(nnxgmean)=c("Class_1","Class_2","Class_3","Class_4","Class_5","Class_6","Class_7","Class_8","Class_9")
write.csv(data.frame(nnxgmean),"nnxg.csv",row.names=TRUE)



#####try kaggle xg###
labels=as.numeric(train_s[,94])
lab=labels-1
xgb_1 <- xgboost(
  data = data.matrix(train_s[,-c(94)]), 
  label = lab, 
  booster="gbtree",
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=9,
  eta=0.1776858,
  max_depth=9,
  gamma=1,
  subsample=0.82314,
  colsample_bytree=0.8,
  colsample_bylevel=0.8,
  min_child_weight=0.8,
  max_delta_step=7,
  nround=300
)

pred_kag=predict(xgb_1, data.matrix(test_s))
a1=data.frame(matrix(unlist(pred_kag), nrow=144368 ,ncol=9, byrow=T))
names(a1)=c("Class_1","Class_2","Class_3","Class_4","Class_5","Class_6","Class_7","Class_8","Class_9")
write.csv(data.frame(a1),"kaggle_xg.csv",row.names=TRUE)


#combine nn and xg
a1
NN_test_bag25 <- read.csv("C:/Users/User/OneDrive/Desktop/New folder/Predictions/NN_test_bag25.csv")
NN_test_bag25=NN_test_bag25[,-1]
xg_bag10 <- read.csv("C:/Users/User/OneDrive/Desktop/New folder/Predictions/xg_bag10.csv")
xg_bag10=xg_bag10[,-1]

mean_nnxg_new=((a1+NN_test_bag25+xg_bag10)/3)
names(mean_nnxg_new)=c("Class_1","Class_2","Class_3","Class_4","Class_5","Class_6","Class_7","Class_8","Class_9")
write.csv(data.frame(mean_nnxg_new),"mean_nnxg_kaggle1.csv",row.names=TRUE)


##mean of svm and everything###
SVM_test_features <- read.csv("C:/Users/User/OneDrive/Desktop/New folder/Predictions/SVM_test_features.csv")
SVM_test_features =SVM_test_features[,-1] 
RF_test_predictions_feature <- read.csv("C:/Users/User/OneDrive/Desktop/New folder/Predictions/RF_test_predictions_feature.csv")
RF_test_predictions_feature=RF_test_predictions_feature[,-1]

mean_all=((a1*(.4))+(NN_test_bag25*(.4))+(xg_bag10*(.1))+(RF_test_predictions_feature*.05)+(SVM_test_features*.05))
names(mean_all)=c("Class_1","Class_2","Class_3","Class_4","Class_5","Class_6","Class_7","Class_8","Class_9")

write.csv(data.frame(mean_all),"mean_all_weight.csv",row.names=TRUE)

