train <- read.csv("C:/Users/User/OneDrive/Desktop/New folder/train.csv/train.csv")
test <- read.csv("C:/Users/User/OneDrive/Desktop/New folder/test.csv/test.csv")
library(data.table)
library(mltools)
library(standardize)
library("dplyr")
library(factoextra)
library(plot3D)
library(rgl)
library(scatterplot3d)
train$target=as.factor(train$target)
summary(train$target)
#very unbalanced
#standardize
train_s<-train[,-1]%>%mutate_if(is.numeric,scale)
#pca
train.pca <- prcomp(train_s[,-94],scale=TRUE) # pca of features
PC= data.frame(train.pca$x)

summary(train.pca)#77 dimensions explan 95% var
fviz_eig(train.pca,geom = "line",choice = "eigenvalue",ncp = 15)
fviz_eig(train.pca,geom = "line",choice = "variance",ncp = 15)
##visualize
PC = cbind(PC,train_s[,94])
colors <- c("red", "blue", "yellow","purple","green","orange","pink","black","azure","coral","gold")
colors <- colors[as.numeric(PC$`train_s[, 94]`)]
scat2<-scatterplot3d(PC[,1:3], pch = 16, color=colors, main = "3")
legend(scat2$xyz.convert(-30,1,15),legend = levels(PC_LM$S_NYC1...16.),col =c("#1B9E77", "#D95F02", "yellow"),pch=16 )
classes=PC[,1:3]
plot3d(classes, col=colors,pch = 50, main="9 classes")
#remove clas 2,6 and 8

red_class=PC[which(PC$`train_s[, 94]`!=("Class_2")),]
red_class=red_class[which(red_class$`train_s[, 94]`!=("Class_6")),]
red_class=red_class[which(red_class$`train_s[, 94]`!=("Class_8")),]

colors <- c("red", "blue", "yellow","green","black","purple","orange")
colors <- colors[as.numeric(red_class$`train_s[, 94]`)]
red_class_PC=red_class[,1:3]
plot3d(red_class_PC, col=colors,pch = 50, main="6 classes")


##plot with best clusteronly###4,7,2,6,8

bestcl=PC[which(PC$`train_s[, 94]`!=("Class_1")),]
bestcl=bestcl[which(bestcl$`train_s[, 94]`!=("Class_3")),]
bestcl=bestcl[which(bestcl$`train_s[, 94]`!=("Class_5")),]
bestcl=bestcl[which(bestcl$`train_s[, 94]`!=("Class_9")),]

colors <- c("pink","red","white", "blue","beige", "yellow","green","black")
colors <- colors[as.numeric(bestcl$`train_s[, 94]`)]
bestcl_PC=bestcl[,1:3]
plot3d(bestcl_PC, col=colors,pch = 50, main="5 best classes")




###k

k.set= c(9,10,11,12,13,14)
impk = numeric(length(k.set))
gini_fin = numeric(length(k.set))
gin = numeric(length(k.set))
full_data= train_s
##cluster #8 is purest
for(k in 1:length(k.set)){
  full_data = train_s
  km_explore<- kmeans(full_data[,-94],k.set[k],iter.max = 100)
  full_data$cluster = NA
  full_data$cluster=as.factor(km_explore$cluster)
  clus_Size = km_explore$size
  gini_cl = numeric(k.set[k])
  impk = 0
  for(i in 1:k.set[k]){#go through each cluster
    clu<-subset(full_data,full_data$cluster == i)
    clu_1 = table(clu$target)[1]/clus_Size[i]
    clu_2=table(clu$target)[2]/clus_Size[i]
    clu_3=table(clu$target)[3]/clus_Size[i]
    clu_4 = table(clu$target)[4]/clus_Size[i]
    clu_5=table(clu$target)[5]/clus_Size[i]
    clu_6=table(clu$target)[6]/clus_Size[i]
    clu_7 = table(clu$target)[7]/clus_Size[i]
    clu_8=table(clu$target)[8]/clus_Size[i]
    clu_9=table(clu$target)[9]/clus_Size[i]
    gini_cl = clu_1*(1-clu_1)+clu_2*(1-clu_2)+clu_3*(1-clu_3)+clu_4*(1-clu_4)+clu_5*(1-clu_5)+clu_6*(1-clu_6)+clu_7*(1-clu_7)+clu_8*(1-clu_8)+clu_9*(1-clu_9)
    impk = impk+gini_cl
    
  }
  gin[k] = impk
  #print(gini_cl)
}
gini_final = numeric(length(k.set))
for(i in 1:length(k.set)){
  gini_final[i] = gin[i]/(0.88889*k.set[i])
}
gini_plot=data.frame(gini_final)
ggplot(gini_plot , aes(x=k.set, y=gini_plot$gini_final)) +
  geom_line(color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=3)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  xlab("k")+
  ylab("gini k")+
  ggtitle("k vs gini index")



#####class 2 and class 6 clster

###########

k.set= c(1,5,9,10,11,12,13,15,16,17,18,19,20,25,30)
Qm <- numeric(length(k.set))
Perf.m <- numeric(length(k.set))
wss = numeric(length(k.set))
for (k in 1:length(k.set)){
  km.class <- kmeans(train_s[,-94], k.set[k],iter.max=100)
  #wss[k] = km.airbnb$tot.withinss
  Q <- km.class$tot.withinss/km.class$totss
  Perf <- 1-(km.class$tot.withinss/km.class$totss)
  Qm[k] = Q
  Perf.m[k] = Perf
}
end = Sys.time()
end-start
ggplot(data.frame(cbind(k.set,Perf.m)),aes(x=k.set, y=Perf.m)) +
  geom_line(color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=3)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  #geom_vline(xintercept = 15)+
  xlab("k")+
  ylab("Perf(k)")+
  ggtitle("k vs Perf(k)")+
  theme_bw()


#######11 clusters######
train_11 = train_s
set.seed(1)
kmbest=kmeans(train_s[,-94], 15, nstart=2,iter.max=100)
train_11$cluster<-as.factor(kmbest$cluster)
#cluster 1
clu1<-subset(train_11,train_11$cluster == "1")#class 8
table(clu1$target) ##0     106      37      11    2120       1       2       1       1
#cluster 2

clu2<-subset(train_11,train_11$cluster == "2")#cl9
table(clu2$target)#117      11       7       8       0     166       4      58    1630 
#cluster 3
clu3<-subset(train_11,train_11$cluster == "3")#class8
table(clu3$target)#0     328     748     209       0      11     179       0       8
#cl4
clu4<-subset(train_11,train_11$cluster == "4")#no winner
table(clu4$target)# 0     328     748     209       0      11     179       0       8
#cluster 5
clu5<-subset(train_11,train_11$cluster == "5")#class 7
table(clu5$target) # 0       0       0       0       0      24     188       0       0

clu6<-subset(train_11,train_11$cluster == "6")#no winner
table(clu6$target) #1536    5904    2578    1088     584    3991    1722    5753    2919 

#cluster 7: 
clu7<-subset(train_11,train_11$cluster == "7")#class 6
table(clu7$target) # 0       0       0       1       2    5034       2      72      15 

#cluster 8
clu8<-subset(train_11,train_11$cluster == "8")#class 6
table(clu8$target) #35       0       0      27       0    4334      47      87      51 
#cluster 9
clu9<-subset(train_11,train_11$cluster == "9")#no winner
table(clu9$target)#  22    6508    2570    1018      16      23     167       6      47 


#cluster 10
clu10<-subset(train_11,train_11$cluster == "10")#no winner
table(clu10$target)#4    1753    1519      10      17      62      55      91       1

#cluster 11
clu11<-subset(train_11,train_11$cluster == "11")#no winner
table(clu11$target) # 0    1415     498     306       0       0       2       0       0 

#clu12
clu12<-subset(train_11,train_11$cluster == "12")#class 8
table(clu12$target)#0       0       0       0       0      41       1     265       0

#clu13
clu13<-subset(train_11,train_11$cluster == "13")#no winner
table(clu13$target)#144      96      47      13       0     410     419     462     279 

#clu14
clu14<-subset(train_11,train_11$cluster == "14")#class 8
table(clu14$target)# 13       0       0       0       0       0       2     271       3 

#clu15
clu15<-subset(train_11,train_11$cluster == "15")#class 1
table(clu15$target)


#####TRAIN-TEST SPLIT######
full_data=train_s
set.seed(1)
Class_1 =full_data[which(full_data$target =="Class_1"),]
set.seed(1)
Class_2 = full_data[which(full_data$target =="Class_2"),]
set.seed(1)
Class_3 =full_data[which(full_data$target =="Class_3"),]

set.seed(1)
Class_4 =full_data[which(full_data$target =="Class_4"),]
set.seed(1)
Class_5 = full_data[which(full_data$target =="Class_5"),]
set.seed(1)
Class_6 =full_data[which(full_data$target =="Class_6"),]

set.seed(1)
Class_7 =full_data[which(full_data$target =="Class_7"),]
set.seed(1)
Class_8 = full_data[which(full_data$target =="Class_8"),]
set.seed(1)
Class_9 =full_data[which(full_data$target =="Class_9"),]



##TRAIN/TEST SET: 1
n<-nrow(Class_1)
train<-sample(1:n, 0.8*n)
trainset_1 <- Class_1[train,]
testset_1 <- Class_1[-train,]
##TRAIN/TEST SET: 2
n<-nrow(Class_2)
train<-sample(1:n, 0.8*n)
trainset_2 <- Class_2[train,]
testset_2 <- Class_2[-train,]
##TRAIN/TEST SET: 3
n<-nrow(Class_3)
train<-sample(1:n, 0.8*n)
trainset_3 <- Class_3[train,]
testset_3 <- Class_3[-train,]
##TRAIN/TEST SET: 4
n<-nrow(Class_4)
train<-sample(1:n, 0.8*n)
trainset_4 <- Class_4[train,]
testset_4 <- Class_4[-train,]

##TRAIN/TEST SET: 5
n<-nrow(Class_5)
train<-sample(1:n, 0.8*n)
trainset_5 <- Class_5[train,]
testset_5 <- Class_5[-train,]
##TRAIN/TEST SET: 6
n<-nrow(Class_6)
train<-sample(1:n, 0.8*n)
trainset_6 <- Class_6[train,]
testset_6 <- Class_6[-train,]
##TRAIN/TEST SET: 7
n<-nrow(Class_7)
train<-sample(1:n, 0.8*n)
trainset_7 <- Class_7[train,]
testset_7 <- Class_7[-train,]
##TRAIN/TEST SET: 8
n<-nrow(Class_8)
train<-sample(1:n, 0.8*n)
trainset_8 <- Class_8[train,]
testset_8 <- Class_8[-train,]
##TRAIN/TEST SET: 9
n<-nrow(Class_9)
train<-sample(1:n, 0.8*n)
trainset_9 <- Class_9[train,]
testset_9 <- Class_9[-train,]




#combined train set
TRAIN_SET = rbind(trainset_1,trainset_2,trainset_3,trainset_4,trainset_5,trainset_6,trainset_7,trainset_8,trainset_9)#High:10176  Low: 10243  Medium:10655 
TEST_SET = rbind(testset_1,testset_2,testset_3,testset_4,testset_5,testset_6,testset_7,testset_8,testset_9)#High: 2544   Low:2561   Medium:2664

library(randomForest)
library(caret)

TRAIN_SET$target<- factor(TRAIN_SET$target) # changing label to factors
TEST_SET$target<- factor(TEST_SET$target)
set.seed(1)
start = Sys.time() ####1.706 mins
RF_200 <- randomForest(target ~., data=train,ntree = 200, ntry=9
                       ,importance=F)




#train 1543   12897    6403    2152    2191   11308    2271    6771 
predSET_100test<-predict(RF_100,newdata = TEST_SET)
results_100test = confusionMatrix(predSET_100test, TEST_SET$target)
# 0.8069 

predSET_400tr<-predict(RF_400,newdata = TRAIN_SET1)
results_400tr = confusionMatrix(predSET_400tr, TRAIN_SET1$target)
predSET_400test<-predict(RF_400,newdata = TEST_SET1)
results_400test = confusionMatrix(predSET_400test, TEST_SET1$target)

Train_Acc = c(results_100tr$overall[1],results_200tr$overall[1],results_300tr$overall[1],results_400tr$overall[1])
Test_Acc = c(results_100test$overall[1],results_200test$overall[1],results_300test$overall[1],results_400test$overall[1])
nfor = c(100,200,300,400)

ggplot(data.frame(cbind(ntree,Train_Acc,Test_Acc)), aes(x=ntree))+
  geom_point(aes(y = Train_Acc, color = "darkred"),shape=18,size=3)+
  geom_line(aes(y = Train_Acc,color = "darkred"),linetype="longdash") +
  geom_point(aes(y = Test_Acc,color = "steelblue"),shape=18,size=3)+
  geom_line(aes(y = Test_Acc,color="steelblue"), linetype="dotdash")+
  ylim(.63,.85)+
  scale_color_identity(name = "",
                       labels = c("Train-set", "Test-set"),
                       guide = "legend") +
  xlab("Number of forests")+
  ylab("Accuracy")+
  ggtitle("Train vs Test set Accuracy")



###knn###



####Naive Bayes###
install.packages("e1071")
library("e1071")

nb=naiveBayes(target ~., train_s, laplace = 1)

pred_nb = predict(nb, test, "raw")
a=round(pred_nb,2)
library("class")
install.packages("ISLR")
write.csv(data.frame(a),"pred_nb.csv")

###KNN###")
library(ISLR)
install.packages("ggplot2")
library(caret)

K.set = c(15)
knn.train.accuracy <- numeric(length(K.set))

#train set knn
knn_mod = knn3(target ~ ., data = train_s, k = 5)
head(predict(knn_mod, test, type = "prob"), n = 10)



##SVM##

set.seed(1)
model = svm(train_S[,-94],train_S[,94],probability=T)
SVM_test = predict(model,test_S,decision.values= T, probability= T)
SVM_TEST=(attr(SVM_test,"probabilities"))
write.csv(SVM_TEST,"SVM_test_features.csv",row.names=T)
