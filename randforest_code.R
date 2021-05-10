#Random forests
rm(list=ls())
data<-read.csv("C:\\Users\\admin\\Desktop\\data_and_code\\smote_data_train.csv",head=TRUE)[,-1]
nrow(data)
head(data)
str(data)
#删除多余变量
colnames(data)
data<-data[,c(-17,-19,-20,-21,-22)]
data$Gender<-factor(data$Gender,levels = c(1,2),labels = c("Male","Female"))
data$HU_disease<-factor(data$HU_disease,levels = c(0,1))
data$Fever <-factor(data$Fever,levels = c(0,1))
data$Cough<-factor(data$Cough,levels = c(0,1))
data$Expectoration<-factor(data$Expectoration,levels = c(0,1))
data$Fatigue<-factor(data$Fatigue,levels = c(0,1))
data$Dyspnea<-factor(data$Dyspnea,levels = c(0,1))
data$Diarrhea<-factor(data$Diarrhea,levels = c(0,1))
data$Poor_appetite<-factor(data$Poor_appetite,levels = c(0,1))
data$Emesis<-factor(data$Emesis,levels = c(0,1))
data$Headache<-factor(data$Headache,levels = c(0,1))
data$Muscleache<-factor(data$Muscleache,levels = c(0,1))
data$FG_infection<-factor(data$FG_infection,levels = c(0,1))
data$Pharynx_Dry<-factor(data$Pharynx_Dry,levels = c(0,1))
nrow(data)
data_train<-data

library(randomForest)
set.seed(100)
ntree_fit<-randomForest(Severity~.,data=data_train,mtry=2,ntree=1000)
par(mfrow=c(1,1))
plot(ntree_fit)

library(showtext)
setEPS()
postscript("ntree_fit900.eps")
showtext.begin()
plot(ntree_fit)
showtext.end()
dev.off()
getwd()


set.seed(100)
rf<-randomForest(Severity~.,data=data_train,mtry=2,ntree=900,importance=T,localImp=T,proximity=T)
plot(rf)

rf$importance 
#write.csv(rf$importance , file = "rfimportance_v9.csv")
#save(rf,file="randforestresult.RData")
#load("randforestresult.RData")

#变量重要性排名

varImpPlot(rf)
#保存图
library(showtext)
setEPS()
postscript("importance2.eps")
showtext.begin()

varImpPlot(rf)
showtext.end()
dev.off()
getwd()

#预测
data_test<-read.csv("C:\\Users\\admin\\Desktop\\data_and_code\\smote_data_test.csv",head=TRUE)[,-1]
colnames(data_test)
data_test<-data_test[,c(-17,-19,-20,-21,-22)]
data_test$Gender<-factor(data_test$Gender,levels = c(1,2),labels = c("Male","Female"))
data_test$HU_disease<-factor(data_test$HU_disease,levels = c(0,1))
data_test$Fever <-factor(data_test$Fever,levels = c(0,1))
data_test$Cough<-factor(data_test$Cough,levels = c(0,1))
data_test$Expectoration<-factor(data_test$Expectoration,levels = c(0,1))
data_test$Fatigue<-factor(data_test$Fatigue,levels = c(0,1))
data_test$Dyspnea<-factor(data_test$Dyspnea,levels = c(0,1))
data_test$Diarrhea<-factor(data_test$Diarrhea,levels = c(0,1))
data_test$Poor_appetite<-factor(data_test$Poor_appetite,levels = c(0,1))
data_test$Emesis<-factor(data_test$Emesis,levels = c(0,1))
data_test$Headache<-factor(data_test$Headache,levels = c(0,1))
data_test$Muscleache<-factor(data_test$Muscleache,levels = c(0,1))
data_test$FG_infection<-factor(data_test$FG_infection,levels = c(0,1))
data_test$Pharynx_Dry<-factor(data_test$Pharynx_Dry,levels = c(0,1))
nrow(data_test)

colnames(data_train)
colnames(data_test)

pred1<-predict(rf,newdata=data_test) #预测结果
length(pred1)
nrow(data_test)

Freq1<-table(pred1,data_test$Severity)
sum(diag(Freq1))/sum(Freq1)  #准确率

library(caret)
forestmatrix <- confusionMatrix(data = pred1,data_test$Severity)
#结果提取
forestmatrix$byClass


library(ROCR)
#代码 3 将预测和实际结果均转换为0-1数值型
treepre<-vector()
treepre[pred1=='mild']<-0
treepre[pred1=='severe']<-1
factresult<-vector()
factresult[data_test$Severity=='mild']<-0
factresult[data_test$Severity=='severe']<-1
tree.pred <- prediction(treepre,factresult)
tree.perf <- performance(tree.pred, "tpr", "fpr") 
plot(tree.perf)

#实际需要和逻辑回归一样，pre要随机森林的预测概率
#pred4<-predict(rf,newdata=data_test,type = "response")
#加了type = "response"  表示输出结果预测响应变量为1的概率，但随机森林仍然为结果

roc<-roc(as.ordered(data_test$Severity) ,as.ordered(pred1))
plot(roc,print.auc=T, auc.polygon=T, grid=c(0.1, 0.2), grid.col=c("green","red"), 
     max.auc.polygon=T, auc.polygon.col="skyblue",print.thres=T)


library(showtext)
setEPS()
postscript("forestforcast.eps")
showtext_begin()

plot(roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)
showtext_end()
dev.off()
getwd()


#英文绘图改变量名
ENG<-data_train
fix(ENG)
View(ENG)
set.seed(100)
rf_ENG<-randomForest(Severity~.,data=ENG,mtry=2,ntree=900,importance=T,localImp=T,proximity=T)
plot(rf_ENG)

rf_ENG$importance 
#变量重要性排名可视化
library(showtext)
setEPS()
postscript("importance_english.eps")
showtext.begin()

varImpPlot(rf_ENG,main="rf")
showtext.end()
dev.off()
getwd()

#训练集数据的混淆矩阵
pred0122<-predict(rf,newdata=data_train) #预测结果
Freq0122<-table(pred0122,data_train$Severity)
sum(diag(Freq0122))/sum(Freq0122)  #准确率
