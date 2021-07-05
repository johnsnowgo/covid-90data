#smote_model
#rm(list =ls())
data<-read.csv("C:\\Users\\admin\\Desktop\\NLR\\smotedata_nlr_finally.csv",head=TRUE)[,-1]
head(data)
str(data)
colnames(data)
nrow(data)

names(data)<-c('Gender','age','HU_disease','heart_rate','Fever','Cough','Expectoration','Fatigue',
  'Dyspnea','Diarrhea','Poor_appetite','Emesis','Headache','Muscleache','FG_infection','Severity','ICU',
'Pharynx_Dry','antiviral_therapy','Anti_infective_treatment','Chinese_medicine',
'Oxygen_therapy','onset_time','WBC2','LYMBH2','PLT2','Prothrombin_time','CK','CK_MB',
'Procalcitonin','D_dimer','Actual_days','WBC1','LYMBH1','PLT1','hypertension',
'diabetes','Cardiovascular_diseases','Chronic_liver_disease','Respiratory_disease',
'Nervous_system_disease','Metabolic_diseases','Blood_system_diseases','Chronic_kidney_disease',
'Tumor','other','Neutrophils','NLR')


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
data$ICU<-factor(data$ICU,levels = c(0,1))
data$Pharynx_Dry<-factor(data$Pharynx_Dry,levels = c(0,1))
data$antiviral_therapy<-factor(data$antiviral_therapy,levels = c(0,1))
data$Anti_infective_treatment<-factor(data$Anti_infective_treatment,levels = c(0,1))
data$Chinese_medicine<-factor(data$Chinese_medicine,levels = c(0,1))
data$Oxygen_therapy<-factor(data$Oxygen_therapy,levels = c(0,1))
data$hypertension<-factor(data$hypertension,levels = c(0,1))
data$diabetes<-factor(data$diabetes,levels = c(0,1))
data$Cardiovascular_diseases<-factor(data$Cardiovascular_diseases,levels = c(0,1))
data$Chronic_liver_disease<-factor(data$Chronic_liver_disease,levels = c(0,1))
data$Respiratory_disease<-factor(data$Respiratory_disease,levels = c(0,1))
data$Nervous_system_disease<-factor(data$Nervous_system_disease,levels = c(0,1))
data$Metabolic_diseases<-factor(data$Metabolic_diseases,levels = c(0,1))
data$Blood_system_diseases<-factor(data$Blood_system_diseases,levels = c(0,1))
data$Chronic_kidney_disease<-factor(data$Chronic_kidney_disease,levels = c(0,1))
data$Tumor<-factor(data$Tumor,levels = c(0,1))
data$other<-factor(data$other,levels = c(0,1))
str(data)
nrow(data)
head(data)
###########single-logic
colnames(data)
model<-glm(Severity~age
               ,data=data, family = "binomial") 
summary(model) 
model<-glm(Severity~Fatigue
               ,data=data, family = "binomial") 
summary(model)
model<-glm(Severity~NLR
               ,data=data, family = "binomial")
summary(model) 
################lasso
datav8<-data
yv8<-as.matrix(datav8$Severity)
x1v8<-datav8[,c("age","onset_time","LYMBH2","CK",
                "Procalcitonin","D_dimer","LYMBH1")] 
xfactorsv8<- model.matrix(yv8~datav8$HU_disease+datav8$Fever+datav8$Cough
                          +datav8$Expectoration+datav8$Fatigue 
                          +datav8$Pharynx_Dry)[,-1]

x2v8<-cbind(x1v8,xfactorsv8)
colnames(x2v8)
head(x2v8)

xv8<-as.matrix(data.frame(x2v8))
library(glmnet)
set.seed(1000)
cv.fitv8<-cv.glmnet(xv8,yv8,alpha=1,family = "binomial",type.measure="deviance") #做交叉验证找lambda值
summary(cv.fitv8)
#par(mfrow=c(1,2))
plot(cv.fitv8)
cv.fitv8$lambda.min
cv.fitv8$lambda.1se 
fit_v8new<-glmnet(xv8,yv8,alpha=1,family="binomial")
plot(fit_v8new)
Coefficients <- coef(fit_v8new, s = cv.fitv8$lambda.min) #指定lambda值
Active.Index <- which(Coefficients != 0)
Active.Coefficients <- Coefficients[Active.Index]
Active.Index      
Active.Coefficients         
row.names(Coefficients)[Active.Index]   
summary(fit_v8new,s = cv.fitv8$lambda.min)

############save picture
library(showtext)
setEPS()
postscript("lasso_finally.eps")
showtext_begin()
par(mfrow=c(1,2))
plot(cv.fitv8)
plot(fit_v8new)
showtext.end()
dev.off()
getwd()
###########

glmv<-glm(Severity~age+LYMBH2+ CK+Procalcitonin+D_dimer+LYMBH1+HU_disease+
                 Cough+Expectoration+Fatigue+ Pharynx_Dry  
                  ,data=datav8, family = "binomial")  
summary(glmv) 

glmv82<-glm(Severity~age+CK+Procalcitonin+LYMBH1+  
                 Cough+Fatigue+ Pharynx_Dry  #+Expectoration
                  ,data=datav8, family = "binomial")  
summary(glmv82) 


#90original forecast

yuanshidata<-read.csv("C:\\Users\\admin\\Desktop\\NLR\\90_neut_all.csv",head=TRUE)[,-1]
head(yuanshidata)
colnames(yuanshidata)
names(yuanshidata)<-c('Gender','age','HU_disease','heart_rate','Fever','Cough','Expectoration','Fatigue',
  'Dyspnea','Diarrhea','Poor_appetite','Emesis','Headache','Muscleache','FG_infection','Severity','ICU',
'Pharynx_Dry','antiviral_therapy','Anti_infective_treatment','Chinese_medicine',
'Oxygen_therapy','onset_time','WBC2','LYMBH2','PLT2','Prothrombin_time','CK','CK_MB',
'Procalcitonin','D_dimer','Actual_days','WBC1','LYMBH1','PLT1','hypertension',
'diabetes','Cardiovascular_diseases','Chronic_liver_disease','Respiratory_disease',
'Nervous_system_disease','Metabolic_diseases','Blood_system_diseases','Chronic_kidney_disease',
'Tumor','other','Neutrophils','NLR')

yuanshi<-yuanshidata[,c("Severity","age","CK","Procalcitonin","LYMBH1","Cough","Fatigue","Pharynx_Dry")]
yuanshi$Cough<-factor(yuanshi$Cough,levels = c(0,1))
yuanshi$Fatigue<-factor(yuanshi$Fatigue,levels = c(0,1))
yuanshi$Pharynx_Dry<-factor(yuanshi$Pharynx_Dry,levels = c(0,1))

datav8$Severity;
yuanshi$Severity;
yuanshi$newSeverity[yuanshi$Severity=="severe"]<-1
yuanshi$newSeverity[yuanshi$Severity=="mild"]<-0
yuanshi$newSeverity<-as.numeric(as.character(yuanshi$newSeverity))

datav8$newSeverity[datav8$Severity=="severe"]<-1
datav8$newSeverity[datav8$Severity=="mild"]<-0
datav8$newSeverity<-as.numeric(as.character(datav8$newSeverity))
colnames(yuanshi)


fittedresults = predict(glmv82
                        ,newdata = subset(yuanshi,select=c(2:8)),type='response');
fittedresults = ifelse(fittedresults>0.5,1,0);
Error = mean(fittedresults != yuanshi$newSeverity);  
print(paste('Accuracy',1-Error))

  
 
library(pROC)
yuanshiroc<- roc(yuanshi$Severity,fittedresults) 
par(mfrow=c(1,1))
plot(yuanshiroc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)

library(showtext)

setEPS()
postscript("logisticsROC.eps")
showtext_begin()
plot(yuanshiroc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)
showtext.end()
dev.off()
getwd()
########## logic end
#########randforest
######divid data 7:3
set.seed(999)
ind <- sample(2,nrow(data),replace = TRUE,prob = c(0.7,0.3))  
train_data <- data[ind==1,]  
test_data <- data[ind==2,]  
head(train_data)
write.csv(train_data, file = "train_data_finally.csv")
write.csv(test_data, file = "test_data_finally.csv")
getwd()


colnames(train_data)
train_data2<-train_data[,-c(17,19:22,36:47)]
str(train_data2)
colnames(train_data2)
#######found best mytree 
library("randomForest")
n<-length(names(train_data2))
set.seed(9)
min=100
num=0
for (i in 1:(n-1)){
  mtry_fit<- randomForest(Severity~., data=train_data2, mtry=i)
  err<-mean(mtry_fit$err.rate)
  print(err)
  if(err<min) {    
    min =err     
    num=i }
}
print(min)
print(num)

#########
library(randomForest)
set.seed(100)
ntree_fit<-randomForest(Severity~.,data=train_data2,mtry=6,ntree=2000)
par(mfrow=c(1,1))
plot(ntree_fit)
##########
library(showtext)
setEPS()
postscript("randforest_ntree2000.eps")
showtext_begin()

set.seed(100)
ntree_fit<-randomForest(Severity~.,data=train_data2,mtry=6,ntree=2000)
par(mfrow=c(1,1))
plot(ntree_fit)
showtext_end()
dev.off()
getwd()
#############


set.seed(100)
rf2<-randomForest(Severity~.,data=train_data2,mtry=6,ntree=1300,importance=T,localImp=T,proximity=T)
rf2
 

rf2$importance 
varImpPlot(rf2)

write.csv(rf2$importance , file = "rfimportance_finally2.csv")
getwd()

library(showtext)
setEPS()
postscript("rfimportance_finally2.eps")
showtext.begin()
varImpPlot(rf2)
showtext.end()
dev.off()
getwd()

#预测
colnames(test_data)
test_data2<-test_data[,-c(17,19:22,36:47)]
str(test_data2)



pred1<-predict(rf2,newdata=test_data2) #forecast
length(pred1)
nrow(test_data2)

Freq1<-table(pred1,test_data2$Severity)
sum(diag(Freq1))/sum(Freq1)  
library(caret)
forestmatrix <- confusionMatrix(data = pred1,test_data2$Severity)
forestmatrix
forestmatrix$byClass

roc2<-roc(as.ordered(test_data2$Severity) ,as.ordered(pred1))
plot(roc2,print.auc=T, auc.polygon=T, grid=c(0.1, 0.2), grid.col=c("green","red"), 
     max.auc.polygon=T, auc.polygon.col="skyblue",print.thres=T)
######
library(showtext)
setEPS()
postscript("randforest_roc2.eps")
showtext.begin()
plot(roc2,print.auc=T, auc.polygon=T, grid=c(0.1, 0.2), grid.col=c("green","red"), 
     max.auc.polygon=T, auc.polygon.col="skyblue",print.thres=T)
showtext.end()
dev.off()
getwd()



