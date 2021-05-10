rm(list=ls())
data<-read.csv("C:\\Users\\admin\\Desktop\\data_and_code\\smotedata.csv",head=TRUE)
nrow(data)
head(data)
str(data)
colnames(data)
data$Gender<-factor(data$Gender,levels = c(1,2))
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
data$Severity<-factor(data$Severity,levels = c(0,1),labels = c("mild","severe"))
data$ICU<-factor(data$ICU,levels = c(0,1))
data$Pharynx_Dry<-factor(data$Pharynx_Dry,levels = c(0,1))
data$antiviral_therapy<-factor(data$antiviral_therapy,levels = c(0,1))
data$Anti_infective_treatment<-factor(data$Anti_infective_treatment,levels = c(0,1))
data$Chinese_medicine<-factor(data$Chinese_medicine,levels = c(0,1))
data$Oxygen_therapy<-factor(data$Oxygen_therapy,levels = c(0,1))


datav8<-data
colnames(datav8)
str(datav8)
nrow(datav8)
# Single variable logistics
onelog<-glm(Severity~age#change variable
                ,data=datav8, family = "binomial") 
summary(onelog)  
#年龄、发病到入院时间、凝血酶原时间、肌酸激酶、降钙素原、D二聚体、刚入院淋巴细胞计数。
nrow(datav8)
yv8<-as.matrix(datav8$Severity)
colnames(datav8)
x1v8<-datav8[,c(2,23,27,28,30,31,34)]  #连续型变量   
str(x1v8)
cor(x1v8)
xfactorsv8 <- model.matrix(yv8~datav8$HU_disease+datav8$Fever+datav8$Cough
                           +datav8$Expectoration+datav8$Fatigue+datav8$Poor_appetite+datav8$Pharynx_Dry)[,-1]
x2v8<-cbind(x1v8,xfactorsv8)
head(x2v8)
str(x2v8)
xv8<-as.matrix(data.frame(x2v8))
nrow(xv8)
length(yv8)
#sum(is.na(data))
library(glmnet)
set.seed(1000)
cv.fitv8<-cv.glmnet(xv8,yv8,alpha=1,family = "binomial",type.measure="deviance") #做交叉验证找lambda值
summary(cv.fitv8)
#par(mfrow=c(1,2))
plot(cv.fitv8,main='Figure 3a.')
cv.fitv8$lambda.min
cv.fitv8$lambda.1se  #取值
fit_v8new<-glmnet(xv8,yv8,alpha=1,family="binomial")
plot(fit_v8new)
Coefficients <- coef(fit_v8new, s = cv.fitv8$lambda.min) #指定lambda值
Active.Index <- which(Coefficients != 0)
Active.Coefficients <- Coefficients[Active.Index]
Active.Index      #非零位置
Active.Coefficients         #选出的系数值
row.names(Coefficients)[Active.Index]  #选出的变量名字
summary(fit_v8new,s = cv.fitv8$lambda.min)

library(showtext)
setEPS()
postscript("lasso.eps")
showtext_begin()
par(mfrow=c(1,2))
plot(cv.fitv8)
plot(fit_v8new)
showtext.end()
dev.off()
getwd()

glmv82<-glm(Severity~age+onset_time+CK+Procalcitonin#+D_dimer
                        +LYMBH1 +Expectoration #+Cough+HU_disease
                +Fatigue+Poor_appetite+Pharynx_Dry
                ,data=datav8, family = "binomial") 
summary(glmv82)   
library(car)
vif(glmv82)

#原始数据  
yuanshi<-read.csv("C:\\Users\\admin\\Desktop\\data_and_code\\90original.csv",head=TRUE)[,-1]
colnames(yuanshi)
head(yuanshi)
yuanshi$HU_disease<-factor(yuanshi$HU_disease,levels = c(0,1))
yuanshi$Fever <-factor(yuanshi$Fever,levels = c(0,1))
yuanshi$Cough<-factor(yuanshi$Cough,levels = c(0,1))
yuanshi$Expectoration<-factor(yuanshi$Expectoration,levels = c(0,1))
yuanshi$Fatigue<-factor(yuanshi$Fatigue,levels = c(0,1))
yuanshi$Poor_appetite<-factor(yuanshi$Poor_appetite,levels = c(0,1))
yuanshi$Pharynx_Dry<-factor(yuanshi$Pharynx_Dry,levels = c(0,1))
   
datav8$Severity;
yuanshi$Severity;
yuanshi$new重症[yuanshi$Severity=="severe"]<-1
yuanshi$new重症[yuanshi$Severity=="mild"]<-0
#yuanshi$new重症<-factor(yuanshi$new重,levels = c(0,1))
yuanshi$new重症<-as.numeric(as.character(yuanshi$new重症))

datav8$new重症[datav8$Severity=="severe"]<-1
datav8$new重症[datav8$Severity=="mild"]<-0
#yuanshi$new重症<-factor(yuanshi$new重,levels = c(0,1))
datav8$new重症<-as.numeric(as.character(datav8$new重症))


 
#yuanshi[2,c(2,23,28,30,34,8,7,11,18)]
fittedresults = predict(glmv82
                        ,newdata = subset(yuanshi,select=c(2,23,28,30,34,8,7,11,18)),type='response');
fittedresults = ifelse(fittedresults>0.5,1,0);
Error = mean(fittedresults != yuanshi$new重症); #注意重症是文字还是0-1
print(paste('Accuracy',1-Error))


preyuanshiresults = ifelse(fittedresults>0.5,1,0);
Error = mean(preyuanshiresults != yuanshi$new重症); #注意重症是文字还是0-1
print(paste('Accuracy',1-Error))

library(pROC)
yuanshiroc<- roc(yuanshi$Severity,fittedresults)#括号内为实际分类结果和预测分类概率。
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
########## 



