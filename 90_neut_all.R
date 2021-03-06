#90 original
#rm(list =ls())
data<-read.csv("C:\\Users\\admin\\Desktop\\NLR\\90_neut_all.csv",head=TRUE)[,-1]
head(data)
str(data)
colnames(data)

data$�Ƿ��л�����<-factor(data$�Ƿ��л�����,levels = c(0,1))
data$���� <-factor(data$����,levels = c(0,1))
data$����<-factor(data$����,levels = c(0,1))
data$��̵<-factor(data$��̵,levels = c(0,1))
data$����<-factor(data$����,levels = c(0,1))
data$��������<-factor(data$��������,levels = c(0,1))
data$��к<-factor(data$��к,levels = c(0,1))
data$ʳ����<-factor(data$ʳ����,levels = c(0,1))
data$Ż��<-factor(data$Ż��,levels = c(0,1))
data$ͷʹ<-factor(data$ͷʹ,levels = c(0,1))
data$������ʹ<-factor(data$������ʹ,levels = c(0,1))
data$��ͥ�ۼ���Ⱦ<-factor(data$��ͥ�ۼ���Ⱦ,levels = c(0,1))
data$ICU����<-factor(data$ICU����,levels = c(0,1))
data$�ʸ�<-factor(data$�ʸ�,levels = c(0,1))
data$����������<-factor(data$����������,levels = c(0,1))
data$����Ⱦ<-factor(data$����Ⱦ,levels = c(0,1))
data$��ҩ����<-factor(data$��ҩ����,levels = c(0,1))
data$����<-factor(data$����,levels = c(0,1))
data$��Ѫѹ<-factor(data$��Ѫѹ,levels = c(0,1))
data$����<-factor(data$����,levels = c(0,1))
data$��Ѫ�ܼ���<-factor(data$��Ѫ�ܼ���,levels = c(0,1))
data$���Ը��༲��<-factor(data$���Ը��༲��,levels = c(0,1))
data$����ϵͳ����<-factor(data$����ϵͳ����,levels = c(0,1))
data$��ϵͳ����<-factor(data$��ϵͳ����,levels = c(0,1))
data$��л�Լ���<-factor(data$��л�Լ���,levels = c(0,1))
data$ѪҺϵͳ����<-factor(data$ѪҺϵͳ����,levels = c(0,1))
data$�������༲��<-factor(data$�������༲��,levels = c(0,1))
data$����<-factor(data$����,levels = c(0,1))
data$����<-factor(data$����,levels = c(0,1))


install.packages("DMwR")
library(DMwR)
set.seed(999)  
trainSplit<-SMOTE(�Ƿ���֢~.,data,perc.over=1000,perc.under=100)
head(trainSplit)
write.csv(trainSplit, file = "smotedata_nlr_finally.csv") 
getwd()


colnames(data)
names(data)<-c('Gender','age','HU_disease','heart_rate','Fever','Cough','Expectoration','Fatigue',
  'Dyspnea','Diarrhea','Poor_appetite','Emesis','Headache','Muscleache','FG_infection','Severity','ICU',
'Pharynx_Dry','antiviral_therapy','Anti_infective_treatment','Chinese_medicine',
'Oxygen_therapy','onset_time','WBC2','LYMBH2','PLT2','Prothrombin_time','CK','CK_MB',
'Procalcitonin','D_dimer','Actual_days','WBC1','LYMBH1','PLT1','hypertension',
'diabetes','Cardiovascular_diseases','Chronic_liver_disease','Respiratory_disease',
'Nervous_system_disease','Metabolic_diseases','Blood_system_diseases','Chronic_kidney_disease',
'Tumor','other','Neutrophils','NLR')

head(data)

########90
#Normality test
shapiro.test(data$age);shapiro.test(data$heart_rate);
shapiro.test(data$onset_time)
shapiro.test(data$WBC2);shapiro.test(data$LYMBH2);
shapiro.test(data$PLT2);shapiro.test(data$Prothrombin_time);
shapiro.test(data$CK);shapiro.test(data$CK_MB);
shapiro.test(data$Procalcitonin);shapiro.test(data$D_dimer);
shapiro.test(data$Actual_days);shapiro.test(data$WBC1);
shapiro.test(data$LYMBH1);shapiro.test(data$PLT1)
shapiro.test(data$Neutrophils);shapiro.test(data$NLR)



#step 1
colnames(data)
colon<-data[,-c(17,47)]
head(colon)
library(tableone) 
#2  step
CreateTableOne(data = colon)  

#3  
dput(names(colon))
myVars <- c("Gender", "age", "HU_disease", "heart_rate", "Fever", "Cough", 
            "Expectoration", "Fatigue", "Dyspnea", "Diarrhea", "Poor_appetite", 
            "Emesis", "Headache", "Muscleache", "FG_infection", 
            "Pharynx_Dry", "antiviral_therapy", "Anti_infective_treatment", 
            "Chinese_medicine", "Oxygen_therapy", "onset_time", "WBC2", "LYMBH2", 
            "PLT2", "Prothrombin_time", "CK", "CK_MB", "Procalcitonin", "D_dimer", 
            "Actual_days", "WBC1", "LYMBH1", "PLT1", "hypertension", "diabetes", 
            "Cardiovascular_diseases", "Chronic_liver_disease", "Respiratory_disease", 
            "Nervous_system_disease", "Metabolic_diseases", "Blood_system_diseases", 
            "Chronic_kidney_disease", "Tumor", "other",  "NLR"
)
#4
catVars <- c("Gender", "HU_disease",  "Fever", "Cough", 
             "Expectoration", "Fatigue", "Dyspnea", "Diarrhea", "Poor_appetite", 
             "Emesis", "Headache", "Muscleache", "FG_infection", 
             "Pharynx_Dry", "antiviral_therapy", "Anti_infective_treatment", 
             "Chinese_medicine", "Oxygen_therapy", "hypertension", "diabetes", 
             "Cardiovascular_diseases", "Chronic_liver_disease", "Respiratory_disease", 
             "Nervous_system_disease", "Metabolic_diseases", "Blood_system_diseases", 
             "Chronic_kidney_disease", "Tumor", "other"
)     
 

#5 Specify skew data
nonvar<-c("onset_time", 
          "WBC2", "LYMBH2", "PLT2", "Prothrombin_time", "CK", 
          "CK_MB","Procalcitonin", "D_dimer", 
          "WBC1", "LYMBH1", "PLT1","NLR")




#6 add all
tab4 <- CreateTableOne(vars = myVars, 
                       strata = "Severity", 
                       data = colon, 
                       factorVars = catVars, 
                       addOverall = TRUE) 
print(tab4, nonnormal = nonvar, exact = "catVars")

#10 save data
tab4Mat <- print(tab4, nonnormal = nonvar, exact = "Severity", 
                 quote = FALSE,  
                 noSpaces = TRUE,  
                 printToggle = FALSE)

write.csv(tab4Mat, file = "myTable_90finally.csv")
getwd()





######test
colnames(data)
head(data)
severe<-subset(data,Severity =='severe') 
mild<-subset(data,Severity =='mild') 
#normal data test
t.test(severe$age,mild$age, paired = FALSE, alternative = 'two.sided')
t.test(severe$heart_rate,mild$heart_rate, paired = FALSE, alternative = 'two.sided')
t.test(severe$Actual_days,mild$Actual_days, paired = FALSE, alternative = 'two.sided')

#non-normal data test  wilcox 
wilcox.test(severe$WBC2,mild$WBC2, paired = FALSE, alternative = 'two.sided')
wilcox.test(severe$LYMBH2,mild$LYMBH2, paired = FALSE, alternative = 'two.sided')
wilcox.test(severe$PLT2,mild$PLT2, paired = FALSE, alternative = 'two.sided')
wilcox.test(severe$Prothrombin_time,mild$Prothrombin_time, paired = FALSE, alternative = 'two.sided')
wilcox.test(severe$CK,mild$CK, paired = FALSE, alternative = 'two.sided')
wilcox.test(severe$CK_MB ,mild$CK_MB, paired = FALSE, alternative = 'two.sided')
wilcox.test(severe$Procalcitonin,mild$Procalcitonin, paired = FALSE, alternative = 'two.sided')
wilcox.test(severe$D_dimer,mild$D_dimer, paired = FALSE, alternative = 'two.sided')
wilcox.test(severe$WBC1,mild$WBC1, paired = FALSE, alternative = 'two.sided')
wilcox.test(severe$LYMBH1,mild$LYMBH1, paired = FALSE, alternative = 'two.sided')
wilcox.test(severe$PLT1,mild$PLT1, paired = FALSE, alternative = 'two.sided')
wilcox.test(severe$NLR,mild$NLR, paired = FALSE, alternative = 'two.sided')






#factor data-chisq.test
library(gmodels)
library(vcd)

table1<-xtabs(~Severity+Gender,data=data)
chisq.test(table1)
table2<-xtabs(~Severity+HU_disease,data=data);
chisq.test(table2)
table3<-xtabs(~Severity+Fever,data=data);chisq.test(table3)
table4<-xtabs(~Severity+Cough,data=data);chisq.test(table4)
table5<-xtabs(~Severity+Expectoration,data=data);chisq.test(table5)
table6<-xtabs(~Severity+Fatigue,data=data);chisq.test(table6)
table7<-xtabs(~Severity+Dyspnea,data=data);chisq.test(table7)
table8<-xtabs(~Severity+Diarrhea,data=data);chisq.test(table8)
table9<-xtabs(~Severity+Poor_appetite,data=data);chisq.test(table9)
table10<-xtabs(~Severity+Emesis,data=data);chisq.test(table10)
table11<-xtabs(~Severity+Headache,data=data);chisq.test(table11)
table12<-xtabs(~Severity+Muscleache,data=data);chisq.test(table12)
table13<-xtabs(~Severity+FG_infection,data=data);chisq.test(table13)
table14<-xtabs(~Severity+Pharynx_Dry,data=data);chisq.test(table14)
table15<-xtabs(~Severity+antiviral_therapy,data=data);chisq.test(table15)
table16<-xtabs(~Severity+Anti_infective_treatment,data=data);chisq.test(table16)
table17<-xtabs(~Severity+Chinese_medicine,data=data);chisq.test(table17)
table18<-xtabs(~Severity+Oxygen_therapy,data=data);chisq.test(table18)

#age figure
a<- nrow(subset(severe,age<=24));a
a1<- nrow(subset(severe,24<age & age<=49 ));a1
a2<- nrow(subset(severe,49<age& age<=64 ));a2
a3<- nrow(subset(severe,  age>=65));a3
Severe<-c(a,a1,a2,a3)
b<- nrow(subset(mild,age<=24));b
b1<- nrow(subset(mild,24<age & age<=49 ));b1
b2<- nrow(subset(mild,49<age & age<=64 ));b2
b3<- nrow(subset(mild,age>=65));b3
Mild<-c(b,b1,b2,b3)
agedata<-rbind(Severe,Mild)
name<-c("��24","25-49","50-64","��65") 
barplot(agedata,names.arg = name,legend=c("Severe","Mild"),col=c('cornsilk','cadetblue1'),bty="n")
 
library(showtext)
setEPS()
postscript("age_finally.eps")
showtext.begin()
barplot(agedata,names.arg = name,legend=c("Severe","Mild"),col=c('cornsilk','cadetblue1'),bty="n")
showtext.end()
dev.off()
getwd()