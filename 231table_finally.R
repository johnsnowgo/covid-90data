#smote_table
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

########
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
########
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


#######1 table
colnames(data)
colon<-data[,-c(17,47)]
library(tableone) 


#2 Specify the variables in the baseline table that need to be compared
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
            "Chronic_kidney_disease", "Tumor", "other", "NLR"
)
#######3 factor
catVars <- c("Gender", "HU_disease",  "Fever", "Cough", 
             "Expectoration", "Fatigue", "Dyspnea", "Diarrhea", "Poor_appetite", 
             "Emesis", "Headache", "Muscleache", "FG_infection", 
             "Pharynx_Dry", "antiviral_therapy", "Anti_infective_treatment", 
             "Chinese_medicine", "Oxygen_therapy", "hypertension", "diabetes", 
             "Cardiovascular_diseases", "Chronic_liver_disease", "Respiratory_disease", 
             "Nervous_system_disease", "Metabolic_diseases", "Blood_system_diseases", 
             "Chronic_kidney_disease", "Tumor", "other"
)     


#4 Specify skew data
nonvar<-c("age","heart_rate", "onset_time", "Actual_days",
          "WBC2", "LYMBH2", "PLT2", "Prothrombin_time", "CK", 
          "CK_MB","Procalcitonin", "D_dimer", 
          "WBC1", "LYMBH1", "PLT1","NLR")
#5 add all
tab4 <- CreateTableOne(vars = myVars, 
                       strata = "Severity", 
                       data = colon, 
                       factorVars = catVars, 
                       addOverall = TRUE)  
print(tab4, nonnormal = nonvar, exact = "catVars")

#6 save data
tab4Mat <- print(tab4, nonnormal = nonvar, exact = "Severity", 
                 quote = FALSE,  
                 noSpaces = TRUE,  
                 printToggle = FALSE)
write.csv(tab4Mat, file = "myTablesmote_finally.csv")  #The final descriptive statistics table
getwd()


#########
severe<-subset(data,Severity =='severe') 
mild<-subset(data,Severity =='mild') 
#####count all
nrow(subset(data,WBC1<4));nrow(subset(data,WBC1>=4 & WBC1<=10));nrow(subset(data,WBC1>10))
nrow(subset(data,LYMBH1<1));nrow(subset(data,LYMBH1>=1))
nrow(subset(data,PLT1<100));nrow(subset(data,PLT1>=100))
nrow(subset(data,WBC2<4));nrow(subset(data,WBC2>=4 & WBC2<=10));nrow(subset(data,WBC2>10))
nrow(subset(data,LYMBH2<1));nrow(subset(data,LYMBH2>=1))
nrow(subset(data,PLT2<100));nrow(subset(data,PLT2>=100))
nrow(subset(data,CK<50));nrow(subset(data,CK>=50 &CK<350))
nrow(subset(data,Procalcitonin<0.1));nrow(subset(data,Procalcitonin>=0.1 &Procalcitonin<0.25));nrow(subset(data,Procalcitonin>=0.25))

#####count mild
nrow(subset(mild,WBC1<4));nrow(subset(mild,WBC1>=4 & WBC1<=10));nrow(subset(mild,WBC1>10))
nrow(subset(mild,LYMBH1<1));nrow(subset(mild,LYMBH1>=1))
nrow(subset(mild,PLT1<100));nrow(subset(mild,PLT1>=100))
nrow(subset(mild,WBC2<4));nrow(subset(mild,WBC2>=4 & WBC2<=10));nrow(subset(mild,WBC2>10))
nrow(subset(mild,LYMBH2<1));nrow(subset(mild,LYMBH2>=1))
nrow(subset(mild,PLT2<100));nrow(subset(mild,PLT2>=100))
nrow(subset(mild,CK<50));nrow(subset(mild,CK>=50 &CK<350))
nrow(subset(mild,Procalcitonin<0.1));nrow(subset(mild,Procalcitonin>=0.1 &Procalcitonin<0.25));nrow(subset(mild,Procalcitonin>=0.25))
#####count severe
nrow(subset(severe,WBC1<4));nrow(subset(severe,WBC1>=4 & WBC1<=10));nrow(subset(severe,WBC1>10))
nrow(subset(severe,LYMBH1<1));nrow(subset(severe,LYMBH1>=1))
nrow(subset(severe,PLT1<100));nrow(subset(severe,PLT1>=100))
nrow(subset(severe,WBC2<4));nrow(subset(severe,WBC2>=4 & WBC2<=10));nrow(subset(severe,WBC2>10))
nrow(subset(severe,LYMBH2<1));nrow(subset(severe,LYMBH2>=1))
nrow(subset(severe,PLT2<100));nrow(subset(severe,PLT2>=100))
nrow(subset(severe,CK<50));nrow(subset(severe,CK>=50 &CK<350))
nrow(subset(severe,Procalcitonin<0.1));nrow(subset(severe,Procalcitonin>=0.1 &Procalcitonin<0.25));nrow(subset(severe,Procalcitonin>=0.25))





#non-normal  wilcox test 
colnames(data)
wilcox.test(severe$age,mild$age, paired = FALSE, alternative = 'two.sided')
wilcox.test(severe$heart_rate,mild$heart_rate, paired = FALSE, alternative = 'two.sided')
wilcox.test(severe$onset_time,mild$onset_time, paired = FALSE, alternative = 'two.sided')
wilcox.test(severe$WBC2,mild$WBC2, paired = FALSE, alternative = 'two.sided')
wilcox.test(severe$LYMBH2,mild$LYMBH2, paired = FALSE, alternative = 'two.sided')
wilcox.test(severe$PLT2,mild$PLT2, paired = FALSE, alternative = 'two.sided')
wilcox.test(severe$Prothrombin_time,mild$Prothrombin_time, paired = FALSE, alternative = 'two.sided')
wilcox.test(severe$CK,mild$CK, paired = FALSE, alternative = 'two.sided')
wilcox.test(severe$CK_MB,mild$CK_MB, paired = FALSE, alternative = 'two.sided')
wilcox.test(severe$Procalcitonin,mild$Procalcitonin, paired = FALSE, alternative = 'two.sided')
wilcox.test(severe$D_dimer,mild$D_dimer, paired = FALSE, alternative = 'two.sided')
wilcox.test(severe$Actual_days,mild$Actual_days, paired = FALSE, alternative = 'two.sided')
wilcox.test(severe$WBC1,mild$WBC1, paired = FALSE, alternative = 'two.sided')
wilcox.test(severe$LYMBH1,mild$LYMBH1, paired = FALSE, alternative = 'two.sided')
wilcox.test(severe$PLT1,mild$PLT1, paired = FALSE, alternative = 'two.sided')
wilcox.test(severe$NLR,mild$NLR, paired = FALSE, alternative = 'two.sided')


############chisq  factor test
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