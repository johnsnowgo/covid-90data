rm(list=ls())
data<-read.csv("C:\\Users\\admin\\Desktop\\data_and_code\\90original.csv",head=TRUE)
head(data)
str(data)
colnames(data)
#data$Gender<-factor(data$Gender,levels = c(1,2),labels = c("Male","Female"))
data$HU_disease<-factor(data$HU_disease,levels = c(0,1))
data$Fever<-factor(data$Fever,levels = c(0,1))
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
#data$Severity<-factor(data$Severity,levels = c(0,1),labels = c("mild","severe"))
data$ICU<-factor(data$ICU,levels = c(0,1))
data$Pharynx_Dry<-factor(data$Pharynx_Dry,levels = c(0,1))
data$antiviral_therapy<-factor(data$antiviral_therapy,levels = c(0,1))
data$Anti_infective_treatment<-factor(data$Anti_infective_treatment,levels = c(0,1))
data$Chinese_medicine<-factor(data$Chinese_medicine,levels = c(0,1))
data$Oxygen_therapy<-factor(data$Oxygen_therapy,levels = c(0,1))

w<-data
#步骤1 Draw the baseline table
library(tableone) # 加载包
library(survival)  # 加载包，需要使用survival包的colon数据，此处已经修改为w赋值
colon<-data
#data(colon) # 加载数据集
View(colon) # 预览数据集


#2 单组汇总数据
CreateTableOne(data = colon)  # 汇总整个数据集特征

#3 Specify the variables in the baseline table that need to be compared
dput(names(colon))
myVars <- c("Gender","age",  "onset_time", "Actual_days","FG_infection",
            "HU_disease", "Fever", 
            "Cough", "Expectoration", "Fatigue","Dyspnea",
            "Diarrhea",  "Poor_appetite","Emesis", "Headache",
            "Muscleache","Pharynx_Dry",
              "heart_rate",  
            "WBC1", "LYMBH1", "PLT1",
            "WBC2", "LYMBH2", "PLT2", "Prothrombin_time", "CK", 
            "CK_MB",  "Procalcitonin", "D_dimer", "antiviral_therapy", "Anti_infective_treatment", "Chinese_medicine", "Oxygen_therapy" 
)
#指定分类变量
catVars <- c("Gender",  "Fever", 
             "Cough", "Expectoration", "Fatigue","Dyspnea",
             "Diarrhea",  "Poor_appetite","Emesis", "Headache",
             "Muscleache","ICU","Pharynx_Dry","antiviral_therapy", "Anti_infective_treatment", "Chinese_medicine", "Oxygen_therapy"
)
#is.na(colon$Dyspnea)

#2 单组汇总
tab2 <- CreateTableOne(vars = myVars,  
                       data = colon, 
                       factorVars = catVars);tab2

# 通过vars参数指定哪些变量是基线表中需要汇总的变量
# 通过factorVars参数指定哪些变量是分类变量
# data参数指定变量的数据来源

print(tab2, showAllLevels = TRUE)
summary(tab2)
shapiro.test(w$heart_rate)  #若P值大于0.05，服从正太分布
shapiro.test(w$age)  
#6 指定非正态连续变量
nonvar<-c("onset_time", 
          "WBC2", "LYMBH2", "PLT2", "Prothrombin_time", "CK", 
          "CK_MB","Procalcitonin", "D_dimer", 
          "WBC1", "LYMBH1", "PLT1")

print(tab2,nonnormal = nonvar)

#7 多组汇总数据，指定按照某一变量分组
tab3 <- CreateTableOne(vars = myVars, 
                       strata = "Severity", 
                       data = colon, 
                       factorVars = catVars); tab3
# strata参数表示分层，指定需要分层的变量，这里我们指定是否重症变量
# 通过vars参数指定哪些变量是基线表中需要汇总的变量。
# 通过factorVars参数指定哪些变量是分类变量
# data参数指定变量的数据来源

#8 对非正态连续变量nonvar采用fisher检验
print(tab3, # 前面的tab3对象
      nonnormal = nonvar, # 指定哪些连续变量是非正态分布变量
      exact = catVars)  # 指定哪些变量需要使用fisher精确检验

#9 添加all列
tab4 <- CreateTableOne(vars = myVars, 
                       strata = "Severity", 
                       data = colon, 
                       factorVars = catVars, 
                       addOverall = TRUE) # 增加overall列
print(tab4, nonnormal = nonvar, exact = "catVars")

#10 数据导出
tab4Mat <- print(tab4, nonnormal = nonvar, exact = "Severity", 
                 quote = FALSE, # 不显示引号
                 noSpaces = TRUE, # 删除用于在R控制台中对齐文本的空格
                 printToggle = FALSE)

## 保存为 CSV 格式文件，并命名为 myTable。
#write.csv(tab4Mat, file = "myTable.csv")
getwd()


#制作均值图
tab4 <- CreateTableOne(vars = myVars, 
                       strata = "Severity", 
                       data = colon, 
                       factorVars = catVars, 
                       addOverall = TRUE) # 增加overall列
print(tab4,  exact = "catVars") #nonnormal = nonvar  该参数指定输出是否为均值
#write.csv(tab4Mat, file = "myTable均值.csv")


###3  smote
#one<-read.csv("c:\\missdata1.csv",head=True)
#library(DMwR)
#trainSplit<-SMOTE(Severity~.,one,perc.over=1000,perc.under=100)
#write.csv(trainSplit, file = "smotedata.csv")

#missForest Dealing with missing values
library(missForest)
library(randomForest)
iris.imp <- missForest(data, verbose = TRUE)
iris.imp$ximp #修补后的数据
iris.imp$OOBerror #估计错误 连续变量：NRMSE归一化均方误差 分类变量：PFC错误分类条目 

dataupdate<-cbind(w1,iris.imp$ximp)
#write.csv(dataupdate,file="C:\\Users\\admin\\Desktop\\covdata\\20200611\\missdata1.csv",quote=F,row.names = F)


