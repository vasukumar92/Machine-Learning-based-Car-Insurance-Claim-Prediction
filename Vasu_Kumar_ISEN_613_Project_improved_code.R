library(ISLR)

#Read Data
setwd("C:\\Users\\saman\\OneDrive\\Texas A&M\\R")
data=read.csv("train_set0506.csv", na.strings = "?")
validation=read.csv("test_set07.csv")

#Summary
dim(data)
summary(data)
str(data)
sum(is.na(data))

#C_Claim Column Creation
C_Claim=c(rep(0,100000))
C_Claim[data$Claim_Amount>0]=1
summary(C_Claim)
C_CLaim=as.factor(C_Claim)
plot(C_Claim)
sum(C_Claim==1)
data=cbind(data,C_Claim)
summary(data)
data$C_Claim=as.factor(data$C_Claim)
summary(data)

#Data Imputation
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#Cat1(Replacing missing values with highest frequency factor, converting Hand J to B to boost Cat1)
par(mfrow=c(2,2))
plot(data$Cat1,data$Claim_Amount) #to understand distribution
plot(validation$Cat1)
summary(data)
summary(validation)
summary(data$Cat1)
sum(is.na(data$Cat1))
data$Cat1[is.na(data$Cat1)]="B" 
for(i in 1:100000)
{
  if(data$Cat1[i]=="H" ) {data$Cat1[i]="B"}
  else {data$Cat1[i]=data$Cat1[i]}
  
}
for(i in 1:100000)
{
  if(data$Cat1[i]=="J" ) {data$Cat1[i]="B"}
  else {data$Cat1[i]=data$Cat1[i]}
  
}
plot(data$Cat1)
summary(data$Cat1)
summary(validation$Cat1)
sum(is.na(data$Cat1))

#Cat2 (Assigned Category C to the value)
plot(data$Cat2)
plot(validation$Cat2)
summary(data$Cat2)
sum(is.na(data$Cat2))
data$Cat2[is.na(data$Cat2)]="C" 
summary(data$Cat2)
plot(data$C_Claim)
summary(data)

#Cat3 (#Replacing missing values with highest frequency factor)
plot(data$Cat3)
data$Cat3 =as.character(data$Cat3)
getmode(data$Cat3)
data$Cat3[is.na(data$Cat3)] = "A"
data$Cat3=as.factor(data$Cat3)
summary(data$Cat3)
sum(is.na(data$Cat3))

#Cat4 (Assigned to class A and B to A)

plot(data$Cat4)
sum(is.na(data$Cat4))
data$Cat4[is.na(data$Cat4)] = "A"
summary(data$Cat4)
for(i in 1:100000)
{
  if(data$Cat4[i]=="B" ) {data$Cat4[i]="A"}
  else {data$Cat4[i]=data$Cat4[i]}
  
}
sum(is.na(data$Cat4))

#Cat5 (Assigned a separate A class based on avegrage similarity in Claim_Amount using non integer type conversion)
plot(data$Cat5)
sum(is.na(data$Cat5))
data$Cat5[is.na(data$Cat5)] = "A"
summary(data$Cat5)
sum(is.na(data$Cat5))

#Cat6 based on 1:0 count ratio similarity for all categories
plot(data$Cat6)
sum(is.na(data$Cat6))
data$Cat6[is.na(data$Cat6)] = "E"
summary(data$Cat6)
sum(is.na(data$Cat6))

#Cat7 (Assigned C class category based on per claim average value comparison)
plot(data$Cat7)
sum(is.na(data$Cat7))
data$Cat7[is.na(data$Cat7)]="C"
summary(data$Cat7)
sum(is.na(data$Cat7))

#Cat8 (Replacing missing values with highest frequency factor)
plot(data$Cat8)
sum(is.na(data$Cat8))
data$Cat8[is.na(data$Cat8)] = "A"
summary(data$Cat8)

#Cat9(No missing values)
#Cat10 (Replacing missing values with highest frequency factor)
plot(data$Cat10)
data$Cat10[is.na(data$Cat10)] = "A"
summary(data$Cat10)

#Cat11 (Assigning to Class A as the differece between A)
plot(data$Cat11)
summary (data$Cat11)
data$Cat11[is.na(data$Cat11)] = "A"
summary(data$Cat11)

#Ord Cat(Replacing missing values with highest frequency factor, also shifting 1 value to 4 to boost impact)
hist(data$OrdCat)
summary(data$OrdCat)
sum(is.na(data$OrdCat))
sum(data$OrdCat == 1)
data$OrdCat[is.na(data$OrdCat)] = 4
for(i in 1:100000)
{
  if(data$OrdCat[i]==1 ) {data$OrdCat[i]= 4}
  else {data$OrdCat[i]=data$OrdCat[i]}
  
}
plot(data$OrdCat)

#Cat12(Replacing missing values with highest frequency factor)
summary(data$Cat12)
data$Cat12[data$Cat12==""]="B"
sum(is.na(data$Cat12))

#Blind Make(Replacing missing values with highest frequency factor)
plot(data$Blind_Make)
summary(data$Blind_Make)
sum(is.na(data$Blind_Make))
getmode(data$Blind_Make)
data$Blind_Make[is.na(data$Blind_Make)]="K"
plot(data$Blind_Make)
summary(data$Blind_Make)
sum(is.na(data$Blind_Make))
data$Blind_Make=as.integer(data$Blind_Make)


#Blind Model(Replacing missing values with highest frequency factor)
plot(data$Blind_Model)
getmode(data$Blind_Model)
data$Blind_Model[is.na(data$Blind_Model)]="K.7"
summary(data$Blind_Model)
plot(data$Blind_Model,data$Claim_Amount)
data$Blind_Model=as.integer(data$Blind_Model)
sum(is.na(data$Blind_Model))

#Blind_Submodel(Replacing missing values with highest frequency factor)
plot(data$Blind_Submodel)
sum(is.na(data$Blind_Submodel))
getmode(data$Blind_Submodel)
data$Blind_Submodel[is.na(data$Blind_Submodel)] = "K.7.3"
data$Blind_Submodel = as.factor(data$Blind_Submodel)
data$Blind_Submodel=as.integer(data$Blind_Model)
str(data$Blind_Submodel)

#data split  in 70:30 ratio
set.seed(1)
train=sample(1:nrow(data), nrow(data)*.70)
data.train=data[train, ]
data.test=data[-train, ]
dim(data.train)

#Correlation Marix for correlation values and assessing dimension reduction variables
v1_8Nv1_4CA=data[,c(22:29,31:35)]
cor(v1_8Nv1_4CA)#Var1-Var 5,Var2-Var4,Var-Var3,Var2-Var6,Var3-Var6 significant correlation


#Note: Step Selection takes considerable time to run, the outcome is Cat3 + Var2 + Var4 + NVVar2 + NVVar3  
#Logistic Regression with step selection_AIC
#stepwise slection
logistic.fit=glm(C_Claim ~. -X-Household_ID-Claim_Amount, family=binomial, data=data.train)
step.log=step(logistic.fit)
summary(step.log)
logistic.fit=glm(C_Claim ~ Cat1+ Cat4+ OrdCat+Cat3 + Var2 + Var4 + NVVar2 + NVVar3, family=binomial, data=data.train)
summary(logistic.fit)

#Test Prediction
logistic.probs=predict(logistic.fit, data.test,type="response")  #predict of 30% test split data
summary(logistic.probs)
logistic.pred=rep(0, length(logistic.probs)) #Creating vector
logistic.pred[logistic.probs>0.00815]=1 #setting threshold value
table(logistic.pred,data.test$C_Claim)

#Training Prediction
trainpred=predict(logistic.fit,data.train,type="response")
predtrain=rep(0, nrow(data.train)) #Creating vector
predtrain[trainpred>0.00815]=1 #Seting threshold value
table(predtrain, data.train$C_Claim) 

#Result Vizualization
library(ROCR) #ROC Curve
logproc=prediction(logistic.probs,data.test$C_Claim)
roc=performance(logproc,'tpr','fpr')
plot(roc, main='ROC Curve',colorize=TRUE) #ROC Curve for analysis
abline(a=0,b=1)
d=density(logistic.probs)
plot(d,main="Kernel Density of C_Claim") #probability density curve for C_Claim
polygon(d, col="red", border="blue")


#validation
#C_Claim Column Creation
C_Claim=c(rep(0,nrow(validation)))
C_Claim[validation$Claim_Amount>0]=1
summary(C_Claim)
C_CLaim=as.factor(C_Claim)
plot(C_Claim)
sum(C_Claim==1)
validation=cbind(validation,C_Claim)
summary(validation)
validation$C_Claim=as.factor(validation$C_Claim)
str(validation)

#Vallidation_Prediction
vpred=predict(logistic.fit,validation,type="response")
vpred1=rep(0, nrow(validation)) #Creating vector
vpred1[vpred>0.00815]=1
table(vpred1, validation$C_Claim)


#For maximised accuracy not considering TP rate
#Test Prediction
logistic.pred=rep(0, length(logistic.probs)) #Creating vector
logistic.pred[logistic.probs>0.015]=1 #setting threshold value
table(logistic.pred,data.test$C_Claim)

#Training Prediction
trainpred=predict(logistic.fit,data.train,type="response")
predtrain=rep(0, nrow(data.train)) #Creating vector
predtrain[trainpred>0.015]=1 #Seting threshold value
table(predtrain, data.train$C_Claim) 

#Validation Prediction
vpred=predict(logistic.fit,validation,type="response")
vpred1=rep(0, nrow(validation)) #Creating vector
vpred1[vpred>0.015]=1
table(vpred1, validation$C_Claim)
