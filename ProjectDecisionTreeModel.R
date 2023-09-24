#Library packages needed to build decision tree model 
#install.packages('rpart')
library(rpart)
#install.packages('rpart.plot')
library(rpart.plot)
#install.packages('maptree')
library(maptree)
library(caret)

#1. Missing Values
data<-read.csv("marketing_data.csv")
summary(data$Income)
data$Income[is.na(data$Income)]<-mean(data$Income,na.rm = TRUE)
summary(data$Income)
str(data)

#2. Factor the Response variable (accept offer in the last campaign: 1, otherwise: 0)
data$Response <- as.factor(data$Response)
table(data$Response)

#Defining dummy variables 
data$Education<-factor(data$Education)
data$Marital_Status<-factor(data$Marital_Status)
data$Dt_Customer = as.factor(data$Dt_Customer)
summary(data)

#3. Formalize the date
install.packages("sjmisc")
library(sjmisc)
for (i in 1:nrow(data)){
  if (str_contains(data$Dt_Customer[i],"/")){
    data$Dt_Customer[i]<-as.character(as.Date(data$Dt_Customer[i],format = "%m/%d/%Y"))
  }else if (str_contains(data$Dt_Customer[i],"-")){
    data$Dt_Customer[i]<-as.character(as.Date(data$Dt_Customer[i],format = "%d-%m-%Y"))
  }
}

data$Age<-2021-data$Year_Birth

summary(data$Marital_Status)
for (i in 1:nrow(data)){
  if(data$Marital_Status[i]=="Absurd" || data$Marital_Status[i]=="Alone" || data$Marital_Status[i]=="YOLO"){
    data$Marital_Status[i]<-"Single"
  }
}
data$Marital_Status<-droplevels(data$Marital_Status)
summary(data$Marital_Status)

summary(data$Education)
for(i in 1:nrow(data)){
  if(data$Education[i]=="2n Cycle"){
    data$Education[i]<-"Master"
  }
}
levels(data$Education)<-c("2n Cycle", "Basic", "Bachelor", "Master", "PhD")
data$Education<-droplevels(data$Education)
summary(data$Education)

par(mfrow=c(2,2))
for(i in 1:ncol(data)){
  if(is.numeric(data[1,i])){
    boxplot(data[,i], main=colnames(data)[i])
  }
}
income_order<-data[order(data$Income, decreasing = TRUE),]
head(income_order)
data<-data[-2234,]
income_order<-data[order(data$Income, decreasing = TRUE),]
head(income_order)

Age_order<-data[order(data$Age, decreasing = TRUE),]
head(Age_order)
data<-data[-c(240,340,193),]
Age_order<-data[order(data$Age, decreasing = TRUE),]
head(Age_order)

#First tree model Gini index
treeModel.1 = rpart(Response~., data=data[,-c(1,4,7,9,11,13:15,26:28)]) #default splitting method
#draw and print the tree model 
draw.tree(treeModel.1)
print(treeModel.1)
cutoff=0.5
probability=predict(treeModel.1, data = data)
prediction=probability[,2]>cutoff
actual=data$Response
result=table(actual,prediction)
result
accuracy=(result[1,1]+result[2,2])/sum(result) #perdiction accuracy
accuracy

#sensitivity results 
sensitivity=result[2,2]/(result[2,1]+result[2,2])
sensitivity

#specificity results 
specificity=result[1,1]/(result[1,1]+result[1,2])
specificity  


#Split data into training and validation 
set.seed(12345)
inTrain=sort(sample(1:2240, 700, replace = F))
train=data.frame(data[inTrain,])
validation=data.frame(data[-inTrain,])
default.model = rpart(Response~., data=train[,-c(1,4,7,9,11,13:15,26:28)])
draw.tree(default.model)
print(default.model)

probability=predict(default.model, newdata = validation)
prediction = probability[,2]>0.5
actual=validation$Response
result=table(actual, prediction)
result
accuracy=(result[1,1]+result[2,2])/sum(result) #perdiction accuracy 
accuracy 

#sensitivity results 
sensitivity=result[2,2]/(result[2,1]+result[2,2])
sensitivity

#specificity results 
specificity=result[1,1]/(result[1,1]+result[1,2])
specificity 

#building an overfit model 
overfit.model = rpart(Response~.,data=train[,-c(1,4,7,9,11,13:15,26:28)], minsplit=20, 
                      minbucket=10, maxdepth=10)

#prediction on validation 
probability=predict(overfit.model, newdata = validation)
prediction=probability[,2]>cutoff
actual = validation$Response
result = table(actual, prediction)
result
accuracy=(result[1,1]+result[2,2])/sum(result)  #perdiction accuracy
accuracy 

#sensitivity results 
sensitivity=result[2,2]/(result[2,1]+result[2,2])
sensitivity

#specificity results 
specificity=result[1,1]/(result[1,1]+result[1,2])
specificity


