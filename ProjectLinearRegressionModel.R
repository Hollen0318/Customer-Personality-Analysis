library(sjmisc)

#1. Missing Values
data<-read.csv("marketing_data.csv")
summary(data$Income)
data$Income[is.na(data$Income)]<-mean(data$Income,na.rm = TRUE)
summary(data$Income)


#2. Response variable already defined (accept offer in the last campaign: 1, 0 otherwise)
#set dummy variables 
data$Education<-factor(data$Education)
data$Marital_Status<-factor(data$Marital_Status)
data$Dt_Customer = as.factor(data$Dt_Customer)
summary(data)

#Formalize the data
for(i in 1:nrow(data)){
  if(str_contains(data$Dt_Customer[i],"/")){
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

#First linear Model 
linearModel = lm(Response~Year_Birth+Education+Income+Kidhome+MntMeatProducts+NumDealsPurchases+NumWebPurchases+NumCatalogPurchases
                 +NumStorePurchases+NumWebVisitsMonth+AcceptedCmp3+AcceptedCmp4+AcceptedCmp5+AcceptedCmp1+AcceptedCmp2, data=data)
summary(linearModel)


#Partition the data using seed value 12345
set.seed(12345)
train=sample(2240, 1568)
data.train=data[train,]
data.validation=data[-train,]
#training summary 
summary(data.train$Response)
#validation summary 
summary(data.validation$Response)
#t-test 
t.test(data.train$Response, data.validation$Response)

#Second linear Model for the training data 
linearModel.2 <- lm(Response~.,data=data.train[,-c(8,27,28,30)])
summary(linearModel.2)

Prediction <- predict(linearModel.2, newdata = data.validation)
Actual<-data.validation$Response
#Prediction Bias, the closer to 0 the better
AE=mean(Actual-Prediction)
#Prediction Accuracy, smaller is better
RMSE=sqrt(mean((Actual-Prediction)^2))
AE
RMSE

#Third linear Model 
linearModel.3 <- lm(Response~Education+Income+Kidhome+MntMeatProducts+NumDealsPurchases+NumWebPurchases+NumCatalogPurchases
                    +NumStorePurchases+NumWebVisitsMonth+AcceptedCmp3+AcceptedCmp4+AcceptedCmp5+AcceptedCmp1+AcceptedCmp2, data.train)
summary(linearModel.3)

Prediction.2<-predict(linearModel.3, newdata = data.validation)
Actual.2<-data.validation$Response

#Prediction Bias, the closer to 0 the better
AE2=mean(Actual.2-Prediction.2)

#Prediction Accuracy, smaller is better
RMSE2=sqrt(mean((Actual.2-Prediction.2)^2))
AE2
RMSE2