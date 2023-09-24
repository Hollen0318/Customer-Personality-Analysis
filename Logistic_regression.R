---
  title: "Project"
author: "Haoran Zhang"
date: "11/8/2021"
output: pdf_document
---
  1. Missing Values
```{r}
data<-read.csv("marketing_data.csv")
summary(data$Income)
data$Income[is.na(data$Income)]<-mean(data$Income,na.rm = TRUE)
summary(data$Income)
```
```{r}
#summary(data)
```
2. Factor the Complain variable (Complain: 1 if customer complained in the last 2 years, 0 otherwise)
```{r}
data$Education<-factor(data$Education)
data$Marital_Status<-factor(data$Marital_Status)
#summary(data)
```
3. Formalize the date
```{r}
library(sjmisc)
for (i in 1:nrow(data)){
  if (str_contains(data$Dt_Customer[i],"/")){
    data$Dt_Customer[i]<-as.character(as.Date(data$Dt_Customer[i],format = "%m/%d/%Y"))
  }else if (str_contains(data$Dt_Customer[i],"-")){
    data$Dt_Customer[i]<-as.character(as.Date(data$Dt_Customer[i],format = "%d-%m-%Y"))
  }
}
```
4. The Z_Revenue & Z_CostContact have Constant value, which don't provide any information so we should drop them.
```{r}
data<-subset(data,select=-c(27,28))
```
5. Compared to the year of birth, the age is more informative therefore we transform date of birth to age. 
```{r}
data$Age<-2021-data$Year_Birth
```
6. There are too many marital status, which might affects the efficiency of classification algorithms.
```{r}
summary(data$Marital_Status)
for (i in 1:nrow(data)){
  if (data$Marital_Status[i]=="Absurd" || data$Marital_Status[i]=="Alone" || data$Marital_Status[i]=="YOLO"  ){
    data$Marital_Status[i]<-"Single"
  }
}
data$Marital_Status<-droplevels(data$Marital_Status)
summary(data$Marital_Status)
```
7. There are 5 different values of Education, but according to 'Three Cycle System' from the European Higher Education Area, 2n Cycle is equal to a Master degree. Graduation degree is the same as a Bachelor's degree in Europe. Thus, we adjust the education level of all customers.
```{r}
summary(data$Education)
for (i in 1:nrow(data)){
  if (data$Education[i]=="2n Cycle"){
    data$Education[i]<-"Master"
  }
}
levels(data$Education) <- c("2n Cycle","Basic","Bachelor","Master","PhD")
data$Education<-droplevels(data$Education)
summary(data$Education)
```
8. Identify the outlier and remove them from the data
```{r}
par(mfrow=c(2,2))
for (i in 1:ncol(data)){
  if (is.numeric(data[1,i])){
    boxplot(data[,i],main=colnames(data)[i])
  }
}
income_order<-data[order(data$Income,decreasing = TRUE),]
head(income_order)
data<-data[-2234,]
income_order<-data[order(data$Income,decreasing = TRUE),]
head(income_order)

Age_order<-data[order(data$Age,decreasing = TRUE),]
head(Age_order)
data<-data[-c(240,340,193),]
Age_order<-data[order(data$Age,decreasing = TRUE),]
head(Age_order)
```
1) Supervised Learning Task - Predict Response: As the data description says, the column 'Response' stands for if a certain customer accepted the offer in the last campaign. So the question is whether we can use some customers' responses to this campaign to predict someone else's reactions ? If we can achieve this, a business could promote the campaign to customers that are more likely to accept the offer, which could help it make a more efficient marketing plan. 
```{r}
data_subset<-subset(data,select=-c(1,2,8))
logistic_model <- glm(formula = Response~.,family=binomial(link='logit'),data=data_subset)
summary(logistic_model)
```
```{r}
logistic_predicted_preference<-predict(object=logistic_model,newdata = data_subset)
count<-0
glm_probs = predict(logistic_model,newdata=data_subset,type="response")
glm_pred = rep(FALSE,nrow(data_subset))
glm_pred[glm_probs>.5]<-TRUE
# predicted = predicted.probability > cutoff
comparison<-data.frame(glm_pred,data_subset$Response)
error_matrix<-table(glm_pred,data_subset$Response)
for (a in logistic_predicted_preference) {
  count<-count+1
  if(a>0.5){
    logistic_predicted_preference[count] = 1
  }else{
    logistic_predicted_preference[count] = 0
  }
}
print(paste("The predicted preferences are",sum(logistic_predicted_preference==1)))
print(paste("Actual preferences are",sum(data_subset$Response)))
print(paste("Difference is",abs(sum(data_subset$Response)-sum(logistic_predicted_preference==1))))
```
# sensitivity
```{r}
error_matrix
error_matrix[2,2]/sum(error_matrix[2,])
```

# specifity
```{r}
error_matrix[1,1]/sum(error_matrix[1,])
```
# RMSE
```{r}
actual<-data_subset$Response
AE <- mean(actual-logistic_predicted_preference)
AE
RMSE  = sqrt(mean((actual-logistic_predicted_preference)^2))
RMSE
```
#The accuracy of the model
```{r}
(error_matrix[1,1]+error_matrix[2,2])/(sum(error_matrix[1,])+sum(error_matrix[2,]))
```

```{r}
sum_1=0
sum_2=0
count=0
for (i in actual){
  count=count+1
  if (i==1){
    sum_1=sum_1+(i-logistic_predicted_preference[count])^2
  }
  if (i==0){
    sum_2=sum_2+(i-logistic_predicted_preference[count])^2
  }
}
print(paste(sum_1,sum_2,length(actual)))
RMSE_Accepted<-sqrt(sum_1/length(actual))
RMSE_Resfused<-sqrt(sum_2/length(actual))
print(paste("The RMSE for Accepted Offer is ",RMSE_Accepted))
print(paste("The RMSE for Refused Offer is ",RMSE_Resfused))
```
```{r}
data_subset<-subset(data,select=-c(1,2,8,5,6,10,11,12,14,15,26,28))
logistic_model <- glm(formula = Response~.,family=binomial(link='logit'),data=data_subset)
summary(logistic_model)
```
```{r}
logistic_predicted_preference<-predict(object=logistic_model,newdata = data_subset)
count<-0
for (a in logistic_predicted_preference) {
  count<-count+1
  if(a>0.5){
    logistic_predicted_preference[count] = 1
  }else{
    logistic_predicted_preference[count] = 0
  }
}
print(paste("The predicted preferences are",sum(logistic_predicted_preference==1)))
print(paste("Actual preferences are",sum(data_subset$Response)))
print(paste("Difference is",abs(sum(data_subset$Response)-sum(logistic_predicted_preference==1))))
```
```{r}
actual<-data_subset$Response
RMSE  = sqrt(mean((actual-logistic_predicted_preference)^2))
RMSE
```
```{r}
sum_1=0
sum_2=0
count=0
for (i in actual){
  count=count+1
  if (i==1){
    sum_1=sum_1+(i-logistic_predicted_preference[count])^2
  }
  if (i==0){
    sum_2=sum_2+(i-logistic_predicted_preference[count])^2
  }
}
print(paste(sum_1,sum_2,length(actual)))
RMSE_Accepted<-sqrt(sum_1/length(actual))
RMSE_Resfused<-sqrt(sum_2/length(actual))
print(paste("The RMSE for Accepted Offer is ",RMSE_Accepted))
print(paste("The RMSE for Refused Offer is ",RMSE_Resfused))
```


