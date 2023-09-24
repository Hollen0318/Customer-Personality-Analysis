---
  title: "KNN"
author: "Haoran Zhang"
date: "11/27/2021"
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
Seperate Train and Validation Data
```{r}
set.seed(2322)
data_subset<-subset(data,select=-c(1,2,8,27))
data_subset$Education<-as.numeric(data_subset$Education)
data_subset$Education<-as.factor(data_subset$Education)
data_subset$Marital_Status<-as.numeric(data_subset$Marital_Status)
data_subset$Marital_Status<-as.factor(data_subset$Marital_Status)
N = nrow(data_subset)
train.id = sort(sample(N, N*0.6))
validate.id = seq(N)[-train.id]
data_input<-data_subset
for (i in 1:ncol(data_subset)){
  if (!is.factor(data_subset[,i])){
    data_input[,i]<-scale(data_subset[,i])
  }
}
# data_input
train_input = data_input[train.id,]
validate_input = data_input[validate.id,]
train_output = data[train.id, c(27)]
validate_output = data[validate.id, c(27)]
```
```{r}
library(class)
error_train_list = seq(20)*0
error_validate_list = seq(20)*0
for(i in seq(20)){
  prediction_train = knn(train_input, train_input, train_output, k=i)
  prediction_validate = knn(train_input, validate_input, train_output, k=i)
  prediction_validate
  error_train_list[i] = mean(abs(as.numeric(as.character(prediction_train))-
                                   train_output))
  error_validate_list[i] = mean(abs(as.numeric(as.character(prediction_validate))- validate_output))
}
```
```{r}
plot(seq(20), error_train_list, col="blue", type="b", xlab="k", ylab="Error rate", xlim = c(0,20))
plot(seq(20), error_validate_list, col="blue", type="b", xlab="k", ylab="Error rate", xlim = c(0,20))
print(match(min(error_validate_list),error_validate_list))
```
Based on the graph and min value of validation error, the best k is 6.

Select k = 6, calculate the error of validate (AE)
```{r}
prediction_validate<-knn(train_input, validate_input, train_output, k=6)
error_validate<-mean(abs(as.numeric(as.character(prediction_validate))-validate_output))

error_validate
```
The RMSE
```{r}
RMSE<-sqrt(mean(abs(as.numeric(as.character(prediction_validate))-validate_output)^2))
RMSE
```

Show the confusion matrix for the validation data that results from using the best k.   
```{r}
error_matrix<-table(validate_output,prediction_validate)
error_matrix
```
Sensitivity
```{r}
error_matrix[2,2]/sum(error_matrix[2,])
```
Specifity
```{r}
error_matrix[1,1]/sum(error_matrix[1,])
```
The accuracy of the model
```{r}
(error_matrix[1,1]+error_matrix[2,2])/(sum(error_matrix[1,])+sum(error_matrix[2,]))
```