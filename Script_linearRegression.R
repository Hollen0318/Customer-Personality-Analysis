#1. Missing Values
data<-read.csv("marketing_data.csv")
summary(data$Income)
data$Income[is.na(data$Income)]<-mean(data$Income,na.rm = TRUE)
summary(data$Income)
#summary(data)
#2. Factor the Complain variable (Complain: 1 if customer complained in the last 2 years, 0 otherwise)
data$Education<-factor(data$Education)
data$Marital_Status<-factor(data$Marital_Status)
#summary(data)
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

linearModel = lm(Income~Education+Marital_Status+Kidhome+Teenhome, data=data)
summary(linearModel)