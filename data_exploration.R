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

# 4. The Z_Revenue & Z_CostContact have Constant value, which don't provide any information so we should drop them.

data<-subset(data,select=-c(27,28))

# 5. Compared to the year of birth, the age is more informative therefore we transform date of birth to age. 

data$Age<-2021-data$Year_Birth

# 6. There are too many marital status, which might affects the efficiency of classification algorithms.

summary(data$Marital_Status)
for (i in 1:nrow(data)){
  if (data$Marital_Status[i]=="Absurd" || data$Marital_Status[i]=="Alone" || data$Marital_Status[i]=="YOLO"  ){
    data$Marital_Status[i]<-"Single"
  }
}
data$Marital_Status<-droplevels(data$Marital_Status)
summary(data$Marital_Status)

# 7. There are 5 different values of Education, but according to 'Three Cycle System' from the European Higher Education Area, 2n Cycle is equal to a Master degree. Graduation degree is the same as a Bachelor's degree in Europe. Thus, we adjust the education level of all customers.

summary(data$Education)
for (i in 1:nrow(data)){
  if (data$Education[i]=="2n Cycle"){
    data$Education[i]<-"Master"
  }
}
levels(data$Education) <- c("2n Cycle","Basic","Bachelor","Master","PhD")
data$Education<-droplevels(data$Education)
summary(data$Education)

# 8. Identify the outlier and remove them from the data

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

library(tidyverse)
library(plyr) #count()
library(GGally) #ggcorr() and ggpairs()
library(reshape) #melt()
library(corrplot) #corrplot


#comprehensive boxplots
unwant.cols <- c('AcceptedCmp1', 'AcceptedCmp2', 'AcceptedCmp3', 'AcceptedCmp4', 'AcceptedCmp5', 'Complain',
                 'Education', 'Rel_Status', 'Dt_Customer', 'Response', 'AcceptedPrv')
melt.data <- data %>%
  select(-one_of(unwant.cols)) %>%
  melt()

ggplot(melt.data, aes(factor(variable), value)) +
  geom_boxplot(color = 'steelblue') +
  facet_wrap(~variable, scale = 'free') +
  labs(title = 'Boxplots of Various Variables', x = 'Variables', y = 'Ranges')

#correlation plot between numeric vectors
Correlation_plot <- ggcorr(select(marketing, -one_of(unwant.cols)), 
                           geom = 'blank', label = TRUE, hjust = 0.75, layout.exp = 3) +
  geom_point(size = 10, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.6)) +
  scale_alpha_manual(values = c('TRUE' = 0.25, 'FALSE' = 0)) +
  guides(color = 'none', alpha = 'none') +
  labs(title = 'Correlation Map')

Correlation_plot

