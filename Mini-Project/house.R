#importing required libraries

library(boot)
library(car)
library(QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)
library(MASS)

#checking the present working directory
getwd()

#changing the working directory
setwd("E:\\Helpism\\Project")

#checking the working directory again
getwd()

#loading the housing data
data <- read.csv("Data.csv")

#displaying the top 6 data
head(data)

#summary of data
summary(data)

#checking the null values
sapply(data, function(x) sum(is.na(x)))

#removing the null values
data2 <- na.omit(data)
data <- data2

#checking the null values again
sapply(data, function(x)sum(is.na(x)))

#now, our data is cleaned
#next, we check for the outliers
#displaying the dependent variable through boxplot; to see the outliers
boxplot(data$Price_house)

#checking the outliers through quartile.
quantile(data$Price_house, c(0,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.99,0.995,1))

#removing the outliers
data2 <- data[data$Price_house <11207235, ]
data <- data2
boxplot(data$Price_house)


#the data has been cleaned and outliers has been removed. 
# in next steps, we will fit the data into linear model.
head(data)
fit<- lm(Price_house ~ Taxi_dist + Market_dist +	Hospital_dist  +	Carpet_area  +	Builtup_area +	
      Parking_type + City_type + Rainfall, data=data)
summary(fit)

#fitting only significant codes
fit <- lm(Price_house ~ Parking_type + City_type, data=data)
summary((fit))

###Assumption Diagnostic Tests

#Multicollinearity Test
#checking the VIF score; vif>2 means presence of multicollinearity
vif(fit)

## Get the predicted or fitted values
fitted(fit)

## MAPE
data$pred <- fitted(fit)


#Calculating MAPE
attach(data)
data
(sum((abs(data$Price_house-data$pred))/data$Price_house))/nrow(data)

#Test for Auto-Correlation
#Durbin Watson Test
#if values are greater than 2, then auto-correlation exists
dwt(fit)

#Test for Homoscedasticity
#Breusch-Pagan Test
#p-value should be greater than 0.05, so we accept null hypothesis, i.e test is homoscedasticity
bptest(fit)

#Normality Test
#Anderson-Darling test
resids <- fit$residuals
resids
ad.test(resids)


#writing in excel
write.csv(data,"house_prediction.csv")

