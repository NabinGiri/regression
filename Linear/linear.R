library(boot)
library(car)
library(QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)
library(MASS)

#setting the working directory
getwd()
setwd("E:\\Helpism\\Linear")
#loading data
data <- read.csv("data.csv")
#checking the first 6 rows of data
head(data)
#checking the structure of data
str(data)
#checking the mean, median, quartile of data
summary(data)
#displaying the dependent variable through boxplot; to see the outliers
boxplot(data$SalesInThousands)
#checking the outliers through quartile.
quantile(data$SalesInThousands, c(0,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.99,0.995,1))
#removing the outliers
data2 <- data[data$SalesInThousands <75, ]
data3 <- data2[data2$SalesInThousands >20, ]
#displaying the dependent variable through box  plot; to see the outliers
boxplot(data2$SalesInThousands)
boxplot(data3$SalesInThousands)
#checking for missing values
sapply(data3, function(x) sum(is.na(x)))
#removing null values if any
data3 <- na.omit(data3)
data <- data3
nrow(data)
#fitting into a linear model
fit<- lm(SalesInThousands ~ MarketID + MarketSize +	LocationID +	AgeOfStore +	Promotion +	
           week, data=data)
summary(fit)
#keeping only significant codes
final_fit<- lm(SalesInThousands ~  MarketSize +	AgeOfStore +	Promotion, data=data)
summary(final_fit)

##Assumption Diagnostic Tests
#Multicollinearity Test
#checking the VIF score; vif>2 means presence of multicollinearity
vif(final_fit)
## Get the predicted or fitted values
fitted(final_fit)
## MAPE
data$pred <- fitted(final_fit)
#Calculating MAPE
attach(data)
data
(sum((abs(data$SalesInThousands-data$pred))/data$SalesInThousands))/nrow(data)
dwt(final_fit)
bptest(final_fit)
resids <- final_fit$residuals
resids
#get Anderson-Darling test for normality 
ad.test(resids)

#writing in excel
write.csv(data,"linear_output.csv")
