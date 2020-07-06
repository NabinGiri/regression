library(caret)
library(ggplot2)
library(MASS)
library(car)
library(mlogit)
library(sqldf)
library(Hmisc)

# set the working directory
getwd()
setwd("E:/Helpism/Logistic")
getwd()

#read the data
data <- read.csv("data.csv")
head(data)

str(data)

#converting the categorical variables into factors
data$SeniorCitizen <- as.factor(data$SeniorCitizen)
data$Dependents <- as.factor(data$Dependents)
data$PhoneService <- as.factor(data$PhoneService)
data$MultipleLines <- as.factor(data$MultipleLines)
data$StreamingTV <- as.factor((data$StreamingTV))
data$StreamingMovies <- as.factor((data$StreamingMovies))
data$Contract <- as.factor((data$Contract))

str(data)

#IV for numeric data
iv_num <- function(variable, target,data,groups)
{
  data[,"rank"] <- cut2(data[,variable],g=groups)
  tableOutput <-sqldf(sprintf("select rank, 
                        count(%s) n,
                        sum(%s) good
                 from data 
                 group by rank",target,target))
  tableOutput <- sqldf("select *,
                        (n - good) bad
                         from tableOutput")
  tableOutput$bad_rate<- tableOutput$bad/sum(tableOutput$bad)*100
  tableOutput$good_rate<- tableOutput$good/sum(tableOutput$good)*100
  tableOutput$WOE<- (log(tableOutput$good_rate/tableOutput$bad_rate))*100
  tableOutput$IV <- (log(tableOutput$good_rate/tableOutput$bad_rate))*(tableOutput$good_rate-tableOutput$bad_rate)/100
  IV <- sum(tableOutput$IV[is.finite(tableOutput$IV)])
  IV1 <- data.frame(cbind(variable,IV))
  return(IV1)
}

#checking IV for independent variables
a1<- iv_num("MonthlyCharges","Churn",data,groups=10)
a2<- iv_num("TotalCharges","Churn",data,groups=10)
IV_num<- data.frame(rbind(a1,a2))
IV_num  

#IV for categorical variables
iv_char <- function(variable, target, data) {
  pintu1<- fn$sqldf("select $variable,count($target)n, sum($target)good from data group by $variable")
  
  pintu1<- fn$sqldf("select *, (n-good) bad from pintu1")
  pintu1$bad_rate <- pintu1$bad/sum(pintu1$bad)*100
  
  pintu1$good_rate<- pintu1$good/sum(pintu1$good)*100
  pintu1$WOE<- (log(pintu1$good_rate/pintu1$bad_rate))*100
  pintu1$IV <- (log(pintu1$good_rate/pintu1$bad_rate))*(pintu1$good_rate-pintu1$bad_rate)/100
  IV <- sum(pintu1$IV[is.finite(pintu1$IV)])
  IV1 <- data.frame(cbind(variable,IV))
  return(IV1)
}

str(data)

#IV for categorical variables
A<- iv_char("SeniorCitizen","Churn",data)
B<- iv_char("Dependents","Churn",data)
C<- iv_char("tenure","Churn",data)
D<- iv_char("PhoneService","Churn",data)
E<- iv_char("MultipleLines","Churn",data)
G<- iv_char("StreamingTV","Churn",data)
H<- iv_char("StreamingMovies","Churn",data)
I<- iv_char("Contract","Churn",data)

IV_cat<- data.frame(rbind(A,B,C,D,E,G,H,I))

IV_cat

Final_IV <- data.frame(rbind(IV_num,IV_cat))
Final_IV

#summary
summary(data)
str(data)

#checking outliers
boxplot(data$MonthlyCharges)
boxplot(data$TotalCharges)

sapply(data, function(x) sum(is.na(x)))
data <- na.omit(data)
sapply(data, function(x) sum(is.na(x)))

#logistic regression
model <- glm(Churn ~ gender +	SeniorCitizen +	Partner +	Dependents +	
               tenure +	PhoneService +	MultipleLines + InternetService + 
               OnlineSecurity + OnlineBackup+ DeviceProtection + TechSupport +
               StreamingTV + StreamingMovies+ Contract + PaperlessBilling + 
               PaymentMethod + MonthlyCharges + TotalCharges , data=data, family=binomial())

summary(model)

model <- glm(Churn ~ SeniorCitizen +	Dependents +	
               tenure +	 InternetService + Contract + PaperlessBilling + TotalCharges , data=data, family=binomial())

summary(model)

#VIF
vif(model)


# R square
modelChi <- model$null.deviance - model$deviance

#Finding the degree of freedom for Null model and model with variables
chidf <- model$df.null - model$df.residual
chisq.prob <- 1 - pchisq(modelChi, chidf)
R2.hl<-modelChi/model$null.deviance
R.cs <- 1 - exp ((model$deviance - model$null.deviance) /nrow(data))
R.n <- R.cs /(1-(exp(-(model$null.deviance/(nrow(data))))))
R.n ## ranges from 0 to 1; closer to 1 better the model

# Predicted Probabilities
prediction <- predict(model,newdata = data,type="response")
library(pROC)
rocCurve   <- roc(response = data$Churn, predictor = prediction, 
                  levels = rev(levels(data$Churn)))
data$Churn <- as.factor(data$Churn)
#Metrics - Fit Statistics
predclass <-ifelse(prediction>coords(rocCurve,"best")[1],1,0)
predclass
Confusion <- table(Predicted = predclass,Actual = data$Churn)
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
Gini <-2*auc(rocCurve)-1
AUCmetric <- data.frame(c(coords(rocCurve,"best"),AUC=auc(rocCurve),AccuracyRate=AccuracyRate,Gini=Gini))
AUCmetric <- data.frame(rownames(AUCmetric),AUCmetric)
rownames(AUCmetric) <-NULL
names(AUCmetric) <- c("Metric","Values")
AUCmetric

Confusion 
plot(rocCurve)

names(data)[9] <- "pred"
write.csv(data,"logistic.csv")
