#source code for final project.

#checking the present working directory
getwd()

#changing the present working directory
setwd("E:\\Helpism\\Final-Project")

#reading the data
data <- read.csv("data.csv")

#displaying the first 6 rows of data
head(data)

#statistics of data 
str(data)

#summary of data
summary(data)

#displaying the data and checking the outliers using boxplot
boxplot(data$Customer.Lifetime.Value)

#checking the outliers through quartiles
quantile(data$Customer.Lifetime.Value, c(0,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.99,0.995,1))

#checking the number of rows available in the data before dropping the outliers
nrow(data)

#removing outliers of the data
data2 <- data[data$Customer.Lifetime.Value <14400, ]
data <- data2

#checking for the outilers from the data again
boxplot(data$Customer.Lifetime.Value)

#checking the total rows available after dropping outliers
nrow(data)

#checking for the null values
sapply(data, function(x) sum(is.na(x)))

#dropping of there are any missing values available
data <- na.omit(data)

# After studying the data, we find the dependent variable is customer life time value(clv)
# clv data is continuous variable, as such, we use the linear regression model.
# first we split the data into Development Sample and Validation Sample
# 70% of data is development sample and 30% is validation sample data
dt = sort(sample(nrow(data), nrow(data)*.7))
development_sample <- data[dt,]
validation_sample <- data[-dt,]

#checking if the data has been splitted
#counting the total number of rows in data
nrow(data)

#counting the total number of rows in development sample
nrow(development_sample)

#counting the total number of rows in validation sample
nrow(validation_sample)

#checking if the total number of rows of development sample and validation sample
all.equal(nrow(data),(nrow(development_sample) + nrow(validation_sample)))

#running the multiple regression model for development sample
fit = lm(Customer.Lifetime.Value ~ State + Response + Coverage +
           Education + Effective.To.Date + EmploymentStatus + Gender + 
           Income + Location.Code + Marital.Status + Monthly.Premium.Auto +
           Months.Since.Last.Claim + Months.Since.Policy.Inception +
           Number.of.Open.Complaints + Number.of.Policies + Policy.Type + 
           Policy + Renew.Offer.Type + Sales.Channel + Total.Claim.Amount + Vehicle.Class +
           Vehicle.Size, data = development_sample)

summary(fit)


model = lm(Customer.Lifetime.Value ~ I(State == "Nevada") + I(State == "Washington")+
           I(Coverage == "Premium") +
           I(Education == "College") + I(Education == "High School or Below")+
           I(Effective.To.Date == "1/29/11") + I(Effective.To.Date == "2/25/11")+
           I(Effective.To.Date == "2/11/11") + I(Effective.To.Date == "2/12/11")+ 
           I(Effective.To.Date == "2/4/11")+
           I(EmploymentStatus == "Unemployed") + Monthly.Premium.Auto +
           Number.of.Open.Complaints + Number.of.Policies + 
           Renew.Offer.Type +  I(Vehicle.Class == "Luxury SUV")  +
           I(Vehicle.Class == "SUV"), data = development_sample)

summary(model)
 
# lets perform assumption diagnostic test for the above linear model.
# Normalization Test
# Anderson - Daring Test
resids <- model$residuals
ad.test(resids)

#Homoscedasticity Test
bptest(model)


#Multicollinearity Test
vif(model)


#Auto Correlation Test
dwt(model)


#prediction on validation sample
val_predict <- predict(model, validation_sample)
val_predict

## MAPE
validation_sample$pred <- val_predict

#Calculating MAPE
head(validation_sample)
(sum((abs(validation_sample$Customer.Lifetime.Value-validation_sample$pred))/validation_sample$Customer.Lifetime.Value))/nrow(validation_sample)

write.csv(validation_sample,"validation_data.csv")








