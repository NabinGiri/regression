Problem Domain : 
Predict Customer Life-time Value for an Auto Insurance Company.

Objective : 
For an Auto Insurance company, predict the conditions effecting customer life time value
(CLV). CLV is the total revenue the client will derive from their entire relationship with a
customer. Because we don&#39;t know how long each customer relationship will be, we make a
good estimate and state CLV as a periodic value — that is, we usually say “this customer&#39;s
12-month (or 24-month, etc) CLV is $x”.

Approach Used:
1) Understanding the variables in the data set and study about the Industry
2) Created hypothesis and validated
3) Identified the statistical model to use- used linear regression model as the dependent variable is continuous
4) Cleaned the data set and removed any outliers - used boxplot and quartile
5) Divided the data set into two parts: - Development Sample (training data - 70%) & Validation Sample (testing data - 30%)
6) Built Regression model.
7) Checked which variables are significant by looking at p values and business reasons.
8) Preformed different tests like multicollinearity test, Homoscadasticity test, Normality test, Auto Correlation, MAPE.
9) Created a business report with final recommendations / insights.

Files:
1) Code.R = contains the R code of the model
2) data.csv = the dataset of the auto insurance company
3) validation.csv = the output saved in csv file of validation sample
4) project.docx = the detail explaination of the solution
