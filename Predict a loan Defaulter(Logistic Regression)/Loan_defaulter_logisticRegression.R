#Download the data set as german_credit,stringsasfactors is kept as false as we dont want strings to be
#considered as factors or categorical data.
german_credit<-read.csv("german.csv",stringsAsFactors = F)

#install the required packages.

#install.packages("car")
#install.packages("Hmisc")
#install.packages("ROCR")
#install.packages("Caret")

library("car")
library("Hmisc")
library("ROCR")
library("caret")

#-------------------------------------------------------------------------------------------------------------------------------

#Checkpoint 1: Data Understanding and Data Exploration

#-------------------------------------------------------------------------------------------------------------------------------

#The Business objective of the assignment is to find out the chancees of if a customer is likely to 
#default while paying his credit card bill amount.So, from teh data available we have to predict if the 
#custome will default or not

#Thus the above is the aim of solving the assignment,now first we will see the structure of the dataset
#to explore the data at hand.

str(german_credit)

#Thus we observe that there are 1000rows of 21 variables
#using the data dictionary to understand what each varaible means.
#now we check the summary of the dataset

summary(german_credit)

#looking at the summary we understand that the dataset currently has a set of some character variables 
#and some integer variables!

#to understand the data carefully we will do univariate analysis of some variables
#We will do univariate analysis for the following variables.

#First we check our target variable and see how it is spreaded across its both levels.
ggplot(german_credit,aes(factor(german_credit$Default_status)))+geom_bar()

#We now check the status of existing checking account
ggplot(german_credit,aes(factor(german_credit$Status.of.existing.checking.account)))+geom_bar()

#We now check the for Credit history
ggplot(german_credit,aes(factor(german_credit$Credit.history)))+geom_bar()

#We now check the for Savings account bonds
ggplot(german_credit,aes(factor(german_credit$Savings.account.bonds)))+geom_bar()

#We now check the Jobs Status.
ggplot(german_credit,aes(factor(german_credit$Job_status)))+geom_bar()
#------------------------------------------------------------------------------------------------------------------------------
  
#Checkpoint 2: Data Cleaning and Transformation
 
#-------------------------------------------------------------------------------------------------------------------------------
#Data Cleaning: Check for na's in the whole dataset
  
sum(is.na(german_credit))

#the data looks clean there are no na values in the whole dataset !

#Data cleaning:we will check for outliers in the dataset next
#before doing this lets transform the variables in their correct data type or class type 
#look at the structure of the dataset to see the variables in the dataset!

str(german_credit)
#let us look at every variable in the dataset,

#Attribute no. 1
#status of existing checking account: This should be a categorical/factor variable

german_credit$Status.of.existing.checking.account<-as.factor(german_credit$Status.of.existing.checking.account)

#Attribute no. 2
#Duration in month: This should numerical and is numerical so no transformation needed.

#Attribute no. 3
#Credit history: This should be categorical/factor variable

german_credit$Credit.history<-as.factor(german_credit$Credit.history)

#Attribute no. 4
#Purpose: This should be categorical/factor variable

german_credit$Purpose<-as.factor(german_credit$Purpose)

#Attribute no. 5
#Credit amount: This should be numerical and is numerical so no transformation needed.

#Attribute no. 6
#Savings account bonds: This should be categorical/factor variable

german_credit$Savings.account.bonds<-as.factor(german_credit$Savings.account.bonds)

#Attribute no. 7
#Present employment since: This should be categorical/factor variable

german_credit$Present.employment.since.<-as.factor(german_credit$Present.employment.since.)

#Attribute no. 8
#Installment rate in percentage of disposable income: This should be numerical and is numerical so no transformation needed.


#Attribute no. 9
#Personal status and sex: This should be categorical/factor variable

german_credit$Personal.status.and.sex<-as.factor(german_credit$Personal.status.and.sex)

#Attribute no. 10
#Other debtors guarantors: This should be categorical/factor variable

german_credit$Other.debtors...guarantors<-as.factor(german_credit$Other.debtors...guarantors)

#Attribute no. 11
#Present residence since: This should be numerical and is numerical so no transformation needed.

#Attribute no. 12
#Property: This should be categorical/factor variable.

german_credit$Property<-as.factor(german_credit$Property)

#Attribute no. 13
#Age in Years: This should be numerical and is numerical so no transformation needed.

#Attribute no. 14
#Other installment plans:This should be categorical/factor variable.

german_credit$Other.installment.plans<-as.factor(german_credit$Other.installment.plans)

#Attribute no. 15
#Housing:This should be categorical/factor variable.

german_credit$Housing.<-as.factor(german_credit$Housing.)

#Attribute no. 16
# Number of existing credits at this bank.:This should be numerical and is numerical so no transformation needed.

#Attribute no. 17
#Job_status :This should be categorical/factor variable.

german_credit$Job_status<-as.factor(german_credit$Job_status)

#Attribute no. 18
#Number of people being liable to provide maintenance for :This should be numerical and is numerical so no transformation needed.


#Attribute no. 19
#Telephone:This should be categorical/factor variable.

german_credit$Telephone.<-as.factor(german_credit$Telephone.)

#Attribute no. 20
#foreign.worker:This should be categorical/factor variable.

german_credit$foreign.worker<-as.factor(german_credit$foreign.worker)

#Attribute no. 21
#Default_status:This is the target variable remains as numerical

#german_credit$Default_status<-as.factor(german_credit$Default_status)

#lets look at the struture of the dataset now:
str(german_credit)

#we have following int values: Duration of month,credit amount,Installment.rate.in.percentage.of.disposable.income ,
#Present.residence.since,Age.in.Years,Number.of.existing.credits.at.this.bank.,Number.of.people.being.liable.to.provide.maintenance.for.

#checking outliers for numerical variables:

#Duration of month:
quantile(german_credit$Duration.in.month,seq(0,1,0.01))
#no outliers detected here in duration of month

#Credit amount:
quantile(german_credit$Credit.amount,seq(0,1,0.01))
#no outliers detected here in duration of month

#Installment rate in percentage of disposable income:
quantile(german_credit$Installment.rate.in.percentage.of.disposable.income,seq(0,1,0.01))
#no outliers detected here in Installment rate in percentage of disposable income

#Present residence since:
quantile(german_credit$Present.residence.since,seq(0,1,0.01))
#no outliers detected here in Present residence since

#Number of existing credits at this bank:
quantile(german_credit$Number.of.existing.credits.at.this.bank.,seq(0,1,0.01))
#no outliers detected here in Number of existing credits at this bank

#Number of people being liable to provide maintenance for
quantile(german_credit$Number.of.people.being.liable.to.provide.maintenance.for.,seq(0,1,0.01))
#no outliers detected here in Number of people being liable to provide maintenance for

#so we have removed all the outliers from the dataset, and clean the data from all the NA's .We have also 
#transformed the variables into numerical and factor variables.

#we will now create dummy variables out of the factor variables

dummy_status_of_existing_checking_amount<-model.matrix(~german_credit$Status.of.existing.checking.account -1, data=german_credit)
dummy_credit_history<-model.matrix(~german_credit$Credit.history -1, data=german_credit)
dummy_purpose<-model.matrix(~german_credit$Purpose -1, data=german_credit)
dummy_savings_account_bonds<-model.matrix(~german_credit$Savings.account.bonds -1, data=german_credit)
dummy_present_employment_since<-model.matrix(~german_credit$Present.employment.since. -1, data=german_credit)
dummy_personal_status_and_sex<-model.matrix(~german_credit$Personal.status.and.sex -1, data=german_credit)
dummy_other_debtors_guarantors<-model.matrix(~german_credit$Other.debtors...guarantors -1, data=german_credit)
dummy_property<-model.matrix(~german_credit$Property -1, data=german_credit)
dummy_other_intallment_plans<-model.matrix(~german_credit$Other.installment.plans -1, data=german_credit)
dummy_housing<-model.matrix(~german_credit$Housing. -1, data=german_credit)
dummy_job_status<-model.matrix(~german_credit$Job_status -1, data=german_credit)
dummy_telephone<-model.matrix(~german_credit$Telephone. -1, data=german_credit)
dummy_foreign_worker<-model.matrix(~german_credit$foreign.worker -1, data=german_credit)

#add the dummy variables to the german credit and remove the categorical variables
german_credit_new<-german_credit[c(2,5,8,11,13,16,18,21)]
dummy<-cbind(dummy_status_of_existing_checking_amount,dummy_credit_history,dummy_purpose,dummy_savings_account_bonds,dummy_present_employment_since,
             dummy_personal_status_and_sex,dummy_other_debtors_guarantors,dummy_property,dummy_other_intallment_plans,
             dummy_housing,dummy_job_status,dummy_telephone,dummy_foreign_worker)

german_credit<-cbind(german_credit_new,dummy)

#-----------------------------------------------------------------------------------------------------------------------------

#Checkpoint 3: Splitting the Dataset into train and test

#-----------------------------------------------------------------------------------------------------------------------------

#setting the seed to 100 and then splitting the data into 70:30 for train:test dataset 
#we will use sample.split from the caTools package
library(caTools)
indices=sample.split(german_credit,0.7)

#putting the data as train and test
train = german_credit[indices==T,]
test  = german_credit[indices==F,]

#-----------------------------------------------------------------------------------------------------------------------------

#Checkpoint 4:Modeling

#-----------------------------------------------------------------------------------------------------------------------------

#build the initial model
initial_model = glm(train$Default_status ~ ., data = train, family = "binomial")  
summary(initial_model)

#build the stepwise model to remove insignificant variables

step_model = step(initial_model,direction = "both")

#look at the step model

step_model

#build a model with those variables and name it model_1
model_1<-glm(formula = train$Default_status ~ Duration.in.month + Credit.amount + 
               Installment.rate.in.percentage.of.disposable.income + `german_credit$Status.of.existing.checking.accountA11` + 
               `german_credit$Status.of.existing.checking.accountA12` + 
               `german_credit$Credit.historyA30` + `german_credit$Credit.historyA31` + 
               `german_credit$Credit.historyA32` + `german_credit$PurposeA41` + 
               `german_credit$PurposeA410` + `german_credit$PurposeA42` + 
               `german_credit$PurposeA43` + `german_credit$Savings.account.bondsA61` + 
               `german_credit$Savings.account.bondsA62` + `german_credit$Present.employment.since.A71` + 
               `german_credit$Present.employment.since.A74` + `german_credit$Personal.status.and.sexA93` + 
               `german_credit$Other.debtors...guarantorsA101` + `german_credit$Other.debtors...guarantorsA102` + 
               `german_credit$Other.installment.plansA141` + `german_credit$Other.installment.plansA142` + 
               `german_credit$Housing.A151` + `german_credit$foreign.workerA201` + 
               `german_credit$PropertyA124`, family = "binomial", data = train)

#check the summary of model_1

summary(model_1)

#check the VIF values to make it free from multi collinearity
#we will consider the VIF values less than 3 only
vif(model_1)

#all the vif are less than 3 so we will just remove the insignificant variables based on p values
#We will remove all the variables with no stars

model_2<-glm(formula = train$Default_status ~ Duration.in.month + 
               Installment.rate.in.percentage.of.disposable.income + `german_credit$Status.of.existing.checking.accountA11` + 
               `german_credit$Status.of.existing.checking.accountA12` + 
               `german_credit$Credit.historyA30` + `german_credit$Credit.historyA31` + 
               `german_credit$Credit.historyA32` + `german_credit$PurposeA41` + 
               `german_credit$PurposeA42` + 
               `german_credit$PurposeA43` + `german_credit$Savings.account.bondsA61` + 
               `german_credit$Present.employment.since.A74` + `german_credit$Personal.status.and.sexA93` + 
               `german_credit$Other.debtors...guarantorsA102` + 
               `german_credit$Other.installment.plansA141` + 
               `german_credit$Housing.A151` + `german_credit$foreign.workerA201` , family = "binomial", data = train)

#check the summary of model_2
summary(model_2)

#We will remove all the variables with no stars and name the model as model_3

model_3<-glm(formula = train$Default_status ~ Duration.in.month + Installment.rate.in.percentage.of.disposable.income + 
               `german_credit$Status.of.existing.checking.accountA11` + 
               `german_credit$Status.of.existing.checking.accountA12` + 
               `german_credit$Credit.historyA30` + `german_credit$Credit.historyA31` + 
               `german_credit$Credit.historyA32` + `german_credit$PurposeA41` + 
               `german_credit$PurposeA42` + `german_credit$PurposeA43` + 
               `german_credit$Savings.account.bondsA61` + `german_credit$Present.employment.since.A74` + 
                `german_credit$Housing.A151` + 
               `german_credit$foreign.workerA201`, family = "binomial", 
             data = train)

#check summary for model_3
summary(model_3)

#We will remove all the variables with no stars and name the model as model_4

model_4<-glm(formula = train$Default_status ~ Duration.in.month + Installment.rate.in.percentage.of.disposable.income + 
               `german_credit$Status.of.existing.checking.accountA11` + 
               `german_credit$Status.of.existing.checking.accountA12` + 
               `german_credit$Credit.historyA30` + `german_credit$Credit.historyA31` + 
               `german_credit$Credit.historyA32` + `german_credit$PurposeA41` + 
               `german_credit$PurposeA42` + `german_credit$PurposeA43` + 
               `german_credit$Savings.account.bondsA61` + `german_credit$Present.employment.since.A74` + 
               `german_credit$Housing.A151` , 
             family = "binomial", data = train)

#We will remove all the variables with no stars and name the model as model_4
summary(model_4)

#We will remove all the variables with one star now one by one and name the model as model_5

#removing the variables based on p value untill all the one stared variables are removed.

#first removing the variable `german_credit$PurposeA42` as it has the 
#highest p value among all the variables

model_5<-glm(formula = train$Default_status ~ Duration.in.month + Installment.rate.in.percentage.of.disposable.income + 
               `german_credit$Status.of.existing.checking.accountA11` + 
               `german_credit$Status.of.existing.checking.accountA12` + 
               `german_credit$Credit.historyA30` + `german_credit$Credit.historyA31` + 
               `german_credit$Credit.historyA32` + `german_credit$PurposeA41` + 
                `german_credit$PurposeA43` + 
               `german_credit$Savings.account.bondsA61` + `german_credit$Present.employment.since.A74` + 
               `german_credit$Housing.A151`, family = "binomial", data = train)
#check the summary of model
summary(model_5)

#Removing the variable `german_credit$Housing.A151` as it has the 
#highest p value among all the remaining variables and name it model_6

model_6<-glm(formula = train$Default_status ~ Duration.in.month + Installment.rate.in.percentage.of.disposable.income + 
               `german_credit$Status.of.existing.checking.accountA11` + 
               `german_credit$Status.of.existing.checking.accountA12` + 
               `german_credit$Credit.historyA30` + `german_credit$Credit.historyA31` + 
               `german_credit$Credit.historyA32` + `german_credit$PurposeA41` + 
               `german_credit$PurposeA43` + `german_credit$Savings.account.bondsA61` + 
               `german_credit$Present.employment.since.A74` , 
             family = "binomial", data = train)

#check the summary of model
summary(model_6)

#We will remove all the variables with two star now one by one and name the model as model_7

#removing the variables based on p value untill all the two stared variables are removed.

#Removing the variable `german_credit$Present.employment.since.A74` as it has the 
#highest p value among all the remaining variables and name it model_7

model_7<-glm(formula = train$Default_status ~ Duration.in.month + Installment.rate.in.percentage.of.disposable.income + 
               `german_credit$Status.of.existing.checking.accountA11` + 
               `german_credit$Status.of.existing.checking.accountA12` + 
               `german_credit$Credit.historyA30` + `german_credit$Credit.historyA31` + 
               `german_credit$Credit.historyA32` + `german_credit$PurposeA41` + 
               `german_credit$PurposeA43` + `german_credit$Savings.account.bondsA61`, family = "binomial", 
             data = train)
#check the summary of model
summary(model_7)

#Removing the variable `german_credit$PurposeA43` as it has the 
#highest p value among all the remaining variables and name it model_8

model_8<-glm(formula = train$Default_status ~ Duration.in.month + Installment.rate.in.percentage.of.disposable.income + 
               `german_credit$Status.of.existing.checking.accountA11` + 
               `german_credit$Status.of.existing.checking.accountA12` + 
               `german_credit$Credit.historyA30` + `german_credit$Credit.historyA31` + 
               `german_credit$Credit.historyA32` + `german_credit$PurposeA41` + 
               `german_credit$Savings.account.bondsA61`, 
             family = "binomial", data = train)
#check the summary of model
summary(model_8)

#removing `german_credit$PurposeA41` as it has one star so remove it
#and name the model as model_9

model_9<-glm(formula = train$Default_status ~ Duration.in.month + Installment.rate.in.percentage.of.disposable.income + 
      `german_credit$Status.of.existing.checking.accountA11` + 
      `german_credit$Status.of.existing.checking.accountA12` + 
      `german_credit$Credit.historyA30` + `german_credit$Credit.historyA31` + 
      `german_credit$Credit.historyA32` +  
      `german_credit$Savings.account.bondsA61`, family = "binomial", 
    data = train)

#check the summary of model
summary(model_9)

#removing `german_credit$Credit.historyA32` as it has two star and has the highest p value among the 2 stared variables 
#and name the model as model_10

#now keep an eye on AIC 

model_10<-glm(formula = train$Default_status ~ Duration.in.month + Installment.rate.in.percentage.of.disposable.income + 
               `german_credit$Status.of.existing.checking.accountA11` + 
               `german_credit$Status.of.existing.checking.accountA12` + 
               `german_credit$Credit.historyA30` + `german_credit$Credit.historyA31` + 
               `german_credit$Savings.account.bondsA61`, 
             family = "binomial", data = train)

#check the summary of model
summary(model_10)

#removing Installment.rate.in.percentage.of.disposable.income, as it has two star and has the highest p value among the
#2 stared variables 
#and name the model as model_11

model_11<-glm(formula = train$Default_status ~ Duration.in.month +
                `german_credit$Status.of.existing.checking.accountA11` + 
                `german_credit$Status.of.existing.checking.accountA12` + 
                `german_credit$Credit.historyA30` + `german_credit$Credit.historyA31` + 
                `german_credit$Savings.account.bondsA61`, family = "binomial", 
              data = train)

#check the summary of model
summary(model_11)

#The AIC has not increased a lot so we can still remove some less insignificant variables from the dataset.

#removing `german_credit$Credit.historyA30`, as it has two star and has the highest p value among the
#2 stared variables 
#and name the model as model_12

model_12<-glm(formula = train$Default_status ~ Duration.in.month +
                `german_credit$Status.of.existing.checking.accountA11` + 
                `german_credit$Status.of.existing.checking.accountA12` + 
                `german_credit$Credit.historyA31` + 
                `german_credit$Savings.account.bondsA61`, family = "binomial", 
              data = train)

#check the summary of model
summary(model_12)

#all the variables look significant enough to be kept in the final model for logistic regression.
#rename it to final model

final_model<-model_12

#check the summary of the final model to get the values of AIC , Null deviance and Residual Deviance.

summary(model_12)
#Thus we have an AIC of 720.04
#Null deviance of 839.69
#Residual deviance of 708.04

#-----------------------------------------------------------------------------------------------------------------------------

#Checkpoint 5: Model Evaluation

#-----------------------------------------------------------------------------------------------------------------------------

#Evaluating the model using c statistic using the train dataset
train$predicted_prob = predict(final_model,  type = "response")
rcorr.cens(train$predicted_prob,train$Default_status) 
#1st argument is your vector of predicted probabilities, 2nd observed values of outcome variable
#the outcome being our target variable.

#Thus we get a c statistic as 0.76 for our train data  which is acceptable.

#Evaluating the model using c statistic using the test dataset
test$predicted_prob = predict(final_model,test, type = "response")
rcorr.cens(test$predicted_prob,test$Default_status)
# 1st argument is your vector of predicted probabilities, 2nd observed values of outcome variable

#Thus we get a c statistic as 0.755 for our train data  which is acceptable.

#Thus our model is aa good model.

#now we will check for ks statistics

#creating the prediction 
model_score <- prediction(train$predicted_prob,train$Default_status)

#creating the performance object for the evaluating the performance, which will give us
#the true positive rate and false positive rate and hence the two arguments named tpr and fpr

model_perf <- performance(model_score, "tpr", "fpr")

#the ks table contains the difference between cummulative true positive and
#cummulative false positive rates!
ks_table <- attr(model_perf, "y.values")[[1]] - (attr(model_perf, "x.values")[[1]])

#the maximum of the ks_table will be our ks statistic
ks = max(ks_table)

#thus view our ks statistic

ks

#it is 41 and hence acceptable as ks statistic more than 40 is good

#we can check in which decile our ks statistic lies by finding the row number for ks in the
#ks_table and then dividing by the total number of rows
which(ks_table == ks)/nrow(train)

#thus its 0.11 means it lies in the 2nd decile

#now we will check for ks statistics


#now we will calculate the ks statistic for test dataset.

#creating the prediction for test now
model_score_test <- prediction(test$predicted_prob,test$Default_status)

#creating the performance object for the evaluating the performance, which will give us
#the true positive rate and false positive rate and hence the two arguments named tpr and fpr

model_perf_test <- performance(model_score_test, "tpr", "fpr")

#the ks table contains the difference between cummulative true positive and
#cummulative false positive rates!
ks_table_test <- attr(model_perf_test, "y.values")[[1]] - (attr(model_perf_test, "x.values")[[1]])

#the maximum of the ks_table will be our ks statistic
ks_test = max(ks_table_test)

#thus view our ks statistic

ks_test

#it is 44.9 and hence acceptable as ks statistic more than 40 is good


#-----------------------------------------------------------------------------------------------------------------------------

#Checkpoint 6: Threshold value

#-----------------------------------------------------------------------------------------------------------------------------


#now we need to plot the ROC curve using the performace measure which we generated above and saved it in
#model_pref

#Thus plot model_pref
plot(model_perf)
#The curve lies ahead of the y=x line, which is a good. 
#THus the first sanity check is passed
#Next, we need to know the area under the curve.

#calculating the area under the curve and naming it auc
auc<-performance(model_score, "auc")

#to get the area the following command is used
auc@y.values[[1]]

#Oh Great the area under the curve is 0.76 which is good area under the curve ratio

#now using the ROC curve we will find the suitable probablity,where the TPR is high and FPR is low
#once the threshold is decided all the points above are likely to default and below are
#likelg not to default

confusionMatrix(as.numeric(train$predicted_prob>0.5),train$Default_status)

#Thus the Accurancy of the model is 0.7385 i.e 73.99%
#the Sensitivity of the model is 0.9158
#and the Specificity of the model is 0.3085
