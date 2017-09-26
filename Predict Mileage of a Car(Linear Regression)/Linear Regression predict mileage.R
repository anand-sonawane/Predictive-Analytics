#loading the dataset into R 
carmileage<-read.csv("carMPG.csv",stringsAsFactors = F)

#install the required packages for the assignment
install.packages("MASS")
install.packages("car")

#load the above packages installed
library("MASS")
library("car")

#See the carmileage dataset and see its structure
View(carmileage)
str(carmileage)
#--------------------------------------------------------------------------------------------------------
#Checkpoint 1: Business Understanding and Data Understanding
#we look at the data well and understand that for perdicting the mileage of the car
#the variables that can be of significance are to be selected from the 9 variables that
#we have in the dataset. Also using the 398 observations that we have with us. Out of the
#9 variables 3 are of factor type and 5 are of continous type means numerical variables
#and the last variable is just the name and has no significance in model building.

#--------------------------------------------------------------------------------------------------------
#Checkpoint 2: Data Cleaning and Preparation

#First stage: Variables Formatting  

#we need to ensure that all the variables are in the right format so we see the structure
#of the dataset
str(carmileage)
#we understand from the business objective that the right format for each variable is as
#follows:
# MPG: num (our main variable)
# cylinders: factor
# Displacement: num
# Horsepower: num
# Weight: num
# Acceleration: num
# model-year: factor
# Origin: factor
# Car name: character
# from the structure we observe that we need to convert the following:
# cylinders into factors
# horsepower into num
# weight into num
# model-year into factor
# Origin into factor

# so we convert the above

# cylinders into factors
carmileage$Cylinders<-as.factor(carmileage$Cylinders)

# horsepower into num
carmileage$Horsepower<-as.numeric(carmileage$Horsepower)
# you will get an warning saying the Na's are introduced that is due to values 
#present earlier as "?"

# weight into num
carmileage$Weight<-as.numeric(carmileage$Weight)

# model-year into factor
carmileage$Model_year<-as.factor(carmileage$Model_year)

# Origin into factor
carmileage$Origin<-as.factor(carmileage$Origin)

#check the structure of the dataset again
str(carmileage)

#Second Stage: Data Cleaning 

#now we check for duplicated rows:
x`
unique(carmileage)
# There are 398 unique rows indicating that there are no duplicated rows in the dataset,
# because the total rows are also 398

#now we need to check for missing values if any in the dataset
sum(is.na(carmileage))
#OH! there are 6 missing values, we will now check column wise to remove them.

#first as have had a warning before for Horsepower we will check for it

sum(is.na(carmileage$Horsepower))
#Yes the 6 missing values are in the Horsepower column so we remove them as the 
#number of missing values is very less compared to the total number of observations.

carmileage<-carmileage[which(is.na(carmileage$Horsepower)==FALSE),]

#we check for missing values in the whole dataset
sum(is.na(carmileage))
#yes the answer is 0, so we get a dataset free from all the NA's

#now we will do outlier treatment for all the continous variables
quantile(carmileage$MPG,seq(0,1,0.01))
#here from the output we check if there is a sudden change in the variable value anywhere
#we observe that there is a sudden change at the 98 to 99 percetile so we decide to
#cap it
carmileage$MPG[which(carmileage$MPG>39.652)]<-39.652

#similary for the other variables
quantile(carmileage$Displacement,seq(0,1,0.01))
#no outliers in Displacement

quantile(carmileage$Horsepower,seq(0,1,0.01))
#no outliers in Horsepower

quantile(carmileage$Weight,seq(0,1,0.01))
#no outliers in Weight

quantile(carmileage$Acceleration,seq(0,1,0.01))
#no outliers in Acceleration

#Third Stage: Variables Transformation

#for car-name we will make a factor variable by using the car company name only
#so for that we need to manipulate the strings of car_name column
#done as follows:

carmileage$Car_Name<-gsub("([A-Za-z]+).*", "\\1", carmileage$Car_Name)
#converting into factors
carmileage$Car_Name<-as.factor(carmileage$Car_Name)

#checks the levels
levels(carmileage$Car_Name)
#we observe that the levels vokswagen, volkswagen and vw are the same car company so 
#convert them into 1 factor

#converting vokswagen into volkswagen
carmileage$Car_Name[which(carmileage$Car_Name=="vokswagen")]<-"volkswagen"
carmileage$Car_Name[which(carmileage$Car_Name=="vw")]<-"volkswagen"

#we will perfrom binning for model_year as a specific year wont matter that much . We will first
#see the model_year column structure.

str(carmileage$Model_year)
#it is a factor variable with 13 levels.

#see the summary to gain more insigths in it.
summary(carmileage$Model_year)

#the data is evenly distributed along year 2003 to year 2015, so we will bin it into 3
#values namely new, moderate and old meaning
#new means new car of year 2011-2015
#moderate means moderately aged car of year 2007-2010
#old means old car of year 2003 to 2006

#get the levels in the index vector
index<-levels(carmileage$Model_year)

#check the index
index

#now get the values which we want to substitute the model_year with 
#as per the data written above
values<-c("old","old","old","old","moderate","moderate","moderate","moderate","new","new","new","new","new")

#now check if values and levels have same number of entries
length(index)
length(values)
#Yes! they have same number of entries, now substitute.
#use the match function for that

carmileage$Model_year<-values[match(carmileage$Model_year,index)]

#now check structure and summary of model_year
str(carmileage$Model_year)
summary(carmileage$Model_year)

#it is character: convert into factor

carmileage$Model_year<-as.factor(carmileage$Model_year)

#now again check structure and summary of model_year
str(carmileage$Model_year)
summary(carmileage$Model_year)

#creatingg dummy variables
dummy_cylinders<-model.matrix(~carmileage$Cylinders -1, data=carmileage)
dummy_cylinders<-dummy_cylinders[,-1]

dummy_model_year<-model.matrix(~carmileage$Model_year -1, data=carmileage)
dummy_model_year<-dummy_model_year[,-1]

dummy_origin<-model.matrix(~carmileage$Origin -1, data=carmileage)
dummy_origin<-dummy_origin[,-1]

dummy_carname<-model.matrix(~carmileage$Car_Name -1, data=carmileage)
dummy_carname<-dummy_carname[,-1]

#cbind the dummies into 1 dummy
dummy<-cbind(dummy_cylinders,dummy_model_year,dummy_origin,dummy_carname)

#now remove the categorical variables and add the dummy in that place
carmileage_1<-cbind(carmileage[c(1,3,4,5,6)],dummy)

#now we need to divide the dataset into train and test dataset.


# Dividing the data into 70 train and 30 test respectively and we are settiig the seed to 100

set.seed(100)
indices= sample(1:nrow(carmileage_1), 0.7*nrow(carmileage_1))

train=carmileage_1[indices,]
test = carmileage_1[-indices,]

#--------------------------------------------------------------------------------------------------------
#Checkpoint 3: Model Development

#Look at the structure of the train dataset

str(train)
#its correct with all our values.

#now our dependent variable is MPG that is the mileage of the car, therefore building a model for it

model_1<-lm(train$MPG~.,train)

#now lets see the summary of the model

summary(model_1)

#now for optimisin the model , lets remove the insignificant variables from the model 
#we will start doing it using the stepwise approach

step <- stepAIC(model_1, direction="both")

#now we will run the step object to see the model we will be building that will remove the extremely
#insignificant variables
step

#make a model after removing the insignificant variables and store it in model_2

model_2<-lm(formula = train$MPG ~ Horsepower + Weight + `carmileage$Cylinders4` + 
              `carmileage$Cylinders5` + `carmileage$Cylinders6` + `carmileage$Cylinders8` + 
              `carmileage$Model_yearnew` + `carmileage$Model_yearold` + 
              `carmileage$Car_Namedatsun` + `carmileage$Car_Namefiat` + 
              `carmileage$Car_Namehonda` + `carmileage$Car_Namemazda` + 
              `carmileage$Car_Namenissan` + `carmileage$Car_Nameoldsmobile` + 
              `carmileage$Car_Namepontiac` + `carmileage$Car_Namerenault` + 
              `carmileage$Car_Nametoyota` + `carmileage$Car_Nametriumph` + 
              `carmileage$Car_Namevolkswagen` + `carmileage$Car_Namevolvo` + 
              `carmileage$Car_Nameford` + Acceleration, data = train)

#see the summary of the new model built named model_2
summary(model_2)

#we can observe that the adjusted R-squared value has improved a bit but there are still insignificant
#variables in the model.

#now we will use then VIF function to remove the multicollinearity in the model and then remove such
#variables

#AS per the business objective we are going to consider only the variables which have the VIF less than 2

#Lets start!
#Apply vif to the model_2

vif(model_2)

#now look at the summary of the model to check the significance of the variables.
#we will remove the variables based on both the vif value and the significance or the p-value

summary(model_2)

#the vif is highest for cylinder4 followed by cylinder8 then cylinder6 then horsepower,weight,acceleration,cylinder5
#significance wise cylinders have high significance so we wont remove them and directly move to
#removing acceleration.

model_3<-lm(formula = train$MPG ~ Horsepower + Weight + `carmileage$Cylinders4` + 
              `carmileage$Cylinders5` + `carmileage$Cylinders6` + `carmileage$Cylinders8` + 
              `carmileage$Model_yearnew` + `carmileage$Model_yearold` + 
              `carmileage$Car_Namedatsun` + `carmileage$Car_Namefiat` + 
              `carmileage$Car_Namehonda` + `carmileage$Car_Namemazda` + 
              `carmileage$Car_Namenissan` + `carmileage$Car_Nameoldsmobile` + 
              `carmileage$Car_Namepontiac` + `carmileage$Car_Namerenault` + 
              `carmileage$Car_Nametoyota` + `carmileage$Car_Nametriumph` + 
              `carmileage$Car_Namevolkswagen` + `carmileage$Car_Namevolvo` + 
              `carmileage$Car_Nameford`, data = train)
#check vif of the new model
vif(model_3)
#check summary of the new model
summary(model_3)

#now based on the summary and vif we will remove the horsepower variable and name the new model as model_4

model_4<-lm(formula = train$MPG ~ Weight + `carmileage$Cylinders4` + 
              `carmileage$Cylinders5` + `carmileage$Cylinders6` + `carmileage$Cylinders8` + 
              `carmileage$Model_yearnew` + `carmileage$Model_yearold` + 
              `carmileage$Car_Namedatsun` + `carmileage$Car_Namefiat` + 
              `carmileage$Car_Namehonda` + `carmileage$Car_Namemazda` + 
              `carmileage$Car_Namenissan` + `carmileage$Car_Nameoldsmobile` + 
              `carmileage$Car_Namepontiac` + `carmileage$Car_Namerenault` + 
              `carmileage$Car_Nametoyota` + `carmileage$Car_Nametriumph` + 
              `carmileage$Car_Namevolkswagen` + `carmileage$Car_Namevolvo` + 
              `carmileage$Car_Nameford`, data = train)

#check vif and summary
vif(model_4)
summary(model_4)

#we observe that the R-squared value is not affected much, the next variable to be remove is
#cylinder6 because it has high vif of 20.098546 and has 2 star significance.

#make a new model model_5 removing the variable cylinder6

model_5<-lm(formula = train$MPG ~ Weight + `carmileage$Cylinders4` + `carmileage$Cylinders5` + 
               `carmileage$Cylinders8` + `carmileage$Model_yearnew` + 
              `carmileage$Model_yearold` + `carmileage$Car_Namedatsun` + 
              `carmileage$Car_Namefiat` + `carmileage$Car_Namehonda` + 
              `carmileage$Car_Namemazda` + `carmileage$Car_Namenissan` + 
              `carmileage$Car_Nameoldsmobile` + `carmileage$Car_Namepontiac` + 
              `carmileage$Car_Namerenault` + `carmileage$Car_Nametoyota` + 
              `carmileage$Car_Nametriumph` + `carmileage$Car_Namevolkswagen` + 
              `carmileage$Car_Namevolvo` + `carmileage$Car_Nameford`, data = train)

#lets check the vif and summry of the model_5

vif(model_5)
summary(model_5)

#we now observe that significane of cylinder8 has reduced drastically and also its vif value,
#so it must have had some corelation with cylinder6. now lets remove cylinder8 because of its 
#vif greater than 2 and no star significance in the model
#observe that the model is still at a high accuracy of more about 87 percent

#lets make the new model and call it model_6 and remove cylinder8 from it

model_6<-lm(formula = train$MPG ~ Weight + `carmileage$Cylinders4` + `carmileage$Cylinders5` + 
             `carmileage$Model_yearnew` + `carmileage$Model_yearold` + 
              `carmileage$Car_Namedatsun` + `carmileage$Car_Namefiat` + 
              `carmileage$Car_Namehonda` + `carmileage$Car_Namemazda` + 
              `carmileage$Car_Namenissan` + `carmileage$Car_Nameoldsmobile` + 
              `carmileage$Car_Namepontiac` + `carmileage$Car_Namerenault` + 
              `carmileage$Car_Nametoyota` + `carmileage$Car_Nametriumph` + 
              `carmileage$Car_Namevolkswagen` + `carmileage$Car_Namevolvo` + 
              `carmileage$Car_Nameford`, data = train)

#check the vif and summary of the new model i.e model_6

vif(model_6)
summary(model_6)

#now here we have almost all the variables under the vif of 2 except the weight and cylinder4
#if you would have seen they are constantly at high vif and almost same
#there is a probablity that they may be corealted to each other so lets see their correlation

cor(train$Weight,train$`carmileage$Cylinders4`)
#wow they are negatively corelated with a corelation of about 79.9 percent approx 80 percent
#thus keeping them both is not needed and can negatively affect our model so remove the more
#insignificant one from them 

#checking the p values we observe that cylinder4 is more insignificant comparitevely so we
#decide to remove it

#name the new model as model_7

model_7<-lm(formula = train$MPG ~ Weight + `carmileage$Cylinders5` + 
              `carmileage$Model_yearnew` + `carmileage$Model_yearold` + 
              `carmileage$Car_Namedatsun` + `carmileage$Car_Namefiat` + 
              `carmileage$Car_Namehonda` + `carmileage$Car_Namemazda` + 
              `carmileage$Car_Namenissan` + `carmileage$Car_Nameoldsmobile` + 
              `carmileage$Car_Namepontiac` + `carmileage$Car_Namerenault` + 
              `carmileage$Car_Nametoyota` + `carmileage$Car_Nametriumph` + 
              `carmileage$Car_Namevolkswagen` + `carmileage$Car_Namevolvo` + 
              `carmileage$Car_Nameford`, data = train)

#now check vif and the summary of the new model

vif(model_7)
summary(model_7)

#now we have all the vif values less than 2 and thus have successfully removed the multicollinearity
#from the model and now we will check for p values and remove the values which are insignificant
#so that we improve the model more, optimising the model.This will increase the R-squared value
#and we will also move to our objective which is to reduce the model to variables less than 5.

#now we see that the significance of car_name mazda is the least from all the variables as its
#p_value is more so remove it.

model_8<-lm(formula = train$MPG ~ Weight + `carmileage$Cylinders5` + `carmileage$Model_yearnew` + 
              `carmileage$Model_yearold` + `carmileage$Car_Namedatsun` + 
              `carmileage$Car_Namefiat` + `carmileage$Car_Namehonda` + 
               `carmileage$Car_Namenissan` + 
              `carmileage$Car_Nameoldsmobile` + `carmileage$Car_Namepontiac` + 
              `carmileage$Car_Namerenault` + `carmileage$Car_Nametoyota` + 
              `carmileage$Car_Nametriumph` + `carmileage$Car_Namevolkswagen` + 
              `carmileage$Car_Namevolvo` + `carmileage$Car_Nameford`, data = train)

vif(model_8)
summary(model_8)

#so the vif is constantly less and we will top seeing it after this step
#check the summary

#now we see that the significance of car_name volvo is the least from all the variables as its
#p_value is more so remove it.

model_9<-lm(formula = train$MPG ~ Weight + `carmileage$Cylinders5` + `carmileage$Model_yearnew` + 
              `carmileage$Model_yearold` + `carmileage$Car_Namedatsun` + 
              `carmileage$Car_Namefiat` + `carmileage$Car_Namehonda` + 
              `carmileage$Car_Namenissan` + `carmileage$Car_Nameoldsmobile` + 
              `carmileage$Car_Namepontiac` + `carmileage$Car_Namerenault` + 
              `carmileage$Car_Nametoyota` + `carmileage$Car_Nametriumph` + 
              `carmileage$Car_Namevolkswagen` +
              `carmileage$Car_Nameford`, data = train)

#checking the summary
summary(model_9)

#now we see that the significance of car_name nissan is the least from all the variables as its
#p_value is more so remove it.

model_10<-lm(formula = train$MPG ~ Weight + `carmileage$Cylinders5` + `carmileage$Model_yearnew` + 
               `carmileage$Model_yearold` + `carmileage$Car_Namedatsun` + 
               `carmileage$Car_Namefiat` + `carmileage$Car_Namehonda` + 
                `carmileage$Car_Nameoldsmobile` + 
               `carmileage$Car_Namepontiac` + `carmileage$Car_Namerenault` + 
               `carmileage$Car_Nametoyota` + `carmileage$Car_Nametriumph` + 
               `carmileage$Car_Namevolkswagen` + `carmileage$Car_Nameford`, 
             data = train)

#checking the summary
summary(model_10)

#we are keeping an eye on the R-sqaured value and it is almost constant at 85 percent.

#now we see that the significance of car_name oldsmobile is the least from all the variables as its
#p_value is more so remove it.

model_11<-lm(formula = train$MPG ~ Weight + `carmileage$Cylinders5` + `carmileage$Model_yearnew` + 
               `carmileage$Model_yearold` + `carmileage$Car_Namedatsun` + 
               `carmileage$Car_Namefiat` + `carmileage$Car_Namehonda` + 
                `carmileage$Car_Namepontiac` + 
               `carmileage$Car_Namerenault` + `carmileage$Car_Nametoyota` + 
               `carmileage$Car_Nametriumph` + `carmileage$Car_Namevolkswagen` + 
               `carmileage$Car_Nameford`, data = train)

#checking the summary
summary(model_11)


#now we remove cylinder5 as it has the least significance and the highest p value

model_12<-lm(formula = train$MPG ~ Weight + `carmileage$Model_yearnew` + 
               `carmileage$Model_yearold` + `carmileage$Car_Namedatsun` + 
               `carmileage$Car_Namefiat` + `carmileage$Car_Namehonda` + 
               `carmileage$Car_Namepontiac` + `carmileage$Car_Namerenault` + 
               `carmileage$Car_Nametoyota` + `carmileage$Car_Nametriumph` + 
               `carmileage$Car_Namevolkswagen` + `carmileage$Car_Nameford`, 
             data = train)

#checking the summary
summary(model_12)

#Adjusted R-sqaured stands good at 0.8501

#next to be removed is carname triumph

model_13<-lm(formula = train$MPG ~ Weight + `carmileage$Model_yearnew` + 
               `carmileage$Model_yearold` + `carmileage$Car_Namedatsun` + 
               `carmileage$Car_Namefiat` + `carmileage$Car_Namehonda` + 
               `carmileage$Car_Namepontiac` + `carmileage$Car_Namerenault` + 
               `carmileage$Car_Nametoyota` +
               `carmileage$Car_Namevolkswagen` + `carmileage$Car_Nameford`, 
             data = train)
#checking summary
summary(model_13)

#next to be removed is car name ford

model_14<-lm(formula = train$MPG ~ Weight + `carmileage$Model_yearnew` + 
               `carmileage$Model_yearold` + `carmileage$Car_Namedatsun` + 
               `carmileage$Car_Namefiat` + `carmileage$Car_Namehonda` + 
               `carmileage$Car_Namepontiac` + `carmileage$Car_Namerenault` + 
               `carmileage$Car_Nametoyota` + `carmileage$Car_Namevolkswagen`  
            , data = train)

#checking the summary
summary(model_14)

#we still have 10 varaibles left and as we remove them the accuracy will defintely decrease, we can
#reduce them till 80 percent accuracy and 5 variables at the most.

#now remove car name pontiac 

model_15<-lm(formula = train$MPG ~ Weight + `carmileage$Model_yearnew` + 
               `carmileage$Model_yearold` + `carmileage$Car_Namedatsun` + 
               `carmileage$Car_Namefiat` + `carmileage$Car_Namehonda` + 
                `carmileage$Car_Namerenault` + 
               `carmileage$Car_Nametoyota` + `carmileage$Car_Namevolkswagen`, 
             data = train)

#checking the summary
summary(model_15)

#now remove car name toyota

model_16<-lm(formula = train$MPG ~ Weight + `carmileage$Model_yearnew` + 
               `carmileage$Model_yearold` + `carmileage$Car_Namedatsun` + 
               `carmileage$Car_Namefiat` + `carmileage$Car_Namehonda` + 
               `carmileage$Car_Namerenault` +
               `carmileage$Car_Namevolkswagen`, data = train)

#checking the summary
summary(model_16)

#now removing car name renault
#these deletions are based on the p value of the variables

model_17<-lm(formula = train$MPG ~ Weight + `carmileage$Model_yearnew` + 
               `carmileage$Model_yearold` + `carmileage$Car_Namedatsun` + 
               `carmileage$Car_Namefiat` + `carmileage$Car_Namehonda` + 
               `carmileage$Car_Namevolkswagen`, data = train)

#checking the summary
summary(model_17)

#now removing car name fiat

model_18<-lm(formula = train$MPG ~ Weight + `carmileage$Model_yearnew` + 
               `carmileage$Model_yearold` + `carmileage$Car_Namedatsun` + 
               `carmileage$Car_Namehonda` + 
               `carmileage$Car_Namevolkswagen`, data = train)

#checking the summary
summary(model_18)

#now we are removinf car name honda after which we will be left with just 5 variables in our model
model_19<-lm(formula = train$MPG ~ Weight + `carmileage$Model_yearnew` + 
               `carmileage$Model_yearold` + `carmileage$Car_Namedatsun` + 
               `carmileage$Car_Namevolkswagen`, 
             data = train)

#checking the summary
summary(model_19)

#now we observe that carname datsun is significant but the model can be reduced further,maybe
#lets check

#remove car name datsun

model_20<-lm(formula = train$MPG ~ Weight + `carmileage$Model_yearnew` + 
               `carmileage$Model_yearold` +
               `carmileage$Car_Namevolkswagen`, data = train)

#check the model
summary(model_20)

#try removing car name volkswagen from the model and see its effect

model_21<-lm(formula = train$MPG ~ Weight + `carmileage$Model_yearnew` + 
                 `carmileage$Model_yearold` , 
               data = train)

#check the model
summary(model_21)

#the R-square value stands good at 0.8252 so we are high on accuracy still
#next to be removed via the p value scale is model year new

model_22<-lm(formula = train$MPG ~ Weight + 
               `carmileage$Model_yearold`, data = train)

#check the model
summary(model_22)

#lastly once check the vif values for this final model.
vif(model_22)

#nice both variables have a VIF of less than 2

#this is our final model with Adjusted R-squared of 0.8163 and 2 variables namely Weight and Model_yearOld

#we name it model_final
model_final<-model_22

#--------------------------------------------------------------------------------------------------------
#Checkpoint 4: Model Evaluation and Testing 

#now we will test our model which is built

#run predict to get predicted values using our final model

Predict<-predict(model_final,test[,-1])

#R-squared is defined as the square of correlation between actual and predicted values of the variable.

#so we calculate that 
R_squared<-(cor(test$MPG,Predict))^2

#view the R-sqaured value
R_squared

#Nice! we have a R-sqaured value of 0.8366 and thus all our business objectives are successful,
#we have a model with accuracy of about 83.66%

#--------------------------------------------------------------------------------------------------------
#Checkpoint 5: Model acceptance or Rejection

#goal 1: The model should not contain more than 5 variables.

#our final model has 2 varibles

#goal 2: The model should be highly predictive in nature i.e it should show 80% (R squared) of accuracy.

#our final model has an accuracy of 81.63%

#goal 3: The model should give high accuracy (test R-squared ) when tested it on the test dataset.

#our final model has an accuracy of 83.66%
