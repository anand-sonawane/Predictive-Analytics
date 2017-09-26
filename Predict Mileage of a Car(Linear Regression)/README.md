# Predict-Mileage-of-a-Car

# Problem Statement

  The automobile market has become very dynamic as the buyers have varied preferences. Customers look for various features (brand value, mileage, model_year etc) in their dream car. In order to fulfil it's customer requirement, Mycar Dream wants to automate the process of predicting the car mileage which fits the customer preferences, based on the dataset of car features and attributes obtained by market surveys.

# Company Information

  An automobile consultancy firm “Mycar Dream” provides assistance to its clients in making appropriate car deals, based on their requirements.
 
  Based on various market surveys, the firm has gathered a large dataset of different types of cars and their attributes across the world. The business model of the company is solely based on consumer interest, aiming to provide the most appropriate car to their clients and hence maximise the customer satisfaction.
  
# Dataset 

The data set contains the following details about cars: 

  The aim here is to predict the city-cycle fuel consumption in miles per gallon, in terms of 3 multivalued discrete and 5 continuous attributes. Below is the data dictionary to be used for the dataset.
  
Data Dictionary:

1	Mpg		                  Mileage per gallon (continuous variable)
2	Cylinders	              Number of cylinders in car (multi-valued discrete)
3	Displacement	          Volume of fuel inside the engine i.e size of engine (continuous )
4	Horsepower	            picks up of the car (continuous)
5	Weight		              Weight of car (continuous)
6	Acceleration	          Acceleration of car (continuous)
7	Model year	            Year when the car launched (multi-valued discrete)
8	Origin		              Origin of car (multi-valued discrete)
9	Car name	              Name of car company (unique for each instance)


# Goal

You are required to develop a predictive model which can follow these three constraints thoroughly: 

1) The model should not contain more than 5 variables.
2) According to the business needs, set the VIF to 2. 
3) The model should be highly predictive in nature i.e it should show 80% (R squared) of accuracy.

