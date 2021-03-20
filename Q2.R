# \section{Predictive model building: green certification}

### Load the data
library(ggplot2)
library(tidyverse)
original_data = read.csv("data/greenbuildings.csv")
original_data$revenue_per_square = (original_data$leasing_rate/100)*original_data$Rent
# > colnames(original_data)
# [1] "CS_PropertyID"     "cluster"           "size"              "empl_gr"          
# [5] "Rent"              "leasing_rate"      "stories"           "age"              
# [9] "renovated"         "class_a"           "class_b"           "LEED"             
# [13] "Energystar"        "green_rating"      "net"               "amenities"        
# [17] "cd_total_07"       "hd_total07"        "total_dd_07"       "Precipitation"    
# [21] "Gas_Costs"         "Electricity_Costs" "City_Market_Rent" 

# Your goal is to build the best predictive model possible for revenue per square 
# foot per calendar year, and to use this model to quantify the average change in 
# rental income per square foot (whether in absolute or percentage terms) associated 
# with green certification, holding other features of the building constant. 

# We have 2 goals
# (1) build the best predictive model possible for revenue per square 
# foot per calendar year

# Xgboost is used to model
library(xgboost)
x_name = colnames(original_data)[1:(dim(original_data)[2]-1)]
y_name = colnames(original_data)[dim(original_data)[2]]











