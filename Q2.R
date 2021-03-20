# \section{Predictive model building: green certification}

### Load the data
library(ggplot2)
library(xgboost)
library(tidyverse)
original_data = read.csv("data/greenbuildings.csv")
original_data$revenue_per_square = (original_data$leasing_rate/100)*original_data$Rent
# Your goal is to build the best predictive model possible for revenue per square 
# foot per calendar year, and to use this model to quantify the average change in 
# rental income per square foot (whether in absolute or percentage terms) associated 
# with green certification, holding other features of the building constant. 
# We have 2 goals
# (1) build the best predictive model possible for revenue per square 
# foot per calendar year

# Remove features not nacessary
# CS_PropertyID is just ID leasing_rate and rent information leakage
total_name = colnames(original_data)
valid_name = setdiff(total_name,c("CS_PropertyID","leasing_rate","Rent","Energystar","LEED"))
# green_rating must be included in the features.
x_name_total = setdiff(valid_name,c("revenue_per_square"))
y_name = c("revenue_per_square")

### Feature Importance:
X = as.matrix(original_data[c(x_name_total)])
Y = as.matrix(original_data[y_name])
dtrain = xgb.DMatrix(X, label = Y)
bst = xgb.train(data = dtrain, verbose = FALSE,nrounds = 50, nthread = 2,
                max_depth = 7 , eta = 0.5, objective = "reg:squarederror")
importance_matrix = xgb.importance(model = bst)
xgb.plot.importance(importance_matrix = importance_matrix)

## Function for evaluation:
evaluate_xgb=function(original_data,x_name,y_name){
rmse_result = Inf
dep_result = NA
et_result = NA
X = as.matrix(original_data[x_name])
Y = as.matrix(original_data[y_name])
dtrain = xgb.DMatrix(X, label = Y)
for (dep in c(3,5,7,9,11)){
  for(et in c(0.1,0.25,0.5)){
    set.seed(100)
    cv = xgb.cv(data = dtrain, verbose = FALSE,nrounds = 50, nthread = 2, nfold = 5, metrics = list("rmse"),
                max_depth = dep , eta = et, objective = "reg:squarederror")
    evaluation_matrix = cv[["evaluation_log"]]
    test_rmse_mean = as.numeric(evaluation_matrix[dim(evaluation_matrix)[1],"test_rmse_mean"])
    if (test_rmse_mean<rmse_result){
      rmse_result = test_rmse_mean
      dep_result = dep
      et_result = et
    }
  }
}
return(c(paste(x_name, collapse = '+'),dep_result,et_result,rmse_result))
}

result_collect = c()
for (i in 1:dim(importance_matrix)[1]){
  tem_features = importance_matrix$Feature[1:i]
  tem_features = union(tem_features,c("green_rating"))
  result = evaluate_xgb(original_data,tem_features,y_name)
  result_collect = rbind(result_collect,result)
  print(i)
}
result_collect = data.frame(result_collect)
colnames(result_collect) = c("features","max_depth","eta(learning rate)","cv-rmse")
