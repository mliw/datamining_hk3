# \section{Problem 3:Predictive model building: California housing}
## Load packages and data
library(ggplot2)
library(tidyverse)
library(ggmap)
library(leaps)
library(xgboost)
library(scales)
library(ModelMetrics)
housing = read.csv("data/CAhousing.csv", stringsAsFactors=TRUE)
housing$mean_rooms = housing$totalRooms/housing$households
housing$mean_bedrooms = housing$totalBedrooms/housing$households

#Get x_name_total and y_name
total_name = colnames(housing)
x_name_total = setdiff(total_name,c("medianHouseValue"))
y_name = c("medianHouseValue")

#Feature Importance:
X = as.matrix(housing[c(x_name_total)])
Y = as.matrix(housing[y_name])
dtrain = xgb.DMatrix(X, label = Y)
bst = xgb.train(data = dtrain, verbose = FALSE,nrounds = 50, nthread = 2,
                max_depth = 7 , eta = 0.5, objective = "reg:squarederror")
importance_matrix = xgb.importance(model = bst)
xgb.plot.importance(importance_matrix = importance_matrix)

#Function for evaluation:
evaluate_xgb=function(original_data,x_name,y_name){
  rmse_result = Inf
  dep_result = NA
  et_result = NA
  X = as.matrix(original_data[x_name])
  Y = as.matrix(original_data[y_name])
  dtrain = xgb.DMatrix(X, label = Y)
  for (dep in c(3,5,7,9,11)){
    for(et in c(0.25)){
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
  result = evaluate_xgb(housing,tem_features,y_name)
  result_collect = rbind(result_collect,result)
  print(i)
}
result_collect = data.frame(result_collect)
colnames(result_collect) = c("features","max_depth","eta(learning rate)","cv-rmse")
result_collect$features = paste("top",1:dim(result_collect)[1],"features")
write.csv(result_collect,"data/result_collect.csv")
# top 3 features have the lowest cv-rmse

#  Find the best model top 3 features max_depth=11,eta=0.25 cv-rmse=47500.57578
i = 3
tem_features = importance_matrix$Feature[1:i]
X = as.matrix(housing[tem_features])
Y = as.matrix(housing[y_name])
dtrain = xgb.DMatrix(X, label = Y)
model = xgb.train(data = dtrain, verbose = FALSE,nrounds = 50, nthread = 2,
                  max_depth = 11 , eta = 0.25, objective = "reg:squarederror")
prediction = predict(model, dtrain)
rmse(Y,prediction)

#################################################################
# Start plotting
#################################################################
califonia <- c(left = -125, bottom = 32, right = -113, top = 42)
map <- get_stamenmap(califonia, zoom = 9, maptype = "watercolor")
ggmap(map)

# figure 1:a plot of the original data, using a color scale to show medianHouseValue versus longitude (x) and latitude (y).
fig1_realmap=qmplot(alpha=0.01,longitude, latitude,color=medianHouseValue,data=housing)+
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Median HouseValue at Different Tracts in California(real map)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_colour_gradient(low = "yellow", high = "red")+
  labs(color = "Median House Value")
fig1_realmap

fig1_normal = ggplot(housing)+
  geom_point(alpha = 0.05,aes(x = longitude, y = latitude, color = medianHouseValue))+
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Median HouseValue at Different Tracts in California") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_colour_gradient(low = "yellow", high = "red")+
  labs(color = "Median House Value")
fig1_normal


# figure 2:a plot of your model's predictions of medianHouseValue versus longitude (x) and latitude (y).
housing$medianHouseValue_prediction = prediction
fig2_realmap=qmplot(alpha=0.01,longitude, latitude,color=medianHouseValue_prediction,data=housing)+
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Median HouseValue Prediction at Different Tracts in California(real map)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_colour_gradient(low = "yellow", high = "red")+
  labs(color = "Median House Value Prediction")
fig2_realmap

fig2_normal = ggplot(housing)+
  geom_point(alpha = 0.05,aes(x = longitude, y = latitude, color = medianHouseValue_prediction))+
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Median HouseValue Prediction at Different Tracts in California") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_colour_gradient(low = "yellow", high = "red")+
  labs(color = "Median House Value Prediction")
fig2_normal

# figure 3:a plot of your model's errors/residuals versus longitude (x) and latitude (y)
housing$error = abs(Y-prediction)
fig3_realmap=qmplot(alpha=0.0001,longitude, latitude,color=error,data=housing)+
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Prediction Error at Different Tracts in California(real map)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_colour_gradient(low = "yellow", high = "red")+
  labs(color = "Prediction Error")
fig3_realmap

fig3_normal = ggplot(housing)+
  geom_point(alpha = 0.05,aes(x = longitude, y = latitude, color = error))+
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Prediction Error at Different Tracts in California") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_colour_gradient(low = "yellow", high = "red")+
  labs(color = "Prediction Error")
fig3_normal
