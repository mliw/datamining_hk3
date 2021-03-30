# \section{Problem 3:Predictive model building: California housing}
## Load packages and data
install.packages("ggmap")
install.packages("leaps")
library(ggplot2)
library(tidyverse)
library(ggmap)
library(leaps)
library(scales)
CAhousing <- read.csv("data/CAhousing.csv", stringsAsFactors=TRUE)
summary(CAhousing)


# sampling
set.seed(1234)
train <- sample(nrow(CAhousing),0.7*nrow(CAhousing))
CAhousingtrain <- CAhousing[train,]
CAhousingtest <- CAhousing[-train,]

# Q1 build the best predictive model for medianHouseValue
# model selection
### 因为逐步回归法没办法评测到每一个模型，所以在这里使用全子集回归
### 这里我用了一种比较简洁的方法，如果跟兄弟的最佳模型有区别，我可以根据兄弟的模型修改后面的绘图代码
# using all-subsets regression to choose the best model

leaps <-regsubsets( medianHouseValue ~  longitude + latitude +	housingMedianAge + totalRooms + totalBedrooms + population + households + medianIncome, data=CAhousingtrain, nbest=8)
plot(leaps, scale="adjr2")
# As we can see in the plots, the model with all 8 variables has the best R^2, which is equal to 0.65. So the best predictive model is longitude + latitude +	housingMedianAge + totalRooms + totalBedrooms + population + households + medianIncome

# final model
fit <- lm( medianHouseValue ~  longitude + latitude +	housingMedianAge + totalRooms + totalBedrooms + population + households + medianIncome, data=CAhousingtrain)

summary(fit)
coef(fit)
coef(fit) %>% round (3)

# predict the model
test_actual = CAhousingtest$medianHouseValue
test_predictions = predict(fit, CAhousingtest)

# get maps
califonia <- c(left = -125, bottom = 32, right = -113, top = 42)
map <- get_stamenmap(califonia, zoom = 9, maptype = "watercolor")
ggmap(map)

# figure 1:a plot of the original data, using a color scale to show medianHouseValue versus longitude (x) and latitude (y).
head(CAhousing[c('longitude','latitude','medianHouseValue')])
### figure1第一种展现形式是将population作为size大小的变量，可能更好看一些。                                                                                               data=data0[data0$Category %in% Top3,],alpha=1)+labs(x='Longitude',y='Latitude')
plot_map11 = ggmap(map, base_layer = ggplot(CAhousing, 
          aes(x = longitude, y = latitude, color = medianHouseValue 
          ))) +
  geom_point(aes(size = population), alpha = 0.4) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("medianHouseValue versus longitude and latitude(original data)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_distiller(palette = "Paired",labels=comma) +
  labs(color = "Median House Value", size = "Population")

plot_map11

# figure1第二种展现形式是按照题目要求只有median house value 和经纬度，比较直观。
plot_map12 = ggmap(map, base_layer = ggplot(CAhousing, 
             aes(x = longitude, y = latitude, color = medianHouseValue 
              ))) +
  geom_point(alpha = 0.4) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("medianHouseValue versus longitude and latitude(original data)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_distiller(palette = "Paired",labels=comma) +
  labs(color = "Median House Value")

plot_map12

### figure1 兄弟可以二选一

# figure 2:a plot of your model's predictions of medianHouseValue versus longitude (x) and latitude (y).
plot_map2 = ggmap(map, base_layer = ggplot(CAhousingtest, 
            aes(x = longitude, y = latitude, color = test_predictions))) +
  geom_point(alpha = 0.4) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("medianHouseValue versus longitude and latitude(prediction)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_distiller(palette = "Paired",labels=comma) +
  labs(color = "test_prediction")

plot_map2

# figure 3:a plot of your model's errors/residuals versus longitude (x) and latitude (y)


plot_map3 = ggmap(map, base_layer = ggplot(CAhousingtest, 
            aes(x = longitude, y = latitude, color = test_predictions - test_actual ))) +
  geom_point(aes(size = abs(test_predictions - test_actual)),alpha = 0.4) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("residuals versus longitude and latitude") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_distiller(palette = "Paired",labels=comma) +
  labs(color = "residuals", size = "Magnitude of Price Difference")

plot_map3
