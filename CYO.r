library(tidyverse)
library(dplyr)
library(caret)
data <- read_csv('https://raw.githubusercontent.com/Luis-M-Garcia/BMW/main/bmw_pricing_challenge.csv')
names(data)
bmw <- data %>% select(engine_power,fuel,paint_color,car_type,feature_1,feature_2,feature_3,feature_4,feature_4,feature_5,feature_6,feature_7,feature_8,price)
index <- createDataPartition(y = bmw$price, times = 1, p = 0.1, list = FALSE)
train_set <- bmw[-index,]
test_set <- bmw[index,]
names(train_set)
head(data)
#scale the price column to avoid huge RMSEs
train_set <- train_set %>% mutate(price = scale(train_set$price))
test_set <- test_set %>% mutate(price = scale(test_set$price))
nrow(bmw)
#first algo is KNN
#test different k values and add trainControl to cross-validate
values = data.frame(k = seq(1,15,2))
control <- trainControl(method = 'cv',number = 10,p=.8)
train_knn <- train(price~.,
                   data = train_set,
                   method = 'knn',
                   trControl= control,
                   tuneGrid = values)
#predict on train set to see how RMSE compares to test
of <- predict(train_knn,newdata = train_set,type = 'raw')
y_hat <- predict(train_knn,newdata = test_set,type = 'raw')
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
#both RMSEs around same range, not overfit
##DONT FORGET TO CHANGE VARIABLE NAMES
RMSE(train_set$price,of)
RMSE(test_set$price,y_hat)

#second algo is randomTrees
train_rpart <- train(price~.,
                     method = 'rpart',
                     tuneGrid = data.frame(cp = seq(0,.05,10)),
                     data = train_set)
#there was one node for every row in train_set
#RMSE is slightly lower than knn
pred_tree <- predict(train_rpart,newdata = test_set,type = 'raw')
RMSE(test_set$price,pred_tree)
