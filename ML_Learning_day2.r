library(tidyverse)
library(caret)
library(mlbench)
data()

data("BostonHousing")
View(BostonHousing )
df <- BostonHousing

# complete  data?
mean(complete.cases(df))

## 1. train test split
split_data <- function(df, train_size=0.8) {
  set.seed(42)
  n <- nrow(df)
  id <- sample(1:n, size=n*train_size)
  train_df <- df[id, ]
  test_df <- df[-id, ]
  list(train = train_df, test = test_df) # return = optional put in function
}

prep_data <- split_data(df)
prep_data
train_data <- prep_data[[1]]
test_data <- prep_data[[2]]

## 2. train model
model <- train(medv ~ rm + b + crim,
               data = train_data,
               method = "lm")
model # defualt use bootstrap = 25 reps (iteration) 
model$finalModel

## 3. score/predict new data (test/unseen data)
p <- predict(model, newdata = test_data)
p

## 4. evaluate model => absolute metrics
# mae, mse, rmse
cal_mae <- function(actual, pred) {
  error <- actual - pred
  mean(abs(error))
}

cal_mse <- function(actual, pred) {
  error <- actual - pred
  mean(error**2)
}

cal_rmse <- function(actual, pred) {
  error <- actual - pred
  sqrt(mean(error**2))
}

# result evaluation
cal_mae(test_data$medv, p)
cal_mse(test_data$medv, p)
cal_rmse(test_data$medv, p)

## save model .RDS
saveRDS(model, "lm_model.RDS")






