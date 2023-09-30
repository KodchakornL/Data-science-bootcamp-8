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

## Read RDS (load model)
model <- readRDS("lm_model.RDS")
model

# test again
data("BostonHousing")
# predict data 
predict(model, newdata = BostonHousing[1:20, ])


## train control => ควบคุมพฤติกรรมการ train ข้อมูล
# Change resampling technique
# 1. Bootstrap + iters = 50
ctrl <- trainControl(
  method = "boot", # default boot = bootstrap
  number = 50,     # iteration = 50
  verboseIter = TRUE  # print log iteration แต่ละรอบให้เราเห็นถึงรอบที่เท่าไหร่แล้ว
)

model <- train(medv ~ rm + b + crim,
               data = train_data,
               method = "lm",
               trControl = ctrl)

# 2. Leave one out CV => this method don't have iters
ctrl <- trainControl(
  method = "LOOCV", # LOOCV =  Leave one out CV 
  verboseIter = TRUE  # print log iteration แต่ละรอบให้เราเห็นถึงรอบที่เท่าไหร่แล้ว
)

model <- train(medv ~ rm + b + crim,
               data = train_data,
               method = "lm",
               trControl = ctrl)

# 3. K-fold *** + K = 5 + set seed เพื่อระหว่างที่มันสุ่ม k-fold เราอยากให้มันได้ผลลัพธ์เหมือนเดิม
set.seed(42)
ctrl <- trainControl(
  method = "cv", # CV = k-fold
  number = 5,    # K = 5 fold
  verboseIter = TRUE  # print log iteration แต่ละรอบให้เราเห็นถึงรอบที่เท่าไหร่แล้ว
)

model <- train(medv ~ rm + b + crim,
               data = train_data,
               method = "lm",
               trControl = ctrl)

## variable importance => ดูตัวแปรไหนสำคัญ
varImp(model)
model$finalModel
model$finalModel %>%
  summary()   # เรียกดูค่าทางสถิติ

#############################################################
## Normalization / standardization 
## add pre-process
# Standardization 
set.seed(42)
ctrl <- trainControl(
  method = "cv", # CV = k-fold
  number = 5,    # K = 5 fold
  verboseIter = TRUE  # print log iteration แต่ละรอบให้เราเห็นถึงรอบที่เท่าไหร่แล้ว
)

model <- train(medv ~ rm + b + crim,
               data = train_data,
               method = "lm",
               preProcess = c("center", "scale"), #**** z-score => center = val - mean, scale = SD.
               trControl = ctrl)
model

# Normalization
set.seed(42)
ctrl <- trainControl(
  method = "cv", # CV = k-fold
  number = 5,    # K = 5 fold
  verboseIter = TRUE  # print log iteration แต่ละรอบให้เราเห็นถึงรอบที่เท่าไหร่แล้ว
)

model <- train(medv ~ rm + b + crim,
               data = train_data,
               method = "lm",
               preProcess = c("range", "zv", "nzv"), #**** Normalization => range = start value at 0, zv = delete constant value with not make sense, nzv = delete a little vary value compare all data with not make sense
               trControl = ctrl)
model

#############################################################
## K-Nearest Neighbors
set.seed(25)
ctrl <- trainControl(
  method = "cv", # CV = k-fold
  number = 5,    # K = 5 fold
  verboseIter = TRUE  # print log iteration แต่ละรอบให้เราเห็นถึงรอบที่เท่าไหร่แล้ว
)

model <- train(medv ~ rm + b + crim +lstat + age,
               data = train_data,
               method = "knn",
               preProcess = c("range", "zv", "nzv"), #**** Normalization => range = start value at 0, zv = delete constant value with not make sense, nzv = delete a little vary value compare all data with not make sense
               tunelength= 2, # find จากเดิมหา 3 กลุ่ม default มาเป็นหา 5 กลุ่ม
               trControl = ctrl)
model

## rmse for test set
cal_rmse(actual = test_data$medv, p)

#############################################################
## K-Nearest Neighbors train k=5
set.seed(25)
ctrl <- trainControl(
  method = "cv", # CV = k-fold
  number = 5,    # K = 5 fold
  verboseIter = TRUE  # print log iteration แต่ละรอบให้เราเห็นถึงรอบที่เท่าไหร่แล้ว
)

model_k5 <- train(medv ~ rm + b + crim + lstat + age,
               data = train_data,
               method = "knn",
               tuneGrid = data.frame(k=5),    #set k=5 
               preProcess = c("range", "zv", "nzv"), #**** Normalization => range = start value at 0, zv = delete constant value with not make sense, nzv = delete a little vary value compare all data with not make sense
               trControl = trainControl(method="none"))
model_k5

## predict test set
p_train <- predict(model)
p_test <- predict(model, newdata=test_data)

## rmse for test set
rmse_train = cal_rmse(actual = train_data$medv, p_train)
rmse_test = cal_rmse(actual = test_data$medv, p_test)
rmse_train; rmse_test

## tuneLength VS. tuneGrid (set K manually)
ctrl <- trainControl(
    method = "cv", # CV = k-fold
    number = 5,    # K = 5 fold
    repeats = 5, # repeats = 5 
    verboseIter = TRUE  # print log iteration แต่ละรอบให้เราเห็นถึงรอบที่เท่าไหร่แล้ว
)

## tune grid
model <- train(medv ~ rm + b + crim + lstat + age,
                  data = train_data,
                  method = "knn",
                  metric = "Rsquared",
                  tuneGrid = data.frame(k=c(5,7,14)),
                  preProcess = c("range", "zv", "nzv"), #**** Normalization => range = start value at 0, zv = delete constant value with not make sense, nzv = delete a little vary value compare all data with not make sense
                  trControl = ctrl)
model






























