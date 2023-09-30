library(caret) # Classification and regression tree => caret build by Max kuhn
library(tidyverse)

# Simple ML Pipeline
# 1. split data
# 2. Train model
# 3. score model aka. prediction
# 4. evaluate model

View(mtcars)

# Subsset only columns we want
full_df <- mtcars %>%
  select(mpg, hp, wt, am)

# Check NA missing values
full_df %>%
  complete.cases()  %>%  # check missing value True = not missing
  mean()    # check missing value by mean because true = 1, false = 0 
            # if mean results less than 1 => data has missing values *****
            # Ex. resules mean = 0.78 => this data has missing value = 22%

# drop rows with NA missing values
clean_df <- full_df %>%
  drop_na()  # drop missing values
  # replace_na()   # replace missing values by mean median mode

# 1. Split data 80% train, 20% test
split_data <- function(df) {
  set.seed(42)    # lock results by set seed because train is random data sample
  n <- nrow(df)
  train_id <- sample(1:n, size = 0.8*n) #split train 80% test 20%
  train_df <- df[train_id, ]
  test_df <- df[-train_id,]
  return( list(training = train_df,
               testng = test_df) )
}

prep_data <- split_data(clean_df)
prep_data$training
train_df <- prep_data[[1]]
test_df <- prep_data[[2]]

# 2. Train model
set.seed(42)    # lock results by set seed because train is random data sample
lm_model <- train(mpg ~ . , 
                  data=train_df, 
                  method="lm")  # ranger = random forest
            # train(mpg (y variables) ~  . (ใช้ All data, data =train data, method = algorithm = lm = linear regression)

lm_model  # view model and score model

# 3. score model aka. prediction
p <- predict(lm_model, newdata = test_df)

# 4. evaluate model
# Mean sqsuate error (MAE)
(mae <- mean(abs(p - test_df$mpg)))
#mae

# Root MSE
(rmse <- sqrt(mean((p - test_df$mpg)**2)))
#rmse


lm_model$finalModel   # extract equation model
varImp(lm_model)      # variables importance













