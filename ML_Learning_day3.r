## ML Day 3
library(tidyverse)
library(caret)
library(mlbench)
data()

data("PimaIndiansDiabetes")
df <- PimaIndiansDiabetes
View(df)

## Train model
set.seed(42)
n <- nrow(df)
id <- sample(n, size=0.8*n)
train_df <- df[id,]
test_df <- df[-id,]

ctrl <- trainControl(method = "cv",
                     number = 5,
                     verboseIter = TRUE)

logistic_model <- train(diabetes ~ .,
                        data = train_df,
                        method ="glm",
                        trControl = ctrl
                        )
logistic_model

## score new dataset
p <- predict(logistic_model, newdata=test_df)

## evaluate model
confusionMatrix(p, test_df$diabetes, positive = "pos")
#confusionMatrix(p, test_df$diabetes, positive = "neg")

confusionMatrix(p, test_df$diabetes, 
                positive = "pos",
                mode="prec_recall")

###########################################################
## Regularization
# alpha = 0 => Ridge/ 1 => Lasso
my_grid <- expand.grid(alpha = 0:1,
                       lambda = seq(0.0005, 0.05, length=20))
my_grid

glmnet_model <- train(diabetes ~ .,
                        data = train_df,
                        method ="glmnet",
                        tuneGrid = my_grid,
                        trControl = ctrl
)
glmnet_model

## score new dataset
p <- predict(glmnet_model, newdata=test_df)
p

## evaluate model
confusionMatrix(p, test_df$diabetes, positive = "pos")
#confusionMatrix(p, test_df$diabetes, positive = "neg")

confusionMatrix(p, test_df$diabetes, 
                positive = "pos",
                mode="prec_recall")

###########################################################
## Decision tree
library(rpart)
library(rpart.plot)
tree_model <- train(diabetes ~ .,
                      data = train_df,
                      method ="rpart",
                      tuneGrid = expand.grid(cp = c(0.02, 0.1, 0.25)),
                      trControl = ctrl
)
tree_model
rpart.plot(tree_model$finalModel)

## score new dataset
p <- predict(tree_model, newdata=test_df)
p

## evaluate model
confusionMatrix(p, test_df$diabetes, positive = "pos")
#confusionMatrix(p, test_df$diabetes, positive = "neg")

confusionMatrix(p, test_df$diabetes, 
                positive = "pos",
                mode="prec_recall")


###########################################################
## Random forest
# mtry hyperparameter => # columns
rf_model <- train(diabetes ~ .,
                    data = train_df,
                    method ="rf",
                    #tuneGrid = expand.grid(mtry = c(3, 5)),
                    tuneLength = 5,
                    trControl = ctrl
)
rf_model

## score new dataset
p <- predict(rf_model, newdata=test_df)
p

## evaluate model
confusionMatrix(p, test_df$diabetes, positive = "pos")
#confusionMatrix(p, test_df$diabetes, positive = "neg")

confusionMatrix(p, test_df$diabetes, 
                positive = "pos",
                mode="prec_recall")

###########################################################
## resamples() => compare model perforance
# predict diabetes
model1 <- train(diabetes ~ .,
                  data = train_df,
                  method ="glm",
                  trControl = trainControl(
                    method="cv", number = 5
                  ))

model2 <- train(diabetes ~ .,
                data = train_df,
                method ="rpart",
                trControl = trainControl(
                  method="cv", number = 5
                ))

model3 <- train(diabetes ~ .,
                data = train_df,
                method ="rf",
                trControl = trainControl(
                  method="cv", number = 5
                ))

model4 <- train(diabetes ~ .,
                data = train_df,
                method ="glmnet",
                trControl = trainControl(
                  method="cv", number = 5
                ))

# resamples
list_models = list(
  logistic = model1,
  tree = model2,
  randomForest = model3,
  glmnet = model4
)

result <- resamples(list_models)

summary(result)

###########################################################
## K-Nearest Neighbors
data("BostonHousing")

View(BostonHousing)

mini_house <- BostonHousing %>%
  select(medv, rm, age, dis)

# Normalization min-max scaling [0-1]
normalize_data <- function(x) {
  (x - min(x) / max(x) - min(x))
}

# apply this function to all column in dataframe
mini_house_norm <- apply(mini_house, 2, normalize_data) # 2 = column
View(mini_house_norm)

# kmeans cllustering
km_result <- kmeans(mini_house, centers = 5)
km_result

# assign cluster back to dataframe
mini_house$cluster <- km_result$cluster
View(mini_house)

# run descriptive statistics
mini_house %>%
  group_by(cluster) %>%
  summarise(avg_price = mean(medv),
            avg_rooms = mean(rm))








































