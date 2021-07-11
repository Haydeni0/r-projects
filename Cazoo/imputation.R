
# TRY FOR TRANSMISSION, FUEL ETC


library(tree)
library(rpart)
library(rpart.plot)
library(caret)


tree <- rpart(TransmissionType ~ Model, data = na.omit(training_data))

tree.pred <- predict(tree, testing_data, type = "class")

confusionMatrix(tree.pred, testing_data$TransmissionType)

tree <- rpart(TransmissionType ~ Model + Make, data = na.omit(training_data))

tree.pred <- predict(tree, testing_data, type = "class")

confusionMatrix(tree.pred, testing_data$TransmissionType)


library(xgboost)
library(Matrix)
library(magrittr)
library(dplyr)
library(ROCR)
# XGBoost https://finnstats.com/index.php/2021/04/17/gradient-boosting-in-r/

# create DMatrix objects for the training and testing data
# Use as.numeric()-1 for the labels because xgb was giving an error as it wants the data as 0,1 not 1,2
M.train <- sparse.model.matrix(TransmissionType ~ Model+Make, data = training_data)
D.train <- xgboost::xgb.DMatrix(data = as.matrix(M.train), label = as.numeric(training_data$TransmissionType)-1)

M.test <- sparse.model.matrix(TransmissionType ~ Model+Make, data = testing_data)
D.test <- xgboost::xgb.DMatrix(data = as.matrix(M.test), label = as.numeric(testing_data$TransmissionType)-1)

num_classes <- length(unique(training_data$TransmissionType))

# Parameter list
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = num_classes)
watchlist <- list(train = D.train, test = D.test)

# Run xgb
model.xgb <- xgb.train(params = xgb_params,
                       data = D.train,
                       nrounds = 1000,
                       watchlist = watchlist,
                       eta = 0.001,
                       max.depth = 3,
                       gamma = 0,
                       subsample = 1,
                       colsample_bytree = 1)


# Check log loss for the training and testing data to see if xbg is overfitting
e <- data.frame(model.xgb$evaluation_log)
plot(e$iter, e$train_mlogloss, col = 'blue')
lines(e$iter, e$test_mlogloss, col = 'red')

# Predict the training and testing data using the model
p <- predict(model.xgb, newdata = D.test)

pred <- matrix(p, nrow = (num_classes), ncol = length(p)/(num_classes)) %>%
  t() %>%
  data.frame() %>%
  mutate(label = testing_data$TransmissionType, max_prob = max.col(., "last")-1)


table(Prediction = pred$max_prob, Actual = pred$label)


# pred.xgb.training <- predict(model.xgb, newdata = D.train)
# pred.xgb.training <- as.numeric(pred.xgb.training > 0.5)
# 
# pred.xgb.test <- predict(model.xgb, newdata = D.test)
# pred.xgb.test <- as.numeric(pred.xgb.test > 0.5)
# 
# auc.pred.xgb <-
#   prediction(as.numeric(pred.xgb.test),
#              as.factor(testing_data$TransmissionType))
# auc.perf.xgb <- performance(auc.pred.xgb, measure = "auc")
# auc.perf.xgb@y.values






