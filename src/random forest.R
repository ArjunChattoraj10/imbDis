# Random Forest Classification

# Ensure presence of file dependencies
source("train_test.R")

library(caret)

set.seed(225)

tc = trainControl(method = "cv", number = )
rf = train(y ~ ., data = train_dat, method = "rf", 
           trControl = trainControl(method = "cv"))

# Scoring metrics

predsRF_01 = predict(rf$finalModel, test_dat)

# Confusion Matrix

conmatRF = confusionMatrix(predsRF_01, test_dat$y); conmatRF

## Brier Score

## F1 score

precisionRF = conmatRF$byClass["Pos Pred Value"]
recallRF = conmatRF$byClass["Sensitivity"]

F1_RF = 2*precisionRF*recallRF/(precisionRF + recallRF); as.numeric(F1_RF)

## C-statistic

