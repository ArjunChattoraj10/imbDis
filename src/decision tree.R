# Decision Tree Classification

# Ensure presence of file dependencies
source("train_test.R")

library(caret)
library(rattle)
library(rpart)

dec_tree = train(y ~ ., data = train_dat, method = "rpart")
fancyRpartPlot(dec_tree$finalModel, palettes = c("Reds","Greens"))

prune.control = rpart.control(xval = 10)
dec2 = rpart(y ~ ., data = train_dat, method = "class", control = prune.control)
fancyRpartPlot(dec2, palettes = c("Reds","Greens"))

# Scoring metrics

## caret decision tree

predsDT_prob = predict(dec_tree$finalModel, test_dat)
predsDT_01 = factor(ifelse(predsDT_prob[,1] > 0.5, 1, 0), levels = c(1,0))

## rpart decision tree

predsDT2_prob = predict(dec2, test_dat)
predsDT2_01 = factor(ifelse(predsDT2_prob[,1] > 0.5, 1, 0), levels = c(1,0))


## Confusion Matrix

conmatDT = confusionMatrix(predsDT_01, test_dat$y); conmatDT
conmatDT2= confusionMatrix(predsDT2_01,test_dat$y); conmatDT2

## Brier Score

## F1 score

precisionDT = conmatDT$byClass["Pos Pred Value"]
recallDT = conmatDT$byClass["Sensitivity"]

F1_DT = 2*precisionDT*recallDT/(precisionDT + recallDT); as.numeric(F1_DT)

## C-statistic
