# Decision Tree Classification

# Ensure presence of file dependencies
source("train_test.R") # Performs the train/test split
source("class_def.R")  # Loads the class into the environment

library(caret)
library(rattle)
library(rpart)

# decision tree with 7 predictors
dec_tree_7 = train(factor(y) ~ x1+x2+x3+x4+x5+x6+x7, data = train_dat, method = "rpart")
fancyRpartPlot(dec_tree_7$finalModel, palettes = c("Reds","Greens"))

# decision tree with all predictors
dec_tree_all = train(factor(y) ~ ., data = train_dat, method = "rpart")
fancyRpartPlot(dec_tree_all$finalModel, palettes = c("Reds","Greens"))

# obtain predicted probabilities and labels - 7 preds
# col 1 is for label 0, col 2 is for label 1
preds_DT_7 = predict(dec_tree_7$finalModel, test_dat)[,2]

# obtain predicted probabilities and labels - all preds
preds_DT_all = predict(dec_tree_all$finalModel, test_dat)[,2]

# save the files to csv
res_DT = data.frame(test_dat$y, preds_DT_7, preds_DT_all)
names(res_DT)[1] = "orig"
write.csv(res_DT, "../data/res_DT.csv", row.names = FALSE)

# Define classes
SM_DT_7 = simMetric(test_dat$y, preds_DT_7, 1)
SM_DT_all = simMetric(test_dat$y, preds_DT_all, 1)

# c-statistic and F1 for 7 preds DT
auc.simMetric(SM_DT_7)
brier.simMetric(SM_DT_7)

# c-statistic and F1 for all preds DT
auc.simMetric(SM_DT_all)
brier.simMetric(SM_DT_all)

