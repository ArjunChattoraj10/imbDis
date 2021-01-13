# Decision Tree Classification

# Load caret and imbDis
library(caret)
library(imbDis)

# Ensure presence of file dependencies - train/test split data
source("utils/examples/train_test.R") 

set.seed(225)

# decision tree with 7 predictors
dec_tree_7 = train(factor(y) ~ x1+x2+x3+x4+x5+x6+x7, data = train_dat, method = "rpart")

# decision tree with all predictors
dec_tree_all = train(factor(y) ~ ., data = train_dat, method = "rpart")

# obtain predicted probabilities and labels - 7 preds
# col 1 is for label 0, col 2 is for label 1
preds_DT_7 = predict(dec_tree_7$finalModel, test_dat)[,2]

# obtain predicted probabilities and labels - all preds
preds_DT_all = predict(dec_tree_all$finalModel, test_dat)[,2]

# optional - save the files to csv within data directory
# uncomment code to run

# res_DT = data.frame(test_dat$y, preds_DT_7, preds_DT_all)
# names(res_DT)[1] = "orig"
# write.csv(res_DT, "utils/data/res_DT.csv", row.names = FALSE)

# Define classes
imbD_DT_7 = imbDis(test_dat$y, preds_DT_7, 1)
imbD_DT_all = imbDis(test_dat$y, preds_DT_all, 1)

# Metrics for 7 preds DT
auc(imbD_DT_7)
brier(imbD_DT_7)
logLoss(imbD_DT_7)

# Metrics for all preds DT
auc(imbD_DT_all)
brier(imbD_DT_all)
logLoss(imbD_DT_all)
