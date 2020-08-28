# Random Forest Classification

# Ensure presence of file dependencies
source("src/train_test.R")

library(caret)
library(imbDis)

set.seed(225)

# random forest with 7 predictors
tc = trainControl(method = "cv", number = 10)
RF_7 = train(factor(y) ~ x1+x2+x3+x4+x5+x6+x7, data = train_dat, method = "rf", trControl = tc)

# random forest with all predictors
RF_all = train(factor(y) ~ ., data = train_dat, method = "rf", trControl = tc)

# obtain predicted probabilities and labels - 7 preds
preds_RF_7 = predict(RF_7$finalModel, test_dat, type = "prob")[,2]

# obtain predicted probabilities and labels - all preds
preds_RF_all = predict(RF_all$finalModel, test_dat, type = "prob")[,2]

# optional - save the files to csv within data directory
# uncomment code to run

# res_RF = data.frame(test_dat$y, preds_RF_7, preds_RF_all)
# names(res_RF)[1] = "orig"
# write.csv(res_RF, "data/res_RF.csv", row.names = FALSE)

# Define classes
imbD_RF_7 = imbDis(test_dat$y, preds_RF_7, 1)
imbD_RF_all = imbDis(test_dat$y, preds_RF_all, 1)

# c-statistic and F1 for 7 preds RF
auc.imbDis(imbD_RF_7)
brier.imbDis(imbD_RF_7)
logLoss.imbDis(imbD_RF_7)

# c-statistic and F1 for all preds RF
auc.imbDis(imbD_RF_all)
brier.imbDis(imbD_RF_all)
logLoss.imbDis(imbD_RF_all)
