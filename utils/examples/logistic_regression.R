# Logistic Regression Classification

# Load the imbDis package
library(imbDis)

# Ensure presence of file dependencies - train/test split data
source("utils/examples/train_test.R")

set.seed(225)

# logistic regression with 7 predictors - linear
LR_7 = glm(y ~ x1+x2+x3+x4+x5+x6+x7, data = train_dat, family = "binomial")

# logistic regression with all predictors - linear
LR_lin = glm(y ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10, data = train_dat, family = "binomial")

# logistic regression with all predictors - full complexity
LR_full = glm(y ~ x1+x2+x3+x4+x5+x6+x7+x8+x7:x8+x9+I(x9^2)+x10+I(x10^2)+I(x10^3), 
                  data = train_dat, family = "binomial")

# Aside
# calculate McFadden's R-squared
# details: https://thestatsgeek.com/2014/02/08/r-squared-in-logistic-regression/

null_lr = glm(y ~ 1, train_dat, family = binomial)
1 - logLik(LR_7)/logLik(null_lr)
1 - logLik(LR_lin)/logLik(null_lr)
1 - logLik(LR_full)/logLik(null_lr)

# obtain predicted probabilities - 7 preds
preds_LR_7 = predict(LR_7, test_dat, type = "response")

# obtain predicted probabilities - all preds linear
preds_LR_lin = predict(LR_lin, test_dat, type = "response")

# obtain predicted probabilities - all preds full
preds_LR_full = predict(LR_full, test_dat, type = "response")

# optional - save the files to csv within data directory
# uncomment code to run

# res_LR = data.frame(test_dat$y, preds_LR_7, preds_LR_lin, preds_LR_full)
# names(res_LR)[1] = "orig"
# write.csv(res_LR, "utils/data/res_LR.csv", row.names = FALSE)

# Define classes
imbD_LR_7 = imbDis(test_dat$y, preds_LR_7, 1)
imbD_LR_lin = imbDis(test_dat$y, preds_LR_lin, 1)
imbD_LR_full = imbDis(test_dat$y, preds_LR_full, 1)

# Metrics for 7 preds LR
auc(imbD_LR_7)
brier(imbD_LR_7)
logLoss(imbD_LR_7)

# Metrics for all preds linear LR
auc(imbD_LR_lin)
brier(imbD_LR_lin)
logLoss(imbD_LR_lin)

# Metrics for all preds full LR
auc(imbD_LR_full)
brier(imbD_LR_full)
logLoss(imbD_LR_full)
