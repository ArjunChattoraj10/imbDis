# Logistic Regression Classification

# Ensure presence of file dependencies
source("train_test.R")
source("class_def.R")

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

# obtain predicted probabilities and labels - 7 preds
preds_LR_7 = predict(LR_7, test_dat, type = "response")

# obtain predicted probabilities and labels - all preds linear
preds_LR_lin = predict(LR_lin, test_dat, type = "response")

# obtain predicted probabilities and labels - all preds full
preds_LR_full = predict(LR_full, test_dat, type = "response")

# save the files to csv
res_LR = data.frame(test_dat$y, preds_LR_7, preds_LR_lin, preds_LR_full)
names(res_LR)[1] = "orig"
write.csv(res_LR, "../data/res_LR.csv", row.names = FALSE)

# Define classes
SM_LR_7 = simMetric(test_dat$y, preds_LR_7, 1)
SM_LR_lin = simMetric(test_dat$y, preds_LR_lin, 1)
SM_LR_full = simMetric(test_dat$y, preds_LR_full, 1)

# c-statistic and F1 for 7 preds LR
auc.simMetric(SM_LR_7)
brier.simMetric(SM_LR_7)

# c-statistic and F1 for all preds linear LR
auc.simMetric(SM_LR_lin)
brier.simMetric(SM_LR_lin)

# c-statistic and F1 for all preds full LR
auc.simMetric(SM_LR_full)
brier.simMetric(SM_LR_full)

