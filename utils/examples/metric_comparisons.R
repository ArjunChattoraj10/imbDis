# paired t-tests
# compare Decision Tree with Random Forest

# Run logistic_regression.R, decision_tree.R and random_forest.R before running code below

## 7-predictor models ##

# Decision Tree
auc7_DT = auc(imbD_DT_7)$auroc
br7_DT = brier(imbD_DT_7)$brier
ll7_DT = logLoss(imbD_DT_7)$logloss

# Random Forest
auc7_RF = auc(imbD_RF_7)$auroc
br7_RF = brier(imbD_RF_7)$brier
ll7_RF = logLoss(imbD_RF_7)$logloss

t.test(auc7_RF, auc7_DT, paired = T, alternative = 'greater')
t.test(br7_RF, br7_DT, paired = T, alternative = 'less')
t.test(ll7_RF, ll7_DT, paired = T, alternative = 'less')


# c-statistic and F1 for all preds DT
aucAll_DT = auc(imbD_DT_all)$auroc
brAll_DT = brier(imbD_DT_all)$brier
llAll_DT = logLoss(imbD_DT_all)$logloss

# c-statistic and F1 for all preds RF
aucAll_RF = auc(imbD_RF_all)$auroc
brAll_RF = brier(imbD_RF_all)$brier
llAll_RF = logLoss(imbD_RF_all)$logloss

t.test(aucAll_RF, aucAll_DT, paired = T, alternative = 'greater')
t.test(brAll_RF, brAll_DT, paired = T, alternative = 'less')
t.test(llAll_RF, llAll_DT, paired = T, alternative = 'less')
