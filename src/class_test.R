source("class_def.R")

# Testing for simMetric

## to ensure call works
SM1 = simMetric(c(1,1,1,0), c(0.1,0.2,0.3,0.4), 1, c(0.1,0.2,0.3,0.4))
SM2 = simMetric(labels = c(1,1,1,0), pred = c(0.1,0.2,0.3,0.4), case = 1)


## real function calls

set.seed(34)
n = 1000
orig.labels = rbinom(n, 1, 0.2)
pred.labels = rbinom(n, 1, 0.2)

# make realistic pred.probs
pred.probs= rep(0, n)
for(i in 1:n){
    pred.probs[i] = ifelse(pred.labels[i] == 1, 
                           rnorm(1,0.75, 0.2), 
                           rnorm(1,0.25, 0.2))
}

pred.probs[pred.probs < 0] = 0.05
pred.probs[pred.probs > 1] = 0.95

case = 1

SM3 = simMetric(orig.labels, pred.probs, case)
SM3$auc = auc(SM3)
SM3$brier = brier(SM3)

bins = seq(0.05,0.2,0.05)
SM4 = simMetric(orig.labels, pred.probs, case, bins)
SM4$auc = auc(SM4)
SM4$brier = brier(SM4)
