source("class_def.R")

# Testing for simMetric

## to ensure call works
imbD1 = imbDis(c(1,1,1,0), c(0.1,0.2,0.3,0.4), 1, c(0.1,0.2,0.3,0.4))
imbD2 = imbDis(labels = c(1,1,1,0), pred = c(0.1,0.2,0.3,0.4), case = 1)


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

imbD3 = imbDis(orig.labels, pred.probs, case)
imbD3$auc = auc(imbD3)
imbD3$brier = brier(imbD3)
imbD3$logLoss = logLoss(imbD3)

bins = seq(0.05,0.2,0.05)
imbD4 = imbDis(orig.labels, pred.probs, case, bins)
imbD4$auc = auc(imbD4)
imbD4$brier = brier(imbD4)
imbD4$logLoss = logLoss(imbD4)

# trying the generalized metric method - manualLoss

# method 1: wrapper function
custom_auc = function(labels, pred){
    # A wrapper function around ROC
    # Takes in as input the original class labels and prediction probabilities
    # Return a float value of the Area Under The ROC
    
    roc = roc(labels, pred)
    return(roc$auc)
}

SM4$custom_auc = manualLoss(SM4, custom_auc)

# method 2: custom function
custom_brier = function(labels, pred){
    # a user-made function for the Brier score
    # Takes in as input the original class labels and prediction probabilities
    # Return a float value of the Bier score
    
    return(mean((pred - labels)^2))
}

SM4$custom_brier = manualLoss(SM4, custom_brier)


