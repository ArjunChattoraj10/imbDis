library(imbDis)

# using a character vector as input labels

# test to see if it runs
imbD1 = imbDis(c("A","A","A","B"), c(0.1,0.2,0.3,0.4), "A", c(0.1,0.2,0.3,0.4))

# more realistic inputs
set.seed(34)
n = 1000
orig.labels = sample(c("A","B"), n, replace = T, prob = c(0.8,0.2))
pred.labels = sample(c("A","B"), n, replace = T, prob = c(0.8,0.2))

# make realistic pred.probs
pred.probs= rep(0, n)
for(i in 1:n){
    pred.probs[i] = ifelse(pred.labels[i] == "A", 
                           rnorm(1,0.75, 0.2), 
                           rnorm(1,0.25, 0.2))
}

pred.probs[pred.probs < 0] = 0.05
pred.probs[pred.probs > 1] = 0.95

case = "A"

imbD2 = imbDis(orig.labels, pred.probs, case)
imbD2$auc = auc(imbD3)
imbD2$brier = brier(imbD3)
imbD2$logLoss = logLoss(imbD3)
