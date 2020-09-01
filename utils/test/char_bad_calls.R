library(imbDis)

# good inputs - character labels
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

# inputs below should fail and return an error
n = 1000
labs_bad = sample(c("A","B","C"), n, replace = T)
probs_bad = runif(n,0,1); probs_bad[3] = 1.12
case_bad = "D"
bins_bad = c(-0.1, 0.1, 0.2, 0.3)
bins_bad2 = c(0, 0.1, 0.2, 0.3)

# labels error check: non-binary
imbDis(labs_bad, pred.probs, case, bins)

# labels and pred error check - differing lengths
imbDis(labels, pred.probs[-1], case, bins)

# pred error check - value > 1
imbDis(labels, probs_bad, case, bins)

# case error check - not a value in labels
imbDis(labels, pred.probs, case_bad, bins)

# bins error check 1 - value > 1
imbDis(labels, pred.probs, case, bins_bad)

# bin error check 2 - value = 0 or = 1
imbDis(labels, pred.probs, case, bins_bad2)
