library(imbDis)

# good inputs - numeric labels
set.seed(34)
n = 1000
labels = rbinom(n, 1, 0.2)

# make realistic pred.probs
pred.probs= rep(0, n)
for(i in 1:n){
    if(labels[i] == 0) pred.probs[i] = rnorm(1,0.25, 0.2)
    else pred.probs[i] = rnorm(1,0.75, 0.2)
}
pred.probs[pred.probs < 0] = 0.05
pred.probs[pred.probs > 1] = 0.95

bins = c(0.1, 0.2, 0.3)
case = 1

# inputs below should fail and return an error
n = 1000
labs_bad = sample(c(0,1,2), n, replace = T)
probs_bad = runif(n,0,1); probs_bad[3] = 1.12
bins_bad = c(-0.1, 0.1, 0.2, 0.3)
bins_bad2 = c(0, 0.1, 0.2, 0.3)
case_bad = 3

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
