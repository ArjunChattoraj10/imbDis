# Simulating a dataset

# Data to be simulated:
    
# Response:
    
## Binary (0,1) : 20% Class imbalance

# Covariates: 10

## x1: Continuous - Normal(0,1)
## x2: Continuous - Normal(0,1)
## x3: Continuous -  Normal(0,1)
## x4: Continuous -  Normal(0,1)
## x5: Categorical - Bernoulli(0.5)
## x6: Categorical - Bernoulli(0.7)
## x7: Continuous - Normal(0,1)
## x8: Categorical - Bernoulli(0.4)
## x9: Non-Linear - Quadratic
## x10: Non-Linear - Cubic

n_sim = 5000

set.seed(256)

# Continuous Normal
x1 = rnorm(n_sim)
x2 = rnorm(n_sim)
x3 = rnorm(n_sim)
x4 = rnorm(n_sim)

# Categorical Binary
x5 = rbinom(n_sim, 1, 0.5)
x6 = rbinom(n_sim, 1, 0.7)

# Interaction 
x7 = rnorm(n_sim)
x8 = rbinom(n_sim, 1, 0.4)

# Non-linear
x9 = rnorm(n_sim)
x10 = rnorm(n_sim)

# coefs = rnorm(15, -1, 1)
# coefs
# 
# i = 1
# b0 = coefs[i];  i = i+1
# b1 = coefs[i];  i = i+1
# b2 = coefs[i];  i = i+1
# b3 = coefs[i];  i = i+1
# b4 = coefs[i];  i = i+1
# b5 = coefs[i];  i = i+1
# b6 = coefs[i];  i = i+1
# b7 = coefs[i];  i = i+1
# b8 = coefs[i];  i = i+1
# b9 = coefs[i];  i = i+1
# b10 = coefs[i]; i = i+1
# b11 = coefs[i]; i = i+1
# b12 = coefs[i]; i = i+1
# b13 = coefs[i]; i = i+1
# b14 = coefs[i]

b0 = -0.8
b1 = 0.17
b2 = 0.36
b3 = -1.12
b4 = -0.84
b5 = 0.71
b6 = -0.61
b7 = -1.74
b8 = -0.36
b9 = 0.15
b10 = -1.24
b11 = -0.18
b12 = 0.88
b13 = -0.59
b14 = 0.36

# enter logistic model equation

logit = b0 +                                    # intercept
        b1*x1 +                                 # continuous
        b2*x2 +                                 # continuous
        b3*x3 +                                 # continuous
        b4*x4 +                                 # continuous
        b5*x5 +                                 # categorical
        b6*x6 +                                 # categorical
        b7*x7 +                                 # continuous - interaction
        b8*x8 +                                 # categorical - interaction
        b9*x7*x8 +
        b10*x9 + b11*x9^2 +                     # continuous - non-linear - quadratic
        b12*x10 + b13*x10^2 + b14*x10^3         # continuous - non-linear - cubic

# transform to probabilities and generate responses

y = factor(ifelse(plogis(logit) >= 0.7, 1, 0), levels = c(1,0))

# Sanity checks on simulated data
prop_1 = table(y)[2]/sum(table(y)); paste("Proportion of 1s:", prop_1)

barplot(table(y), ylim = c(0,5000), col = c("red2", "green2"),
        main = "Proportions in Simulated Dataset")

plot(sort(plogis(logit)), type = 'l', lwd = 2, main = "Logistic Curve of Simulated Data")

hist(logit, col = "lightcoral", main = "Histogram of Simulated Log(Odds)")

hist(plogis(logit), col = "lightcoral", main = "Distribution of Simulated Probabilities")

sim_dat = data.frame(y,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)
# head(sim_dat)

rm(b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,
   y,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)

# write.csv(sim_dat, file = "simulated_data_arjun.csv", row.names = FALSE)
