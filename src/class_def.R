# define S3 class for new metric

library(pROC)

simMetric = function(labels, pred.probs, case, bins = seq(0.1,0.5,0.1)){
    
    # check values
    param_check(labels, pred.probs, case, bins)
    
    # initialize the class via list object
    val = list(labels = labels, pred.probs = pred.probs, bins = bins, case = case)
    attr(val, "class") = "simMetric" # sets the class
    val
    
}

auc = function(obj) UseMethod("auc")

auc.simMetric = function(simMetric_obj){
    # function takes in an object of class simMetric
    # calculates the C-Statistic for specified frequency bins
    
    # define variables to reduce code clutter
    labels = simMetric_obj$labels
    pred.probs = simMetric_obj$pred.probs
    case = simMetric_obj$case
    bins = simMetric_obj$bins
    
    ## identify value of control
    control = unique(labels[labels != case])
    
    ## set labels as case = 1 and control = 0
    ## necessary to ensure proper functionality of roc()
    labels_01 = rep(0, length(labels))
    labels_01[labels == case] = 1
    
    ## total cases and controls in labels
    tot_cases = sum(labels_01)
    tot_controls = sum(labels_01 == 0)
    
    ## proportion of cases and controls in labels
    prop_cases = mean(labels)
    prop_control = 1 - prop_cases
    
    ## sample size calculations
    samp_size = round(ifelse(prop_cases <= max(bins),    # if the prop(cases) < max(bins)
                             tot_cases/max(bins),        # use the cases procedure
                             tot_controls/max(1-bins)))  # else use the controls procedure
    
    cases_list = round(bins*samp_size)
    
    ## subset labels and probs into only case and only not case
    
    ### indices with case label
    ### ensures correspondence of labels and probabilities after subsetting
    case_i = which(labels_01 == 1)
    
    ### subset labels and probs for case
    labels_case = labels[case_i]
    probs_case = pred.probs[case_i]
    
    ### subset labels and prons for not case
    labels_notcase = labels[-case_i]
    probs_control = pred.probs[-case_i]
    
    ## compute returned values
    auroc = rep(0, length(bins))
    n_samps = rep(0, length(bins))
    
    for(i in 1:length(bins)){
        
        # set up ROC calculations
        n_cases = cases_list[i]
        n_control = samp_size - n_cases
        
        # random sample of indices for case and non-case labels and probabilities
        sub_i_case = sample(length(probs_case), n_cases)
        sub_i_control = sample(length(probs_control), n_control)
        
        # labels and probabilities organized as all elements corresponding to case
        # followed by all elements corresponding to control
        labs = c(labels_case[sub_i_case], labels_notcase[sub_i_control])    
        probs = c(probs_case[sub_i_case], probs_control[sub_i_control])
        
        # shuffle labels and predictions
        # same indices used to subset ensures correspondence of 
        # labels and predictions after shuffling
        i_test = sample(length(labs))
        labs = labs[i_test]
        probs = probs[i_test]
        
        # calculate AUC and save results
        roc_i = roc(labs, probs) # default control = 0, case = 1
        auroc[i] = roc_i$auc
        n_samps[i] = length(probs)
    }
    
    # return data frame
    return(data.frame(bins, auroc, n_samps))
}

param_check = function(labels, pred.probs, case, bins){
    if(length(unique(labels)) != 2) 
        stop('\'labels\' may only have 2 distinct values.')
    
    if(class(pred.probs) != "numeric")
        stop('\'pred.probs\' contains non-numeric values.')
    
    if(any(pred.probs < 0 | pred.probs > 1)) 
        stop('At least one element of \'pred.probs\' is less than 0 or greater than 1.')
    
    if(length(labels) != length(pred.probs))
        stop('Length of \'labels\' does not match length of \'pred.probs\'.')
    
    if(!case %in% labels)
        stop('Value of \'case\' is not present in \'labels\'.')
    
    if(class(bins) != "numeric")
        stop('\'bins\' is non-numeric.')
    
    if(any(bins < 0 | bins > 1)) 
        stop('At least one element of \'bins\' is less than 0 or greater than 1.')
    
    if(any(bins == 0 | bins == 1))
        stop('Elements of \'bin\' cannot be exactly 0 or 1.')
    
}

# Testing

## to ensure calls work
SM1 = simMetric(c(1,1,0,1), c(0.1,0.2,0.3,0.4), 1, c(0.1,0.2,0.3,0.4))
SM2 = simMetric(labels = c(1,1,0,1), pred.probs = c(0.1,0.2,0.3,0.4), case = 1)

## real function calls

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

case = 1

bins = c(0.1, 0.2, 0.3)

SM3 = simMetric(labels, pred.probs, case)
auc(SM3)
