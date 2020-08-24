# define S3 class for new metric

library(pROC)

simMetric = function(labels, probs, case, bins = seq(0.1,0.5,0.1)){
    
    # check values
    param_check(labels, probs, case, bins)
    
    # initialize the class via list object using provided arguments
    obj = list(labels = labels, probs = probs, case = case, bins = bins)
    
    attr(obj, "class") = "simMetric" # sets the class
    
    # define control and add to list
    obj$control = unique(labels[labels != case])
    
    # define standardized labels and add to list
    obj$labs_01 = standardizeLabels(labels, case)
    
    # define sample size and bin case sizes
    obj$sample.size = calc_samplesize(obj$labs_01, bins)
    obj$bin.caseSizes = round(bins*obj$sample.size)
    
    return(obj)
}

param_check = function(labels, probs, case, bins){
    # function to check if arguments of a simMetric object are valid
    
    if(length(unique(labels)) != 2) 
        stop('\'labels\' may only have 2 distinct values.')
    
    if(!is.numeric(probs))
        stop('\'probs\' contains non-numeric values.')  
    
    if(any(probs < 0 | probs > 1)) 
        stop('At least one element of \'pred.probs\' is less than 0 or greater than 1.')
    
    if(length(labels) != length(probs))
        stop('Length of \'labels\' does not match length of \'probs\'.')
    
    if(!case %in% labels)
        stop('Value of \'case\' is not present in \'labels\'.')
    
    if(!is.numeric(bins))
        stop('\'bins\' is non-numeric.')
    
    if(any(bins < 0 | bins > 1)) 
        stop('At least one element of \'bins\' is less than 0 or greater than 1.')
    
    if(any(bins == 0 | bins == 1))
        stop('Elements of \'bin\' cannot be exactly 0 or 1.')
}

standardizeLabels = function(labels, case){
    # converts a binary vector of labels  to a vector
    # where all case values are 1 and controls are 0
    labels_01 = rep(0, length(labels))
    labels_01[labels == case] = 1
    return(labels_01)
}
standardiseLabels = standardizeLabels # alternate spelling

calc_samplesize = function(labels_01, bins){
    ## total cases and controls in labels
    tot_cases = sum(labels_01)
    tot_controls = sum(labels_01 == 0)
    
    ## proportion of cases and controls in labels
    prop_cases = mean(labels_01)
    prop_control = 1 - prop_cases
    
    ## sample size calculations
    samp_size = round(ifelse(prop_cases <= max(bins),    # if the prop(cases) < max(bins)
                             tot_cases/max(bins),        # use the cases procedure
                             tot_controls/max(1-bins)))  # else use the controls procedure
    
    return(samp_size)
}

auc = function(obj) UseMethod("auc")
auc.simMetric = function(obj){
    # function takes in an object of class simMetric
    # calculates the C-Statistic for specified frequency bins
    
    # define variables to reduce code clutter
    probs = obj$probs
    bins = obj$bins
    labels_01 = obj$labs_01 # necessary for proper functionality of roc()
    samp_size = obj$sample.size
    cases_list = obj$bin.caseSizes
    
    ## subset labels and probs into only case and only not case
    
    ### indices with case label
    ### ensures correspondence of labels and probabilities after subsetting
    case_i = which(labels_01 == 1)
    
    ### subset labels and probs for case
    labels_case = labels_01[case_i]
    probs_case = probs[case_i]
    
    ### subset labels and prons for not case
    labels_control = labels_01[-case_i]
    probs_control = probs[-case_i]
    
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
        labs = c(labels_case[sub_i_case], labels_control[sub_i_control])    
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

# brier score:
# BS = 1/n sum_i..n (p_i - o_i)^2
brier = function(obj) UseMethod("brier")
brier.simMetric = function(obj){
    # function takes in an object of class simMetric
    # calculates the C-Statistic for specified frequency bins
    
    # define variables to reduce code clutter
    probs = obj$probs
    bins = obj$bins
    labels_01 = obj$labs_01 # necessary for proper functionality of roc()
    samp_size = obj$sample.size
    cases_list = obj$bin.caseSizes
    
    ## subset labels and probs into only case and only not case
    
    ### indices with case label
    ### ensures correspondence of labels and probabilities after subsetting
    case_i = which(labels_01 == 1)
    
    ### subset labels and probs for case
    labels_case = labels_01[case_i]
    probs_case = probs[case_i]
    
    ### subset labels and prons for not case
    labels_control = labels_01[-case_i]
    probs_control = probs[-case_i]
    
    ## compute returned values
    brier = rep(0, length(bins))
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
        labs = c(labels_case[sub_i_case], labels_control[sub_i_control])    
        probs = c(probs_case[sub_i_case], probs_control[sub_i_control])
        
        # shuffle is not required since it is just a value 
        # of individual observations
        brier[i] = mean((probs - labs)^2)
        n_samps[i] = length(probs)
    }
    
    # return data frame
    return(data.frame(bins, brier, n_samps))
}

manual_loss = function(obj) UseMethod("manual_loss")
munual_loss.simMetric = function(obj, f){
    # framework of metric calculation. 
    # performs all necessary pre-processing and
    # obtains the result of the provided discrimination metric for every loop
    
    # f must be a function that takes in arguments in the form f(labels, probs)
    # and must return a single value.
    
    # define variables to reduce code clutter
    probs = obj$probs
    bins = obj$bins
    labels_01 = obj$labs_01 # necessary for proper functionality of roc()
    samp_size = obj$sample.size
    cases_list = obj$bin.caseSizes
    
    ## subset labels and probs into only case and only not case
    
    ### indices with case label
    ### ensures correspondence of labels and probabilities after subsetting
    case_i = which(labels_01 == 1)
    
    ### subset labels and probs for case
    labels_case = labels_01[case_i]
    probs_case = probs[case_i]
    
    ### subset labels and prons for not case
    labels_control = labels_01[-case_i]
    probs_control = probs[-case_i]
    
    ## compute returned values
    metric = rep(0, length(bins))
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
        labs = c(labels_case[sub_i_case], labels_control[sub_i_control])    
        probs = c(probs_case[sub_i_case], probs_control[sub_i_control])
        
        # shuffle labels and predictions
        # same indices used to subset ensures correspondence of 
        # labels and predictions after shuffling
        i_test = sample(length(labs))
        labs = labs[i_test]
        probs = probs[i_test]
        
        # calculate metric value and save results
        metric[i] = f(labs, probs)
        n_samps[i] = length(probs)
    }
    
    # return data frame
    return(data.frame(bins, metric, n_samps))
}



