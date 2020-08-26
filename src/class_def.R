# define S3 class for new metric

library(pROC)


imbDis = function(labels, pred, case, bins = seq(0.05,0.5,0.05)){
    
    # check values
    param_check(labels, pred, case, bins)
    
    # initialize the class via list object using provided arguments
    obj = list(labels = labels, pred = pred, case = case, bins = bins)
    
    attr(obj, "class") = "imbDis" # sets the class
    
    # define control and add to list
    obj$control = unique(labels[labels != case])
    
    # define standardized labels and add to list
    obj$labs_01 = standardizeLabels(labels, case)
    
    # define sample size and bin case sizes
    obj$sample.size = calc_samplesize(obj$labs_01, bins)
    obj$bin.caseSizes = round(bins*obj$sample.size)
    
    return(obj)
}

param_check = function(labels, pred, case, bins){
    # function to check if arguments of a imbDis object are valid
    
    if(length(unique(labels)) != 2) 
        stop('\'labels\' may only have 2 distinct values.')
    
    if(!is.numeric(pred))
        stop('\'pred\' contains non-numeric values.')  
    
    if(any(pred < 0 | pred > 1)) 
        stop('At least one element of \'pred\' is less than 0 or greater than 1.')
    
    if(length(labels) != length(pred))
        stop('Length of \'labels\' does not match length of \'pred\'.')
    
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
auc.imbDis = function(obj){
    # function takes in an object of class imbDis
    # calculates the C-Statistic for specified frequency bins
    
    # define variables to reduce code clutter
    pred = obj$pred
    bins = obj$bins
    labels_01 = obj$labs_01 # necessary for proper functionality of roc()
    samp_size = obj$sample.size
    cases_list = obj$bin.caseSizes
    
    ## subset labels and pred into only case and only not case
    
    ### indices with case label
    ### ensures correspondence of labels and probabilities after subsetting
    case_i = which(labels_01 == 1)
    
    ### subset labels and pred for case
    labels_case = labels_01[case_i]
    pred_case = pred[case_i]
    
    ### subset labels and pred for not case
    labels_control = labels_01[-case_i]
    pred_control = pred[-case_i]
    
    ## compute returned values
    auroc = rep(0, length(bins))
    n_samps = rep(0, length(bins))
    
    for(i in 1:length(bins)){
        
        # set up ROC calculations
        n_cases = cases_list[i]
        n_control = samp_size - n_cases
        
        # random sample of indices for case and non-case labels and probabilities
        sub_i_case = sample(length(pred_case), n_cases)
        sub_i_control = sample(length(pred_control), n_control)
        
        # labels and probabilities organized as all elements corresponding to case
        # followed by all elements corresponding to control
        labs = c(labels_case[sub_i_case], labels_control[sub_i_control])    
        probs = c(pred_case[sub_i_case], pred_control[sub_i_control])
        
        # shuffle labels and predictions
        # same indices used to subset ensures correspondence of 
        # labels and predictions after shuffling
        i_test = sample(length(labs))
        labs = labs[i_test]
        probs = probs[i_test]
        
        # calculate AUC and save results
        # Message "Setting levels" suppressed - occurs on every loop of ROC calculation
        roc_i = suppressMessages(roc(labs, probs)) # default control = 0, case = 1
        auroc[i] = roc_i$auc
        n_samps[i] = length(probs)
    }
    
    # return data frame
    return(data.frame(bins, auroc, n_samps))
}

# brier score:
# BS = 1/n sum_i..n (p_i - o_i)^2
brier = function(obj) UseMethod("brier")
brier.imbDis = function(obj){
    # function takes in an object of class imbDis
    # calculates the C-Statistic for specified frequency bins
    
    # define variables to reduce code clutter
    pred = obj$pred
    bins = obj$bins
    labels_01 = obj$labs_01 # necessary for proper functionality of roc()
    samp_size = obj$sample.size
    cases_list = obj$bin.caseSizes
    
    ## subset labels and pred into only case and only not case
    
    ### indices with case label
    ### ensures correspondence of labels and probabilities after subsetting
    case_i = which(labels_01 == 1)
    
    ### subset labels and pred for case
    labels_case = labels_01[case_i]
    pred_case = pred[case_i]
    
    ### subset labels and pred for not case
    labels_control = labels_01[-case_i]
    pred_control = pred[-case_i]
    
    ## compute returned values
    brier = rep(0, length(bins))
    n_samps = rep(0, length(bins))
    
    for(i in 1:length(bins)){
        
        # set up ROC calculations
        n_cases = cases_list[i]
        n_control = samp_size - n_cases
        
        # random sample of indices for case and non-case labels and probabilities
        sub_i_case = sample(length(pred_case), n_cases)
        sub_i_control = sample(length(pred_control), n_control)
        
        # labels and probabilities organized as all elements corresponding to case
        # followed by all elements corresponding to control
        labs = c(labels_case[sub_i_case], labels_control[sub_i_control])    
        probs = c(pred_case[sub_i_case], pred_control[sub_i_control])
        
        # shuffle is not required since it is just a value 
        # of individual observationspred
        brier[i] = mean((probs - labs)^2)
        n_samps[i] = length(probs)
    }
    
    # return data frame
    return(data.frame(bins, brier, n_samps))
}

logLoss = function(obj) UseMethod("logLoss")
logLoss.imbDis = function(obj){
    # function takes in an object of class imbDis
    # calculates the log loss for specified frequency bins
    
    # define variables to reduce code clutter
    pred = obj$pred
    bins = obj$bins
    labels_01 = obj$labs_01 # necessary for proper functionality of roc()
    samp_size = obj$sample.size
    cases_list = obj$bin.caseSizes
    
    ## subset labels and pred into only case and only not case
    
    ### indices with case label
    ### ensures correspondence of labels and probabilities after subsetting
    case_i = which(labels_01 == 1)
    
    ### subset labels and pred for case
    labels_case = labels_01[case_i]
    pred_case = pred[case_i]
    
    ### subset labels and pred for not case
    labels_control = labels_01[-case_i]
    pred_control = pred[-case_i]
    
    ## compute returned values
    logloss = rep(0, length(bins))
    n_samps = rep(0, length(bins))
    
    for(i in 1:length(bins)){
        
        # set up ROC calculations
        n_cases = cases_list[i]
        n_control = samp_size - n_cases
        
        # random sample of indices for case and non-case labels and probabilities
        sub_i_case = sample(length(pred_case), n_cases)
        sub_i_control = sample(length(pred_control), n_control)
        
        # labels and probabilities organized as all elements corresponding to case
        # followed by all elements corresponding to control
        labs = c(labels_case[sub_i_case], labels_control[sub_i_control])    
        probs = c(pred_case[sub_i_case], pred_control[sub_i_control])
        
        # shuffle labels and predictions
        # same indices used to subset ensures correspondence of 
        # labels and predictions after shuffling
        i_test = sample(length(labs))
        labs = labs[i_test]
        probs = probs[i_test]
        
        # calculate log loss value and save results
        
        # log loss function definition influenced by Yachen Yan
        # and his MLmetrics package
        # It was necessary in order to handle 0-1 perfectly fit probabilities
        # when calculating logs
        
        
        eps = .Machine$double.eps
        probs = pmax(pmin(probs, 1 - eps), eps) # replace 0 with eps and 1 with 1-eps
        
        logloss[i] = -mean(labs*log(probs) + (1-labs)*log(1-probs))
        n_samps[i] = length(probs)
    }
    
    # return data frame
    return(data.frame(bins, logloss, n_samps))
}

manualLoss = function(obj, f) UseMethod("manualLoss")
manualLoss.imbDis = function(obj, f){
    # framework of metric calculation. 
    # performs all necessary pre-processing and
    # obtains the result of the provided discrimination metric for every loop
    
    # f must be a function that takes in arguments in the form f(labels, probs)
    # and must return a single value.
    
    # define variables to reduce code clutter
    pred = obj$pred
    bins = obj$bins
    labels_01 = obj$labs_01 # necessary for proper functionality of roc()
    samp_size = obj$sample.size
    cases_list = obj$bin.caseSizes
    
    ## subset labels and pred into only case and only not case
    
    ### indices with case label
    ### ensures correspondence of labels and probabilities after subsetting
    case_i = which(labels_01 == 1)
    
    ### subset labels and pred for case
    labels_case = labels_01[case_i]
    pred_case = pred[case_i]
    
    ### subset labels and pred for not case
    labels_control = labels_01[-case_i]
    pred_control = pred[-case_i]
    
    ## compute returned values
    metric = rep(0, length(bins))
    n_samps = rep(0, length(bins))
    
    for(i in 1:length(bins)){
        
        # set up ROC calculations
        n_cases = cases_list[i]
        n_control = samp_size - n_cases
        
        # random sample of indices for case and non-case labels and probabilities
        sub_i_case = sample(length(pred_case), n_cases)
        sub_i_control = sample(length(pred_control), n_control)
        
        # labels and probabilities organized as all elements corresponding to case
        # followed by all elements corresponding to control
        labs = c(labels_case[sub_i_case], labels_control[sub_i_control])    
        probs = c(pred_case[sub_i_case], pred_control[sub_i_control])
        
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



