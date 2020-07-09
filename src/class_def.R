# define S3 class for new metric

library(pROC)

simMetric = function(orig.labels, pred.labels, pred.probs, 
                     case, bins = seq(0.1,0.5,0.1)){
    
    # check values
    param_check(orig.labels, pred.labels, pred.probs, case, bins)
    
    # initialize the class via list object using provided arguments
    obj = list(orig.labels = orig.labels, pred.labels = pred.labels, 
               pred.probs = pred.probs, bins = bins, case = case)
    
    attr(obj, "class") = "simMetric" # sets the class
    
    # define control and add to list
    obj$control = unique(pred.labels[pred.labels != case])
    
    # define standardized labels and add to list
    obj$orig_01 = standardizeLabels(orig.labels, case)
    obj$pred_01 = standardizeLabels(pred.labels, case)
    
    # define sample size and bin case sizes
    obj$sample.size = calc_samplesize(obj$pred_01, bins)
    obj$bin.caseSizes = round(bins*obj$sample.size)
    
    return(obj)
}

param_check = function(orig.labels, pred.labels, pred.probs, case, bins){
    # function to check if arguments of a simMetric object are valid
    
    if(length(unique(orig.labels)) != 2) 
        stop('\'orig.labels\' may only have 2 distinct values.')
    
    if(length(unique(pred.labels)) != 2) 
        stop('\'pred.labels\' may only have 2 distinct values.')
    
    if(!is.numeric(pred.probs))
        stop('\'pred.probs\' contains non-numeric values.')
    
    if(length(orig.labels) != length(pred.labels))
        stop('Length of \'orig.labels\' does not match length of \'pred.labels\'.')    
    
    if(any(pred.probs < 0 | pred.probs > 1)) 
        stop('At least one element of \'pred.probs\' is less than 0 or greater than 1.')
    
    if(length(pred.labels) != length(pred.probs))
        stop('Length of \'pred.labels\' does not match length of \'pred.probs\'.')
    
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
    pred.probs = obj$pred.probs
    bins = obj$bins
    labels_01 = obj$pred_01 # necessary for proper functionality of roc()
    samp_size = obj$sample.size
    cases_list = obj$bin.caseSizes
    
    ## subset labels and probs into only case and only not case
    
    ### indices with case label
    ### ensures correspondence of labels and probabilities after subsetting
    case_i = which(labels_01 == 1)
    
    ### subset labels and probs for case
    labels_case = labels_01[case_i]
    probs_case = pred.probs[case_i]
    
    ### subset labels and prons for not case
    labels_control = labels_01[-case_i]
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

f1 = function(obj) UseMethod("f1")
f1.simMetric = function(obj){
    
    # standardized labels only
    orig = obj$orig_01
    pred = obj$pred_01
    bins = obj$bins
    samp_size = obj$sample.size
    cases_list = obj$bin.caseSizes
    
    ## subset labels into only case and only not case
    
    ### indices with predicted case label
    ### ensures correspondence of labels after subsetting
    case_i = which(pred == 1)
    
    ### subset labels for case
    orig_case = orig[case_i]
    pred_case = pred[case_i]
    
    ### subset labels for not case
    orig_control = orig[-case_i]
    pred_control = pred[-case_i]
    
    ## compute returned values
    f1 = rep(0, length(bins))
    n_samps = rep(0, length(bins))
    
    for(i in 1:length(bins)){
        
        # set up ROC calculations
        n_cases = cases_list[i]
        n_control = samp_size - n_cases
        
        # random sample of indices for case and non-case labels and probabilities
        sub_i_case = sample(length(orig_case), n_cases)
        sub_i_control = sample(length(orig_control), n_control)
        
        # labels and probabilities organized as all elements corresponding to case
        # followed by all elements corresponding to control
        orig_sub = c(orig_case[sub_i_case], orig_control[sub_i_control])    
        pred_sub = c(pred_case[sub_i_case], pred_control[sub_i_control])
        
        # random shuffle not necessary as confusion matrix
        # does not depend on order of entries
        
        # perform F1 calculation
        
        # pivot table -- confusion matrix for subsetted data
        # rows for original, columns for predicted
        pivot = table(orig_sub, pred_sub)
        
        # trueNeg = pivot[1,1]
        truePos = pivot[2,2]
        # falseNeg = pivot[2,1]
        # falsePos = pivot[1,2]
        
        totOrigPos = sum(orig_sub)
        totPredPos = sum(pred_sub)
        
        # precision and recall
        prec = truePos/totPredPos
        rec = truePos/totOrigPos
        
        # use F1 formula and save to vector
        f1[i] = 2*prec*rec/(prec+rec)
        n_samps[i] = length(pred_sub)
    }
    
    # return data frame
    return(data.frame(bins, f1, n_samps))
}

