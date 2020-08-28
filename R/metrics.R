#' @title Area Under the Receiver Operator Curve (ROC AUC)
#' 
#' @description 
#' Calculates the Area under the Receiver Operator Curve (ROC AUC) for each bin frequency 
#' with the same sample size.
#' 
#' @param obj An imbDis object.
#' @return data.frame with columns: bins, AUC, sample size.
#' @export
auc = function(obj) UseMethod("auc", obj)

#' @title Area Under the Receiver Operator Curve (ROC AUC)
#' 
#' @description 
#' Calculates the Area under the Receiver Operator Curve (ROC AUC) for each bin frequency 
#' with the same sample size.
#' 
#' @param obj An imbDis object.
#' @return data.frame with columns: bins, AUC, sample size.
#' @export
auc.imbDis = function(obj){
    
    # define variables to reduce code clutter'
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
        roc_i = suppressMessages(pROC::roc(labs, probs)) # default control = 0, case = 1
        auroc[i] = roc_i$auc
        n_samps[i] = length(probs)
    }
    
    # return data frame
    return(data.frame(bins, auroc, n_samps))
}

#' @title Brier Score
#' 
#' @description 
#' Calculates the Brier score for each bin frequency 
#' with the same sample size.
#' 
#' @param obj an object of class inheriting from "imbDis".
#' @return data.frame with columns: bins, Brier score, sample size.
#' @export
brier = function(obj) UseMethod("brier", obj)

#' @title Brier Score
#' 
#' @description 
#' Calculates the Brier score for each bin frequency 
#' with the same sample size.
#' 
#' @param obj an object of class inheriting from "imbDis".
#' @return data.frame with columns: bins, Brier score, sample size.
#' @export
brier.imbDis = function(obj){
    
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

#' @title Log Loss/Binary Cross-Entropy
#' 
#' @description 
#' Calculates the log loss for each bin frequency 
#' with the same sample size.
#' 
#' @param obj an object of class inheriting from "imbDis".
#' @return data.frame with columns: bins, log loss, sample size.
#' @export
logLoss = function(obj) UseMethod("logLoss", obj)

#' @title Log Loss/Binary Cross-Entropy
#' 
#' @description 
#' Calculates the log loss for each bin frequency 
#' with the same sample size.
#' 
#' @param obj an object of class inheriting from "imbDis".
#' @return data.frame with columns: bins, log loss, sample size.
#' @export
logLoss.imbDis = function(obj){
    
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
        
        # 0 and 1 values replaced with an epsilon and 1-epsilon
        # It is necessary in order to handle 0-1 perfectly fit probabilities
        # when calculating logs
        
        eps = .Machine$double.eps
        probs = pmax(pmin(probs, 1 - eps), eps) # replace 0 with eps and 1 with 1-eps
        
        logloss[i] = -mean(labs*log(probs) + (1-labs)*log(1-probs))
        n_samps[i] = length(probs)
    }
    
    # return data frame
    return(data.frame(bins, logloss, n_samps))
}

#' @title Other metrics framework
#' 
#' @description 
#' Calculates a user-input metric for each bin frequency with the same sample size.
#' Allows imbDis metric calculation using the predicted labels instead of probabilities.
#' 
#' @param obj an object of class inheriting from "imbDis".
#' @param f A function that must be of the format f(labels, preds).
#' @return data.frame with columns: bins, metric, sample size.
#' @export
manualMetric = function(obj, f) UseMethod("manualMetric", obj)

#' @title Other metrics framework
#' 
#' @description 
#' Calculates a user-input metric for each bin frequency with the same sample size.
#' Allows imbDis metric calculation using the predicted labels instead of probabilities.
#' 
#' @param obj an object of class inheriting from "imbDis".
#' @param f A function that must be of the format f(labels, preds).
#' @return data.frame with columns: bins, metric, sample size.
#' @export
manualMetric.imbDis = function(obj, f){
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
