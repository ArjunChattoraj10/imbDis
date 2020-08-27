#' @title imbDis object
#' 
#' @description 
#' imbDis - Imbalanced Discrimination.
#' This function creates an object of type imbDis, an S3 Class. 
#' imbDis objects can be passed into the associated methods as arguments.
#' 
#' @param labels Vector of ground-truth labels
#' @param pred Vector of predicted probabilities
#' @param case An integer or string that is the case class. Must be a value in labels.
#' @param bins Vector of different class imbalance frequencies. Default is seq(0.05, 0.5, 0.05)
#' @return simMetric object

imbDis = function(labels, pred, case, bins = seq(0.05,0.5,0.05)){
    
    # check values
    .param_check(labels, pred, case, bins)
    
    # initialize the class via list object using provided arguments
    obj = list(labels = labels, pred = pred, case = case, bins = bins)
    
    attr(obj, "class") = "imbDis" # sets the class
    
    # define control and add to list
    obj$control = unique(labels[labels != case])
    
    # define standardized labels and add to list
    obj$labs_01 = .standardizeLabels(labels, case)
    
    # define sample size and bin case sizes
    obj$sample.size = .calc_samplesize(obj$labs_01, bins)
    obj$bin.caseSizes = round(bins*obj$sample.size)
    
    return(obj)
}

#' @title Parameter Check
#' 
#' @description 
#' Helper function to ensure parameters are valid.
#' 
#' @param labels Vector of ground-truth labels
#' @param pred Vector of predicted probabilities
#' @param case An integer or string that is the case class. Must be a value in labels.
#' @param bins Vector of different class imbalance frequencies. Default is seq(0.05, 0.5, 0.05)
 
.param_check = function(labels, pred, case, bins){
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

#' @title Standardized Labels
#' 
#' @description 
#' Helper function to convert labels vector to 0-1 labels. 1 corresponds to the case label.
#' 
#' @param labels Vector of ground-truth labels
#' @param case An integer or string that is the case class. Must be a value in labels.
#' @return Standardized labels vector

.standardizeLabels = function(labels, case){
    # converts a binary vector of labels  to a vector
    # where all case values are 1 and controls are 0
    labels_01 = rep(0, length(labels))
    labels_01[labels == case] = 1
    return(labels_01)
}

#' @title Standardised Labels
#' 
#' @description 
#' Helper function to convert labels vector to 0-1 labels. 1 corresponds to the case label.
#' 
#' @param labels Vector of ground-truth labels
#' @param case An integer or string that is the case class. Must be a value in labels.
#' @return Standardised labels vector

.standardiseLabels = .standardizeLabels # alternate spelling

#' @title Calculates the sample size
#' 
#' @description 
#' Helper function to convert labels vector to 0-1 labels. 1 corresponds to the case label.
#' 
#' @param labels Vector of ground-truth labels
#' @param case An integer or string that is the case class. Must be a value in labels.
#' @return Sample size integer
 
.calc_samplesize = function(labels_01, bins){
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

#' @title Area under the Receiver Operator Curve (ROC AUC)
auc = function(obj) UseMethod("auc")

#' @title Area under the Receiver Operator Curve (ROC AUC)
#' 
#' @description 
#' Calculates the Area under the Receiver Operator Curve (ROC AUC) for each bin frequency 
#' with the same sample size.
#' 
#' @param obj An object of type imbDis
#' @return data.frame with columns: bins, AUC, sample size
 
auc.imbDis = function(obj){
    
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
        roc_i = suppressMessages(pROC::roc(labs, probs)) # default control = 0, case = 1
        auroc[i] = roc_i$auc
        n_samps[i] = length(probs)
    }
    
    # return data frame
    return(data.frame(bins, auroc, n_samps))
}

#' @title Brier Score
brier = function(obj) UseMethod("brier")

#' @title Brier Score
#' 
#' @description 
#' Calculates the Brier score for each bin frequency 
#' with the same sample size.
#' 
#' @param obj An object of type imbDis
#' @return data.frame with columns: bins, Brier score, sample size
 
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
logLoss = function(obj) UseMethod("logLoss")

#' @title Log Loss/Binary Cross-Entropy
#' 
#' @description 
#' Calculates the log loss for each bin frequency 
#' with the same sample size.
#' 
#' @param obj An object of type imbDis
#' @return data.frame with columns: bins, log loss, sample size

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
manualLoss = function(obj, f) UseMethod("manualLoss")

#' @title Other metrics framework
#' 
#' @description 
#' Calculates a user-input metric for each bin frequency with the same sample size.
#' Allows imbDis metric calculation using the predicted labels instead of probabilities.
#' 
#' @param obj An object of type imbDis
#' @param f A function that must be of the format f(labels, preds)
#' @return data.frame with columns: bins, metric, sample size

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
