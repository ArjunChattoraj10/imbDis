#' @title imbDis - Imbalanced Discrimination
#' 
#' @description 
#' This function creates an object of type \code{imbDis}, an S3 Class. 
#' imbDis objects can be passed into the associated methods as arguments.
#' Examples of associated methods are \code{auc}, \code{brier} and \code{logLoss}.
#' 
#' @param labels Vector of ground-truth labels.
#' @param pred Vector of predicted probabilities.
#' @param case A value present in the \code{labels} argument, that denotes the case class label.
#' @param bins Vector of class imbalance frequencies. Values must be larger than 0 and smaller than 1. Default is seq(0.05, 0.5, 0.05).
#' 
#' @return A simMetric object - S3 class that contains the input parameters, standardized labels, 
#'     and sample size.
#' @export
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

# @title Parameter Check
# 
# @description 
# Helper function to ensure parameters are valid.
# 
# @param labels Vector of ground-truth labels
# @param pred Vector of predicted probabilities
# @param case An integer or string that is the case class. Must be a value in labels.
# @param bins Vector of different class imbalance frequencies. Default is seq(0.05, 0.5, 0.05)
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

# @title Standardized Labels
# 
# @description 
# Helper function to convert labels vector to 0-1 labels. 1 corresponds to the case label.
# 
# @param labels Vector of ground-truth labels.
# @param case An integer or string that is the case class. Must be a value in labels.
# @return Standardized labels vector.
.standardizeLabels = function(labels, case){
    # converts a binary vector of labels  to a vector
    # where all case values are 1 and controls are 0
    labels_01 = rep(0, length(labels))
    labels_01[labels == case] = 1
    return(labels_01)
}

# @title Standardised Labels
# 
# @description 
# Helper function to convert labels vector to 0-1 labels. 1 corresponds to the case label.
# 
# @param labels Vector of ground-truth labels.
# @param case An integer or string that is the case class. Must be a value in labels.
# @return Standardised labels vector.
.standardiseLabels = .standardizeLabels # alternate spelling

# @title Calculates the sample size
# 
# @description 
# Helper function to convert labels vector to 0-1 labels. 1 corresponds to the case label.
# 
# @param labels Vector of ground-truth labels.
# @param case An integer or string that is the case class. Must be a value in labels.
# @return Sample size integer.
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

