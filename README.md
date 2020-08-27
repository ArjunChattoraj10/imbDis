# imbDis - Imbalanced Discrimination

An S3 Class object that creates subset of the testing set labels and calculate various discrimination metric, either built-in or user-defined, maintaining a consistent sample size for all metrics and bin frequencies.

## Motivation

In the world of Machine Learning, many applications rely on Binary Classification algorithms, which answer questions such as, *“Is that symbol on the road a stop sign?”*, *“Will the company make a profit or a loss this quarter?”*, or *“Is the patient at risk of a heart attack?”.* Logistic Regression, Support Vector Machines, Decision Trees, Random Forests and Neural Networks are a few examples of Binary Classifiers, and their effectiveness is measured via various discrimination metrics, such as the ROC AUC, Brier Score, Cross-Entropy loss, etc. 

However, one of the issues that exists is a lack of tools that measure the quality of the model for different populations. That is **when the proportion of the outcome classes are different from the dataset that the model was trained on**. 

**imbDis** is a solution to that. It varies the class imbalance and calculates discrimination metrics for each imbalance frequency. The discrimination metrics may be a pre-defined method within `imbDis` - such as AUC, Brier and Log Loss, or can be other functions from any package or user-defined.

## Usage

To build the class, use the constructor function `imbDis()`. This function has 4 arguments:

- `labels`: A vector of the ground-truth class labels.
- `pred`: A vector of values corresponding to predictions. These values should be probabilities, but may be predicted labels as well.
- `case`: A value present in the `labels` argument, that denotes the case class label.
- `bin`: A vector of values (larger than 0 and smaller than 1) that represent the different class imbalance frequencies.  It has a default value of `seq(0.05, 0.5, 0.05)` which is a vector of values from 0.05 to 0.5 separated by 0.05.

To define the object, run the following command:

```
imbDis(labels, pred, case, bin)
```

