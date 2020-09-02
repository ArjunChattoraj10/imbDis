

# imbDis - Imbalanced Discrimination

An S3 Class object that creates subset of the testing set labels and calculate various discrimination metric, either built-in or user-defined, maintaining a consistent sample size for all metrics and bin frequencies.

## Installation

Use the following code to install the package from Github. You will need the *devtools* package:

```R
devtools::install_github("ArjunChattoraj10/imbDis")
```

## Motivation

In the world of Machine Learning, many applications rely on Binary Classification algorithms, which answer questions such as, *“Is that symbol on the road a stop sign?”*, *“Will the company make a profit or a loss this quarter?”*, or *“Is the patient at risk of a heart attack?”.* Logistic Regression, Support Vector Machines, Decision Trees, Random Forests and Neural Networks are a few examples of Binary Classifiers, and their effectiveness is measured via various discrimination metrics, such as the ROC AUC, Brier Score, Cross-Entropy loss, etc. 

However, one of the issues that exists is a lack of tools that measure the quality of the model for different populations. That is **when the proportion of the outcome classes are different from the dataset that the model was trained on**. 

**imbDis** is a solution to that. It varies the class imbalance and calculates discrimination metrics for each imbalance frequency. The discrimination metrics may be a pre-defined method within `imbDis`, or can be other functions from any package or user-defined.

## Usage

To build the class, use the constructor function `imbDis()`. This function has 4 arguments:

- `labels`: A vector of the ground-truth class labels.
- `pred`: A vector of values corresponding to predictions. These values should be probabilities, but may be predicted labels as well.
- `case`: A value present in the `labels` argument, that denotes the case class label.
- `bin`: A vector of values (larger than 0 and smaller than 1) that represent the different relative frequencies of the case class. It has a default value of `seq(0.05, 0.5, 0.05)` which is a vector of values from 0.05 (5%) to 0.5 (50%) separated by 0.05.

To define the object, run the following command:

```R
imbDis(labels, pred, case, bin)
```

*imbDis* performs some additional calculations in the background to obtain values that are helpful during the metric calculations. These values can be accessed using the `$` notation or the `[["..."]]` notation. 

These additional values are:

- `control`: This is the control label present in the `labels`.
- `labs_01`: This is a conversion of the provided `labels` to a 0-1 vector, using the `case` argument.
- `sample.size`: This value is a consistent sample size for all metrics and bin frequencies.
- `bin.caseSizes`: The number of cases that can be attributed to each bin frequency.

After the `imbDis`-type object is created, you can calculate the associated metrics. Built-in methods include `auc`, `brier` and `logLoss`. Each of these simply take in the `imbDis` object as the argument and return a `data.frame` with the columns: bins, metric, number of samples. 

There is also a method `manualMetric` which takes in as arguments the `imbDis` object and also a function in the format: `f(labels, preds)`. It is present to allows more freedom to perform different types of analyses.  As long as you maintain that format, the `manualMetric` method will perform as expected. The `manualMetric` can also let you use predicted labels instead of probabilities.

## Example

Here is a simple example using the `mtcars` dataset from Base R.

We will start by defining the model, which in this case is a *Logistic Regression* model. The `vs` variable is binary, so we can use that as response. After we train the model, we can obtain predict. Here we are predicting on the same dataset we trained on, which is equivalent to calculating the model's fitted values. 

```R
LR = glm(vs ~ mpg + hp + wt, data = mtcars, family = "binomial")
preds = predict(LR, mtcars, type = "response") 
```

Now that we have the model and the prediction, we can define an `imbDis` object. We provide the following arguments:

- **labels**: `mtcars$vs`
- **pred**: `preds`
- **case**: `1`
- **bin**: `seq(0.1, 0.9, 0.1)`

```R
imbD = imbDis(mtcars$vs, preds, 1, seq(0.1, 0.9, 0.1))
```

Now we can calculate the metrics for this object. Let's calculate the *Brier Score* here:

```R
brier(imbD)
```

The output is below. There is randomization involved so values may not be the same as below:

```
  bins      brier n_samps
1  0.1 0.11136114      16
2  0.2 0.06845671      16
3  0.3 0.04766746      16
4  0.4 0.12044719      16
5  0.5 0.04166886      16
6  0.6 0.08927128      16
7  0.7 0.05264758      16
8  0.8 0.08150856      16
9  0.9 0.05096148      16
```

## More Examples

More examples are within the *utils* directory of this repository. Within it, you will find the folder called *examples*. In *examples*, there are multiple .R files that simulate data, split into training/testing sets, train models and predict results, which are then evaluated using *imbDis*.

The files present are:

- *data_simulate.R*: This file simulates data from a logistic regression model. There are 10 covariates: some are linears, some have interactions and others are polynomial. Some plots are present to show how the data is distributed. The simulated data is saved in the *data* folder in *utils* under the name *simulated_data.csv*, a comma-separated data file.

- *train_test.R*: A file that reads in the simulated data and splits it such that 70% of it is the training and 30% is the testing set.

- *logistic_regression.R*, *decision_tree.R* & *random_forest.R*: These three files train Logistic Regression models, Decision Tree models and Random Forest models respectively. Each file trains multiple models using different subsets of the covariates. The prediction results from each model is saved in comma-separated value files within the *data* directory: *res_LR.R*, *res_DT.R* and *res_RF.R*.

  Since Decision Trees and Random Forests determine interaction and non-linearity via their algorithms, the files using those have two distinct models:

  - One using only 7 predictors.
  - One using all 10 predictors.

  However, Logistic Regression requires specification of non-linear terms. So, its file has three different models:

  - One using only 7 predictors on a linear scale.
  - One using all 10 predictors on a linear scale.
  - One using all 10 predictors, as well as interactions and polynomials.

  The last model is the model specification used to simulate the data. Thus, it is expected to perform the best out of all 7 models.  

## License

Package is under Apache License, Version 2.0. Refer to *LICENSE.txt* for additional information.

---

