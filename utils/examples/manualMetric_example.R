# Run random_forest.R before running the code below

accuracy = function(label, pred){
    MLmetrics::Accuracy(pred, label)
}

pred01 = predict(RF_all$finalModel, test_dat)
imbD_01 = imbDis(test_dat$y, strtoi(pred01), 1)

manualMetric(imbD_01, accuracy)

