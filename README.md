# MS-Connectivity-Disconnectivity

We provide the R codes that perform the logistic regression with the ridge regularization technique and the libraries that are applicable on version 3.4.4. The codes provided here can be used for any binary classification problems. The model which is presented here is trained with outer and inner loops of 5-fold cross validation to optimize the hyperparameters and test model performance. The folds for both inner and outer loops were stratified to ensure that each fold contained the same proportion of subjects in the two classes as the original dataset. The inner loop (repeated over 5 different partitions of the training dataset only) optimized the set of hyperparameters that maximized validation AUC (area under the ROC curve). A model was then fitted using the entire training dataset and those optimal hyperparameters, which was then assessed on the hold-out test set from the outer loop. The outer loop was repeated for 100 different random partitions of the data. 

Please contact Ceren Tozlu (cet2005@med.cornell.edu) for any questions about the repository.




