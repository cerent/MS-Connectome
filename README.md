# MS-Connectome

We provided the Random Forest codes and library that are applicable on R version 3.4.4. The codes can be used for any two class problems.

The model was trained with outer and inner loops of k-fold cross validation (k = 5) to optimize the hyperparameters and test model performance. The folds for both inner and outer loops were stratified to ensure that each fold contained the same proportion of subjects in the two classes as the original dataset. The inner loop (repeated over 10 different partitions of the training dataset only) optimized the set of hyperparameters that maximized validation AUC. A model was then fitted using the entire training dataset and those optimal hyperparameters, which was then assessed on the hold-out test set from the outer loop. The outer loop was repeated for 100 different random partitions of the data. 


