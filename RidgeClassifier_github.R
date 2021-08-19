

# logistic regression with ridge regularization technique ####

# load the packages ####
library(stringr)
library(R.matlab)
library(viridis)
library("lattice")
library(robustbase)
library(matrixStats)
require(AUC)
library(AUC)
library(cvTools)
library(caret)
library(CrossValidate)
library("doParallel")
library(doParallel)
library(doMC)
library(devtools)
library(DMwR)
library("foreach")
library(foreach)
library(pROC)
library(PRROC)
library(plotly)
library(ROCR)
library(rminer)
library(rpart)
library(randomForest)
library(R.utils)
library(R.matlab)
library(ROSE)
library(iterators)
library(elasticnet)
library(e1071)
library(glmnet)
library(ggplot2)
library(grid)
library(lattice)
library(MASS)
library(mclust)
library(nnet)
library(neuralnet)
library(stringr)
library(R.matlab)
library(viridis)
library("lattice")
library(robustbase)
library(matrixStats)
library(vioplot)

# start for cross validation - Ridge  ####
data_used1<-list(firstmodel, # first column  (Output_class) is the class (binary-lower vs  greater disability) and the rest are the variables 
                 secondmodel # first column (Output_class) is the class (binary-lower vs  greater disability) and the rest are the variables 
                 )
length(data_used1) 

for(i in 1:(length(data_used1))){print(dim(data_used1[[i]]))}
lambdainterval<-10^(-3:3)

# empty matrices
{

  predict_all_outer<-NULL;predict_all_connect_all_outer<-NULL;predict_all_ensemble_all_outer<-NULL
  varImp_all_outer<-list();varImp_SC_FC_all_outer<-NULL;varImp_connect_all_outer<-NULL
  auc_outer_outerloop<-NULL; outer_auc_connect_all_outer<-NULL; outer_auc_ensemble_all_outer<-NULL
  best_hyper_outer<-NULL; best_hyper_connect_outer<-NULL; best_hyper_ensemble_outer<-NULL
  best_threshold_all_outer<-NULL;best_threshold_all_connect_all_outer<-NULL;best_threshold_all_ensemble_all_outer<-NULL
  result_ConfMatrix_outer<-NULL;result_ConfMatrix_outer_connect_all_outer<-NULL;result_ConfMatrix_outer_ensemble_all_outer<-NULL
  err_cv_outer<-NULL;err_cv_connect_outer<-NULL;err_cv_ensemble_outer<-NULL
  dim_data_outer<-NULL;result_ConfMatrix_outer1<-NULL
  auc_single_ensemble_outerloop<-NULL
  result_ConfMatrix_outer05<-NULL
  result_ConfMatrix_outer05_1<-NULL
  result_ConfMatrix_outerAUC<-NULL
  result_ConfMatrix_outerAUC_1<-NULL;varImp_SC_FClist<-list()
  aucpr_outer_outerloop<-NULL;brierscore_allmodels<-NULL;predicted_observed_brier_outerloop<-NULL
  mean_pred_ensemble_outer12345_weighted<-NULL;predict_all_ensemble_all_outer_weighted<-NULL;cor_pred<-NULL
  predict_test_enet_class_outer<-NULL;predict_test_enet_class_outer_sensitivty<-NULL
  OuterKfold<-5;InnerKfold<-5;InnerIterNumber<-5
    varImp_SC_FC<-NULL
}

OuterKfold<-100
for(outerloop in 1:OuterKfold){
  cat(paste("outerloop=",outerloop))
  varimplistnumber<-1
  # Split the data in 10 partitions
  auc_outer_outerloop_onlyforthisouterloop<-NULL
  aucpr_outer_outerloop_onlyforthisouterloop<-NULL
  
  folds_outerloop<-createFolds(factor(as.data.frame(data_used1[[1]])$Output_class),k=OuterKfold,list = FALSE)  
  
  for(model in 1:length(data_used1)){
    data_used<-data_used1[[model]]
    data_used<-as.data.frame(data_used)
    names1<-names(data_used)
    names(data_used)<-make.names(names1, unique = TRUE, allow_ = TRUE)
    data_used$Output_class<-as.factor(data_used$Output_class) 
    
    # Create a train dataset using 4 partitions over 5                                                           
    trainData <- data_used[folds_outerloop != outerloop, ]
    
    # Create a test dataset using 1 partition over 5                                                           
    testData <- data_used[folds_outerloop == outerloop, ]
    
    err_cv<-NULL
    # Start inner loop ####
    for (iterinner in 1:InnerIterNumber) {
      # cat(paste("iterinner=",iterinner))
      
      # Create partitions from train dataset
      folds_outerloop_inner<-createFolds(factor(trainData$Output_class),k=InnerKfold,list = FALSE)
      
      for(innerloop in 1:InnerKfold){
        
        trainingData <- trainData[folds_outerloop_inner != innerloop, ]
        validationData <- trainData[folds_outerloop_inner == innerloop, ]
        dim(validationData)
        
        # normalize the data using 0 as mean
        normParam_training <- preProcess(trainingData,method = c("center", "scale"))
        trainingData <- predict(normParam_training, trainingData)
        validationData <- predict(normParam_training, validationData)
        
        # smote function to balance the dataset
        trainingData <- SMOTE(Output_class ~ ., trainingData, perc.over = 100)
        # remove missing data if you have
        trainingData<-na.omit(trainingData)
        
        
        for (lambda1 in lambdainterval) {
          # input and output datasets
          x<-data.matrix(trainingData[,-which(names(trainingData) %in% "Output_class")]) # input = predictors
          y<-data.matrix(trainingData[,which(names(trainingData) %in% "Output_class")]) # output =class = the one you want to predict
          
          # Fit the model using lambda
          innermodel_enet_prob <- glmnet(x,y,lambda=lambda1,alpha=0,family = "binomial",standardize = FALSE) 
          
          # Predict the validation dataset
          validationData$Output_class<-NULL
          predict_validation_prob<-predict(innermodel_enet_prob,newx = data.matrix(validationData),s=lambda1,type="response") 
          
          validationData <- trainData[folds_outerloop_inner == innerloop, ]
          
          # AUC values to check the performance of the classification method
          auc_inner <- ROSE::roc.curve(validationData$Output_class, predict_validation_prob,plotit = FALSE)$auc
          err_cv<-rbind(err_cv,c(iterinner,innerloop,lambda1,auc_inner,model))
          
        }
        
      }
      
    }
    err_cv_outer<-rbind(err_cv_outer,err_cv)
    
    # End inner loop ####
    
    # redefining the train and test datasets
    trainData <- data_used[folds_outerloop != outerloop, ]
    testData <- data_used[folds_outerloop == outerloop, ]
    
    trainData <- SMOTE(Output_class ~ ., trainData, perc.over = 100)
    trainData<-na.omit(trainData)
    
    
    # Best hyperparameter ###
    param_median_auc<-NULL
    for(lambdabest in levels(as.factor(err_cv[,3]))){
      row1<-which(err_cv[,3]==lambdabest)
      param_median_auc<-rbind(param_median_auc,c(as.numeric(lambdabest),
                                                 as.numeric(median(err_cv[row1,4]))))
    }
    
    # find the best hyperparameter=lambda by maximizing the AUC
    best_hyper<-param_median_auc[which.max(param_median_auc[,2]),1]
    
    best_hyper_outer<-rbind(best_hyper_outer,best_hyper)
    
    # normalizing the data
    normParam_train <- preProcess(trainData,method = c("center", "scale"))
    trainData <- predict(normParam_train, trainData)
    testData <- predict(normParam_train, testData)
    
    # defining input and output datasets
    x<-data.matrix(trainData[,-which(names(trainData) %in% "Output_class")])
    y<-data.matrix(trainData[,which(names(trainData) %in% "Output_class")])
    
    # fitting the model using the BEST hyperparameter=lambda
    outermodel_enet_prob <- glmnet(x,y,lambda=best_hyper,alpha=0,family = "binomial",standardize = FALSE) 
    
    # variable importance from the model
    varImp_SC_FC<-rbind(varImp_SC_FC,as.matrix(coef(outermodel_enet_prob,s=best_hyper)))
    varImp_SC_FClist[[varimplistnumber]]<-as.matrix(coef(outermodel_enet_prob,s=best_hyper))
    varimplistnumber<-varimplistnumber+1
    
    varImp_all_outer<-rbind(varImp_all_outer,as.matrix(coef(outermodel_enet_prob,s=best_hyper)))
    
    # Predict the test dataset (1/5 of the subjects)
    testData$Output_class<-NULL
    predict_test_prob<-predict(outermodel_enet_prob,newx = data.matrix(testData),s=best_hyper,type="response") 
    
    # save all results
    predict_test_enet_class_outer_sensitivty<-list(predict_test_enet_class_outer_sensitivty,
                                                   cbind(predict_test_prob,
                                                         as.numeric(as.character(testData$Output_class)),
                                                         rep(model,length(predict_test_prob)),
                                                         rep(outerloop,length(predict_test_prob))))
    
    # prediction of the classes as binary outcomes
    testData$Output_class<-NULL
    predict_test_enet_class<-predict(outermodel_enet_prob,newx = data.matrix(testData),s=best_hyper,type="class") 
    
    # save the prediction of the model from the test datset
    predict_test_enet_class_outer<-list(predict_test_enet_class_outer,cbind(predict_test_enet_class,
                                                                            as.numeric(as.character(testData$Output_class)),
                                                                            rep(model,length(predict_test_enet_class)),
                                                                            rep(outerloop,length(predict_test_enet_class))))
    # compute AUC on test dataset
    testData <- data_used[folds_outerloop == outerloop, ]
    auc_inner <- ROSE::roc.curve(testData$Output_class, predict_test_prob,plotit = FALSE)$auc
    
    
    # Predict&Output Demo ###
    predict_all_outer<-rbind(predict_all_outer,cbind(predict_test_prob,
                                                     as.numeric(as.character(testData$Output_class)),
                                                     rep(model,length(predict_test_prob)),
                                                     rep(outerloop,length(predict_test_prob))))   
    
    # AUC
    auc_outer <- ROSE::roc.curve(testData$Output_class, predict_test_prob,plotit = FALSE)$auc
    
    # save AUC for all outer loop iterations
    auc_outer_outerloop_onlyforthisouterloop<-rbind(auc_outer_outerloop_onlyforthisouterloop,c(auc_outer,model=model,outerloop=outerloop))
    
    # Brier score
    squareofthedifference<-NULL
    for(i in 1:length(testData$Output_class)){
      squareofthedifference<-rbind(squareofthedifference,(predict_test_prob[i]-(as.numeric(testData$Output_class)[i]-1))*(predict_test_prob[i]-(as.numeric(testData$Output_class)[i]-1)))
    }
    brierscore<-sum(squareofthedifference)/length(testData$Output_class)
    #confusion matrix
    confusionMatrixresults<-confusionMatrix(as.factor(predict_test_enet_class), testData$Output_class,positive="1")
    
    # save all classification results 
    auc_outer_outerloop<-rbind(auc_outer_outerloop,c(auc_outer,outerloop,model,
                                                     brierscore,
                                                     confusionMatrixresults$byClass["Sensitivity"],
                                                     confusionMatrixresults$byClass["Specificity"],
                                                     confusionMatrixresults$byClass["Balanced Accuracy"],
                                                     confusionMatrixresults$byClass["Recall"],
                                                     confusionMatrixresults$byClass["F1"]))
    
    # Data dimensions  
    dim_data<-c(dim(data_used)[1],
                dim(data_used)[2],
                length(which(data_used$Output_class==1)),
                length(which(data_used$Output_class==0)),
                length(which(testData$Output_class==1)),
                length(which(testData$Output_class==0)),
                length(which(trainData$Output_class==1)),
                length(which(trainData$Output_class==0)))
    dim_data_outer<-rbind(dim_data_outer,dim_data)
    
    
  } ## end of the model loop
}## end of the outer loop

allresults_ridge_10outerloops_SC_FC_FS86atlas<-
  list(err_cv_outer,
                 varImp_all_outer,
                  predict_all_outer,
                  predict_test_enet_class_outer,
                 predict_all_outer,
                 auc_outer_outerloop_onlyforthisouterloop,
                 auc_outer_outerloop,dim_data_outer)

setwd("/change directory")
save("allresults_ridge_10outerloops_SC_FC_FS86atlas.RData")

setwd("/change directory")
load(file="allresults_ridge_10outerloops_SC_FC_FS86atlas.RData")
