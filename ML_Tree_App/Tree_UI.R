## Set the working directory
setwd("D:/Documents/R_Projects/Data_Camp_Tutorials/ML_Tree_App")

set.seed(123)
## load the dataset
data_x = read.csv("credit.csv")

## Preprocessing the dataset

data_x$default <- ifelse(data_x$default == "yes", 1, 0)

## split the dataset
get_dataset<- function(x, split_ratio = 0.8, set = 'train'){
  
  if (set == 'train'){
    n <- nrow(data_x)
    shuffled <- data_x[sample(n),]
    
    # Split the data in train and test
    train <- shuffled[1:round(split_ratio * n),]
    test <- shuffled[(round(split_ratio * n) + 1):n,]
    return(train)
    
  }
   if(set == 'test'){
    n <- nrow(data_x)
    shuffled <- data_x[sample(n),]
    
    # Split the data in train and test
    train <- shuffled[1:round(split_ratio * n),]
    test <- shuffled[(round(split_ratio * n) + 1):n,]
    return(test)
   }
  else
    return (NULL)

}

train = get_dataset(data_x, set = 'train')
nrow(train)

test= get_dataset(data_x, set = 'test')
nrow(test)


## load the necessary packages
pacman::p_load(shiny,shinydashboard,gbm, randomForest,ggplot2,ipred,caret,ROCR,dplyr,ModelMetrics) 


## User Defined Function for loading data, splitting and fitting the model


model = function(algo =gbm ,distribution = 'bernoulli', 
                 type = 'response', set='AUC',n.trees=10000){
  ## Fit the model
    
    model<- algo(formula = default ~ ., 
                 distribution = distribution,
                 data = data,
                 n.trees = n.trees,
                 cv.fold= 3)
    
    ## Generate the prediction on the test set
    
    pred<- predict(object = model,
                   newdata = test,
                   n.trees = n.trees,
                   type = type)
    
    ## Generate the test set AUCs using the pred
    
    AUC<- auc(actual = test$default, predicted = pred)
    
  
  
  if (set == 'AUC'){
    return(AUC)
  }
  
  if (set == 'predictions'){
    return(pred)
  }
  if (set == 'model'){
    return(model)
    
  }
  else
    return(NULL)
  
} 



## get the AUC for different algo

 get_auc= function(algo, type,n.trees){
    z = model(algo = algo,type = type, set = 'AUC')
}

 
GBM_auc = get_auc(algo = gbm, type='response')
RF_auc =  get_auc(algo = randomForest, type ='response')
BAG_auc = get_auc(algo = bagging, type ='class')

## Make a list of predictions

get_pred<- function(algo, type, n.trees =10000){
  
  z= model(algo = algo, type= type, set = 'predictions')
  
}

BAG_preds<- get_pred(algo = bagging, type= 'class')
RF_pred<- get_pred(algo = randomForest, type ='response')
GBM_pred<- get_pred(algo = gbm, type = 'response')


pred_list<-list(BAG_preds,RF_pred, GBM_pred)


## List of different models

get_model<- function(algo,type = 'response', ntrees = 10000){
  z= model(algo = algo, type= type, set = 'model')
  
}

Bag_model<- get_model(algo = bagging, type='prob')
RF_model<- get_model(algo = randomForest)
GBM_model<- get_model(algo = gbm)


## make a list of actual values for 3 sets

m<-length(pred_list)
actual_list<-rep(list(test$default),m)

## Plot the ROC Curvea

pred <- prediction(pred_list, actual_list)
roc <- performance(pred, "tpr", "fpr")
plot(roc, col = as.list(1:m), main = "Test Set ROC Curves")
legend(x = "bottomright", 
       legend = c("Bagged Trees", "Random Forest", "GBM"),
       fill = 1:m)

## user defined ROC Curve

get_ROC = function(pred_list, actual = test$default, legend){
  pred<-prediction(pred_list, actual)
  roc<-performance(pred,"tpr","fpr")
  plot(roc, main = "Test Set ROC Curves")
  legend(x = "bottomright", 
         legend = legend)
  
}

get_ROC(pred_list = GBM_pred,legend = "Random Forest")


## Tuning based on OOB (out of bag)

# Optimal ntree estimate based on cv
ntree_gbm_cv <- gbm.perf(object = GBM_model, 
                          method = "cv")


## get the AUC score after optimisation

GBM_A<-get_auc(algo = gbm,type='response',n.trees = ntree_gbm_cv)


## optimisation for Random Forest by grid search which imcludes mtry and ntrees

## To optimise random forest, response variable has to be converted into factors

## lets create seperate dataset for random Forest

train_rf<- train
test_rf<-test
train_rf$default<-as.factor(train_rf$default)

# Establish a list of possible values for mtry, nodesize and sampsize
mtry <- seq(1, ncol(train) * 0.8, 2)
nodesize <- seq(3, 8, 2)
sampsize <- nrow(train) * c(0.7, 0.8)

# Create a data frame containing all combinations 
hyper_grid <- expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

# Create an empty vector to store OOB error values
oob_err <- c()

# Write a loop over the rows of hyper_grid to train the grid of models
for (i in 1:nrow(hyper_grid)) {
  
  # Train a Random Forest model
  model <- randomForest(train_rf$default ~ ., 
                        data = train_rf,
                        distribution ='bernoulli',
                        mtry = hyper_grid$mtry[i],
                        nodesize = hyper_grid$nodesize[i],
                        sampsize = hyper_grid$sampsize[i],
                        ntrees = 10000)
  
  # Store OOB error for the model                      
  oob_err[i] <- model$err.rate[nrow(model$err.rate), "OOB"]
}

# Identify optimal set of hyperparmeters based on OOB error
opt_i <- which.min(oob_err)
optimal_grid_options<-print(hyper_grid[opt_i,])



## Runing the model with optimum set of values 

model_rf<-randomForest(train_rf$default ~ ., 
                                    data = train_rf,
                                    distribution ='bernoulli',
                                    mtry = optimal_grid_options[[1]],
                                    nodesize = hyper_grid$nodesize[[2]],
                                    sampsize = hyper_grid$sampsize[[3]],
                                    ntrees = 10000)
predict_opt<-predict(object = model,
                      newdata = test_rf,
                      n.trees = 10000,
                      type = 'response')

                       
model_rf_AUC<-auc(actual = test$default, predicted = predict_opt)


z<-randomForest(as.factor(train$default)~.,
                data = train)
predict_z<-predict(object = model,
                   newdata = test,
                   type='prob')

predict_z_auc<-auc(actual = test$default, predicted = predict_opt[,2])
























