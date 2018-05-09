
## p-significance test
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

p.mat <- cor.mtest(mtcars)
head(p.mat[, 1:5])

## Mode Function

Mode <- function(x) { 
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))] 
}

## Split the dataset


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

## User defined funciton for MOdel (GBM and Bagging),


model = function(data=data,algo = gbm ,formula = formula,distribution = 'bernoulli', 
                 type = 'response', set='AUC',n.trees=10000, cv.fold=cv.fold, newdata=newdata){
  ## Fit the model
  
  model<- algo(formula = formula, 
               distribution = distribution,
               data = data,
               n.trees = n.trees,
               cv.fold= cv.fold)
  
  ## Generate the prediction on the test set
  
  pred<- predict(object = model,
                 newdata = newdata,
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


get_model<- function(data=data,newdata=newdata,algo,formula = formula, type = type, ntrees = 5000,cv.fold=cv.fold){
  z= model(algo = algo, type= type, set = 'model',data=data,cv.fold=cv.fold, newdata = newdata, type = 'response')
  
}