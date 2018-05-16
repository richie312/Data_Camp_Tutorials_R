
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
get_dataset<- function(data_x, split_ratio = 0.8, set = 'train'){
  
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

get_ROC = function(pred_list, actual = test$default, legend){
  pred<-prediction(pred_list, actual)
  roc<-performance(pred,"tpr","fpr")
  plot(roc, main = "Test Set ROC Curves")
  legend(x = "bottomright", 
         legend = legend)
  
}
