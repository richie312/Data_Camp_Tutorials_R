## Load the dataset
Titanic

## Set the seed

set.seed(1)

## Shuffle the dataset
n<-nrow(Titanic)
shuffled<-Titanic[sample(n),]

## Activate Accuracy vector

accs<-rep(0,6)

## Create a loop to create 6 fold validation test set

for (i in 1:6){
  
  ## Create the indices for each th fold
  
  indices <- (((i-1) * round((1/6)*nrow(shuffled))) + 1):
    ((i*round((1/6) * nrow(shuffled))))
  
  ## Exclude the test indices from the train dataset,
  
  train<-shuffled[-indices,]
  
  ## Include them in the test set,
  
  test<-shuffled[indices,]
  
  ## Fit the model with tree
  
  tree=rpart(Survived~.,train,method='class')
  
  ## Make the prediction
  
  pred <- predict(tree, test, type = 'class')
  
  ## Create the confusion matrix
  
  conf<-table(Titanic$shuffled, pred)
  
  # Assign the accuracy of this model to the ith index in accs
  
  accs[i]- sum(diag(conf))/sum(conf)
}

