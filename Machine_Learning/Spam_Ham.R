## Read the data Set

## user defined function for spam classifier

spam_classifier<-function(x){
  prediction<- rep(NA, lenght(x))  ## Initialise the prediction vector
  prediction[x>4]<-1
  prediction[x>=3 & x<=4]<- 0
  prediction[x >= 2.2 & x < 3] <- 1
  prediction[x >= 1.4 & x < 2.2] <- 0
  prediction[x > 1.25 & x < 1.4] <- 1
  prediction[x <= 1.25] <- 0
  return(prediction) # prediction is either 0 or 1
}

# Apply the classifier to the avg_capital_seq column: spam_pred
spam_pred<-sam_classifier(emails$avg_capital_seq)

# Compare spam_pred to emails$spam. Use ==

spam_pred == emails$spam