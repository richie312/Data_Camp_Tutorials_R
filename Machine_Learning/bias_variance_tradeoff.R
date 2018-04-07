# The spam filter that has been 'learned' for you
spam_classifier <- function(x){
  prediction <- rep(NA, length(x)) # initialize prediction vector
  prediction[x > 4] <- 1 
  prediction[x >= 3 & x <= 4] <- 0
  prediction[x >= 2.2 & x < 3] <- 1
  prediction[x >= 1.4 & x < 2.2] <- 0
  prediction[x > 1.25 & x < 1.4] <- 1
  prediction[x <= 1.25] <- 0
  return(factor(prediction, levels = c("1", "0"))) # prediction is either 0 or 1
}

# Apply spam_classifier to emails_full: pred_full
pred_full<-spam_classifier(emails_full$avg_capital_seq)

# Build confusion matrix for emails_full: conf_full
conf_full<-table(emails_full$spam,pred_full)


# Calculate the accuracy with conf_full: acc_full

acc_full<-(sum(conf_full[1,1])+sum(conf_full[2,2]))/sum(conf_full)

# Print acc_full

print(acc_full)

##### Increasing the bias 


# The all-knowing classifier that has been learned for you
# You should change the code of the classifier, simplifying it
spam_classifier <- function(x){
  prediction <- rep(NA, length(x))
  prediction[x > 4] <- 1
  prediction[x <= 4] <- 0
  return(factor(prediction, levels = c("1","0")))
}

# conf_small and acc_small have been calculated for you
conf_small <- table(emails_small$spam, spam_classifier(emails_small$avg_capital_seq))
acc_small <- sum(diag(conf_small)) / sum(conf_small)
acc_small

# Apply spam_classifier to emails_full and calculate the confusion matrix: conf_full
conf_full <- table(emails_full$spam, spam_classifier(emails_full$avg_capital_seq))

# Calculate acc_full
acc_full <- sum(diag(conf_full)) / sum(conf_full)

# Print acc_full
acc_full

## "The model no longer fits the small dataset perfectly but it fits the big 
##dataset better. You increased the bias on the model and caused it to 
##generalize better over the complete dataset. While the first classifier 
##overfits the data, an accuracy of 73% is far from satisfying for a spam filter.
##Maybe you can provide us with a better model in a later exercise!"













