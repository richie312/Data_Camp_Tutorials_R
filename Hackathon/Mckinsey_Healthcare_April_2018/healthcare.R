## Set wthe working directory
setwd("D:/Documents/Hackathon/Mckinsey_Healthcare_April_2018")

## Load the necessary packages

library(ROCR)
library(randomForest)
library(caret)
library(gbm)


## Load the dataset
train = read.csv("train.csv", stringsAsFactors = FALSE)
test = read.csv("test.csv", stringsAsFactors = FALSE)

head(train)

head(test)
## Convert few variables into factors

factor_list<- c("gender","hypertension","heart_disease","ever_married","work_type","Residence_type",
                "smoking_status","stroke")

train[,factor_list] <- lapply(train[,factor_list], factor)
str(train)

## Convert the variables in test set


factor_list_test<- c("gender","hypertension","heart_disease","ever_married","work_type","Residence_type",
                "smoking_status")


test[,factor_list_test]<-lapply(test[,factor_list_test], factor)

## Treating the Missing values

## Check whether any of the categorical variable have any missing values

any(is.na(train[,factor_list]))

## None of the categorical variable has missing values
## We will replace the numeric values by row means

train$bmi[is.na(train$bmi)] <- mean(train$bmi,na.rm=T)

any(is.na(train))

## Missing Values for test data set

## Check whether any of the categorical variable have any missing values

any(is.na(test[,factor_list_test]))

## None of the categorical variable has missing values
## We will replace the numeric values by row means

any(is.na(test$age))
any(is.na(test$bmi))
any(is.na(test$avg_glucose_level))

## Only replace the NAs of glucose column by row mean
test$bmi[is.na(test$bmi)] <- mean(test$bmi,na.rm=T)

## Check if there is any NA value still left
any(is.na(test))

## Set the seed

set.seed(1)

## Train the model

train_model<- randomForest(stroke~.,data = train)

## Print the model output

print(train_model)

## variable importance

varImpPlot(train_model)


## Drop few variables

train_1 = train[,c(-6,-4,-2,-8,-1)]

train_model_1<-randomForest(stroke~.,data = train_1, mtry = 2, nodesize = 3, sampsize = 30380)

print(train_model_1)

## Evaluate Out of Bag Error; No need of validation set as RF already creates OOB. 

err<-Model_RF$err.rate

## Find the last row of err

err<-err[nrow(err),"OOB"]

plot(train_model_2)

## Set the number of trees to 50 as there is no sgnificant changes

set.seed(1)              
res <- tuneRF(x = subset(train_1, select = -stroke),
              y = train[,12],
              ntreeTry = 50)

# Look at results
print(res)

# Find the mtry value that minimizes OOB Error
mtry_opt <- res[,"mtry"][which.min(res[,"OOBError"])]
print(mtry_opt)

## Tuning a Random Forest via mtry

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
  model <- randomForest(stroke ~ ., 
                        data = train_1,
                        mtry = hyper_grid$mtry[i],
                        nodesize = hyper_grid$nodesize[i],
                        sampsize = hyper_grid$sampsize[i],
                        ntrees = 50)
  
  # Store OOB error for the model                      
  oob_err[i] <- model$err.rate[nrow(model$err.rate), "OOB"]
}

# Identify optimal set of hyperparmeters based on OOB error
opt_i <- which.min(oob_err)
optimal_grid_options<-print(hyper_grid[opt_i,])

## Remodelling with above optimal hyperparameter


train_model_2<-randomForest(stroke~.,data = train_1, mtry = 1, nodesize = 3, sampsize = 30380)

## Evaluate model on the test set

prob_prediction_1<- as.data.frame(predict(object = train_model_2,
                          newdata = test,
                          type = "prob"))


## Create the dataframe for sample file
colnames(prob_prediction)<- c("Not Happening", "Happening")

sample<-cbind(test$id,prob_prediction$Happening)
colnames(sample)<- c("id", "stroke")
head(sample)
any(is.na(sample))
## Write the sample_submission file

write.csv(sample,"sample_submission.csv")









