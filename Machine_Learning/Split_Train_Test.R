## Load the dataset

mtcars

# Set random seed. Don't remove this line.
set.seed(1)

# Shuffle the dataset, call the result shuffled
n <- nrow(mtcars)
shuffled <- mtcars[sample(n),]

# Split the data in train and test
train_indices<-1:round(0.7*n)
train<-shuffled[train_indices,]
test_indices<-(round(0.7*n)+1):n
test<-shuffled[test_indices,]

## load the "graphics package in order to load the titanic dataset

require(graphics)
library(rpart)
Titanic
# The titanic dataset is already loaded into your workspace

# Set random seed. Don't remove this line.
set.seed(1)

# Shuffle the dataset; build train and test
n <- nrow(titanic)
shuffled <- titanic[sample(n),]
train <- shuffled[1:round(0.7 * n),]
test <- shuffled[(round(0.7 * n) + 1):n,]

# Fill in the model that has been learned.
tree <- rpart(Survived ~ ., train, method = "class")

# Predict the outcome on the test set with tree: pred
pred<- predict(tree,test, type='class')

# Calculate the confusion matrix: conf

conf<-table(test$Survived, pred)

# Print this confusion matrix
print(conf)
