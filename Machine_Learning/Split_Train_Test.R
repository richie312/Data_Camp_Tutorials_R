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