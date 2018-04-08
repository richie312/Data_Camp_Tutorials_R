### List operators

# 2nd element in tricky_list
typeof(tricky_list[[2]])
# Element called x in tricky_list

typeof(tricky_list['x'])
# 2nd element inside the element called x in tricky_list
typeof(tricky_list[['x']][[2]])

## Get the specific elements of the list

# Guess where the regression model is stored
names(tricky_list)

# Use names() and str() on the model element
names(tricky_list[['model']])
str(tricky_list[['model']])

# Subset the coefficients element
tricky_list[['model']][[1]]

# Subset the wt element
tricky_list[['model']][[1]][[2]]


#######  Store the output of the function in a object

# Replace the 1:ncol(df) sequence
for (i in seq_along(df)) {
  print(median(df[[i]]))
}

# Change the value of df
df <- data.frame()

# Repeat for loop to verify there is no error
for (i in seq_along(df)) {
  print(median(df[[i]]))
}

## Store the output in a vector for i values and print them,

# Create new double vector: output
output<-vector("double",ncol(df))

# Alter the loop
for (i in seq_along(df)) {
  # Change code to store result in output
  output[[i]]<-median(df[[i]])
  
}

# Print output
print(output)

































































