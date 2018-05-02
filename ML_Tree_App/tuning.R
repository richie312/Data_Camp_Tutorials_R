## Cross validation

myFolds <- createFolds(train, k = 5)

# Create reusable trainControl object: myControl
myControl <- trainControl(
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE,
  savePredictions = TRUE,
  index = myFolds
)

levels(train$default) <- make.names(levels(factor(train$default)))

model_rf <- train(
  x = train, y = train$default,
  metric = "ROC",
  method = "ranger",
  trControl = myControl
)

summary(model_rf)
print(model_rf)
plot(model_rf)

prediction<-predict(object = model_rf,
                    newdata = test$default,
                    trees = 1000,
                    type = 'prob')


pred<-prediction(model_rf$pred[4], test$default)
roc<-performance(pred,"tpr","fpr")
plot(roc, main = "Test Set ROC Curves")
legend(x = "bottomright", 
       legend = legend)




