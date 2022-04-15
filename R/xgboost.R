control <- trainControl(method='cv', 
                        number=5)

set.seed(123654)
rf_random <- train(default ~ .,
                   data = train,
                   method = 'xgbTree',
                   metric = 'Accuracy',
                   tuneLength  =  10, 
                   trControl = control)
print(rf_random)

#### Treino

test_predict <- predict(rf_random, train)

pred_class <- as.factor(ifelse(test_predict > .5, "1", "0"))

cmtrx <- confusionMatrix(test_predict,train$default,positive = "1");cmtrx


##### Teste

test_predict <- predict(rf_random, test)

pred_class <- as.factor(ifelse(test_predict > .5, "1", "0"))

cmtrx <- confusionMatrix(test_predict,test$default,positive = "1");cmtrx

