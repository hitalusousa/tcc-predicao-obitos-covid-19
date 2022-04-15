library(randomForestSRC)
library(caret)

data(train, package = "randomForestSRC")
breast.obj <- rfsrc(default ~ ., data = train, nsplit = 10)
breast.pred <- predict(breast.obj, train)

#### Treino
pred_class <- as.factor(ifelse(breast.pred$predicted[,2] > .5, "1", "0"))

cmtrx <- confusionMatrix(pred_class,train$default,positive = "1");cmtrx

### teste
breast.pred <- predict(breast.obj, test)

pred_class <- as.factor(ifelse(breast.pred$predicted[,2] > .5, "1", "0"))

cmtrx <- confusionMatrix(pred_class,test$default,positive = "1");cmtrx

