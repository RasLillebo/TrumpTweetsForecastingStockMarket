
library(MASS)
library(randomForest)
## Random forest
set.seed(1)
train_rf1 <- sample(1:nrow(d_NNData_S),nrow(d_NNData_S)*0.75)
test_rf1 <- d_NNData_S[-train_rf1,"d_SPY"]

# We now use a smaller number of potential predictors
RF.Sentiment <- randomForest(d_SPY~d_NegS + d_PosS + d_NeuS, data=d_NNData_S, subset=train_rf1,importance=TRUE )

mean(RF.Sentiment$mse)
(1 - sum((train_rf1-RF.Sentiment$predicted)^2)/sum((train_rf1-mean(train_rf1))^2))
# List the content of the container
importance(RF.Sentiment)
varImpPlot(RF.Sentiment)

#Quant
set.seed(1)
train_rf2 <- sample(1:nrow(d_NNData_Q),nrow(d_NNData_Q)*0.75)
test_rf2 <- d_NNData_Q[-train_rf2,"d_SPY"]

# We now use a smaller number of potential predictors
RF.Quant <- randomForest(d_SPY~d_Ret + d_Fav + d_Freq, data=d_NNData_Q, subset=train_rf2,importance=TRUE)

mean(RF.Quant$mse)
(1 - sum((train_rf2-RF.Quant$predicted)^2)/sum((train_rf2-mean(train_rf2))^2))
# List the content of the container
importance(RF.Quant)
varImpPlot(RF.Quant)

windows()
par(mfrow=c(2,1))
boxplot(RF.Sentiment$mse,xlab='MSE',col='cyan',
        border='blue',names='MSE',
        main='MSE for RF Sentiment',horizontal=TRUE)
boxplot(RF.Quant$mse,xlab='MSE',col='cyan',
        border='blue',names='MSE',
        main='MSE for RF Quant',horizontal=TRUE)
