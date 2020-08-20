#NN Quant

library(neuralnet)
NNData_Q = Total_df[c(1, 3:5)]
NNData_Q = na.exclude(NNData_Q)
#d_SPY <- diff(NNData_Q$SPY.Adjusted)
#d_Ret <- diff(NNData_Q$Retweet_count)
#d_Fav <- diff(NNData_Q$Favorite_count)
#d_Freq <- diff(NNData_Q$Frequency)
#d_NNData_Q <- data.frame(d_SPY, d_Ret, d_Fav, d_Freq)
d_NNData_Q <- NNData_Q
names(d_NNData_Q) <- c("d_SPY", "d_Ret", "d_Fav", "d_Freq")
NNN <- scale(d_NNData_Q)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
maxmindf_Q <- as.data.frame(lapply(d_NNData_Q, normalize))
smp_size <- floor(0.75 * nrow(NNN))
## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(maxmindf_Q)), size = smp_size)
train_Q <- maxmindf_Q[train_ind, ]
test_Q <- maxmindf_Q[-train_ind, ]

set.seed(123)
NN_Q<- neuralnet(d_SPY~ d_Ret + d_Fav + d_Freq, data=train_Q, hidden=c(3,3), linear.output=FALSE, threshold=0.01, likelihood = TRUE)
NN_Q$result.matrix
plot(NN_Q)


temp_test <- subset(test_Q, select = c("d_Ret", "d_Fav", "d_Freq"))
NN.results <- neuralnet::compute(NN_Q, temp_test)
results <- data.frame(actual = test_Q$d_SPY, prediction = NN.results$net.result)
head(results)

#Accuracy
SPY.Adjusted = d_NNData_Q$d_SPY
predicted=results$prediction * abs(diff(range(SPY.Adjusted))) + min(SPY.Adjusted)
actual=results$actual * abs(diff(range(SPY.Adjusted))) + min(SPY.Adjusted)
comparison=data.frame(predicted,actual)
deviation=((actual-predicted)/actual)
comparison=data.frame(predicted,actual,deviation)
accuracy=1-abs(mean(deviation))
accuracy

# Predict median value
pr.nn <- neuralnet::compute(NN_Q, test_Q[1:4])
pr.nn_ <- pr.nn$net.result*(max(d_NNData_Q$d_SPY)-min(d_NNData_Q$d_SPY))+min(d_NNData_Q$d_SPY)
test.r <- (test_Q$d_SPY)*(max(d_NNData_Q$d_SPY)-min(d_NNData_Q$d_SPY))+min(d_NNData_Q$d_SPY)
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_Q)

# Compare MSE's
print(paste(MSE.nn))

############################################################################################
library(rts)
library(plyr)
# Cross-validation NN
set.seed(450)
cv.error_Q <- NULL
k <- 10
pbar <- create_progress_bar('text')
pbar$init(k)
for(i in 1:k){
  train_ind <- sample(seq_len(nrow(maxmindf_Q)), size = smp_size)
  train.cv <- maxmindf_Q[train_ind,]
  test.cv <- maxmindf_Q[-train_ind,]
  nn <- neuralnet(d_SPY~ d_Ret + d_Fav + d_Freq, data=train.cv,hidden=c(3,3),linear.output=T)
  pr.nn <- compute(nn,test.cv)
  pr.nn <- pr.nn$net.result*(max(d_NNData_Q$d_SPY)-min(d_NNData_Q$d_SPY))+min(d_NNData_Q$d_SPY)
  test.cv.r <- (test.cv$d_SPY)*(max(d_NNData_Q$d_SPY)-min(d_NNData_Q$d_SPY))+min(d_NNData_Q$d_SPY)
  cv.error_Q[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)
  pbar$step()
}
mean(cv.error_Q)

#Plot the errors
boxplot(cv.error_Q,xlab='MSE CV',col='cyan',
        border='blue',names='CV error (MSE)',
        main='CV error (MSE) for NN Quant',horizontal=TRUE)

