#Forecasting code: Analysis

set.seed(123)
#Linear models
  LM_Quant <- lm(d_NNData_Q$d_SPY~., data=maxmindf_Q)
  summary(LM_Quant)
  mean(LM_Quant$residuals^2)
  
  LM_Sentiment <- lm(d_NNData_S$d_SPY~., data=maxmindf_S)
  summary(LM_Sentiment)
  mean(LM_Sentiment$residuals^2)
#Prediction
  LM_Quant_pr <-  predict.lm(LM_Quant, test_Q)
  LM_Sentiment_pr <-  predict.lm(LM_Sentiment, test_S)
  NN_Quant_pr <- predict(NN_Q, test_Q)
  NN_Sentiment_pr <- predict(NN_S, test_S)

#Model comparison
library(stats)
  BIC(LM_Quant)
  AIC(LM_Quant)
  BIC(LM_Sentiment)
  AIC(LM_Sentiment)
  head(NN_S$result.matrix)
  head(NN_Q$result.matrix)

  #Forecast
  NN_Quant_pr = as.numeric(NN_Quant_pr)
  NN_Sentiment_pr = as.numeric(NN_Sentiment_pr)
  
library(forecast)
  set.seed(123)
  LMFOR_Quant <- forecast(LM_Quant_pr, h=5)
  LMFOR_Sentiment <- forecast(LM_Sentiment_pr, h=5)
  NNFor_Quant <- forecast(NN_Quant_pr, h=5)
  NNFor_Sentiment <- forecast(NN_Sentiment_pr,  h=5)

#Forecast Comparison
  summary(LMFOR_Quant)
  summary(LMFOR_Sentiment)
  summary(NNFor_Sentiment)
  summary(NNFor_Quant)

  
#Plot results
  windows()
  par(mfrow=c(2, 2))
  plot(LMFOR_Quant, col="red", asp=-+0.11:0.1, main="LM Quant")
  plot(LMFOR_Sentiment, col="blue", asp=-+0.11:0.1, main="LM Sentiment")  
  plot(NNFor_Quant, col="red", asp=-+0.11:0.1, main="NN Quant") 
  plot(NNFor_Sentiment, col="blue", asp=-+0.11:0.1, main="NN Sentiment")  

#Plot the differenced in a correlation matrix
  windows()
  par(mfrow=c(2,1))
  plot(LMFOR_Quant$residuals, type="l")
  lines(LMFOR_Sentiment$residuals, type="l", col="red")
  plot(NNFor_Sentiment$residuals, type="l", col="green")
  lines(NNFor_Quant$residuals, type="l", col="blue")
  
 