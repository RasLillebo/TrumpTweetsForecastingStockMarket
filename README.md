# Forecasting The Stock Market Using Tweets (Using OLS, Neural Network & Random Forest)
### Motivation: A Brief Introduction to Sentiment Analysis.
(Codes to all plots and diagrams will be present in the file "TTFSMPLOTS.R")

Since the election of US president Trump, it has become increasingly evident how social media, and in particular twitter, has become a factor financial markets has to take into account.
This correlation has not only shifted the models on Wall Street to also incorporate social media but is also evidence of an increasingly integrated and convoluted environment navigate when forecasting. For example; Merrill Lynch started
modeling their investments after President Donald Trump’s twitter account activity, as seen below. And Bloomberg.com now has a tracker for mentions of the financial markets, also on Donald Trump's twitter account.
### Plot1: 

Such initiatives shifted more focus onto the existing development in text mining and sentiment analysis, that has seen exponential growth in recent years. Topics like these is where machine learning is said to outperform, and with prominent figures in the
financial sector already at it, it is reasonable to believe the research is well founded. 
Merrill Lynch found that the financial markets would on average be positive if the
current president posted less than five times a day, and negative for more than 35
tweets.
In theory, that is an exploitable correlation that could potentially be used as regressors
when modeling forecasting tools for the financial markets.


Bollen et. Al. (2010) proved correlation between public sentiment and the stock market. The study was conducted using twitter as data source, and a Self-Organizing
Fuzzy Neural Network (SOFNN). Which is basically a neural network with a range of logic, instead of the binary true or false. 
Such a model is more advanced than what I intend to showcase with this read, but the idea is the same.

### Data
If you want to use other tweets than Donald Trumps, here's a great installation guide for a Twitter API to help you in R: (Credits to Leah Wasser & Carson Farmer)

Link: https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/use-twitter-api-r/

I will be using President Donald Trump’s twitter history from 1-11-2016, the month he was inaugurated, up until 01-04-2020. This data will thereafter be split into text- and
activity-based data, and then tested separately. This is to further argue whether sentiment analysis or regular analysis of activity show proof of higher correlation. 

Twitter is a social media platform that has 330 million active users a month and 145 million daily users (2019).
63% of the users are between 35-65 years old (2018), and 40% of users has reportedly made a purchase after seeing ads on twitter.
This shows that people are willing to react based on what they see on twitter (Like buying or selling financial instruments)

#### The data in its raw form is stated below:
- Text (Character): Consists of the 140 characters or less Donald Trump chooses to share with his followers.
- Created_at (Datetime): Shows the exact time the ‘tweet’ was posted online on twitter.com by President Donald Trump or his social media team.
- Retweet_count (numeric): Shows the count of reposts the single ‘tweet’ has.
- Favorite_count (numeric): Shows the count of ‘Favorites’ the single ‘tweet’ has.

Furthermore, I use the ‘Created_at’ column to count the number of ‘tweets’ Donald Trump has on any specific day and call this ‘Frequency’. The ‘Text’ variable will go through multiple cleaning and sorting steps, resulting in 4 different sentiment scores (We shall see how later):
- MeanSentiment (numeric): The mean sentiment score of the day
- Positive: The positive sentiment score of the day
- Negative: The negative sentiment score of the day
- Neutral: The neutral sentiment score of the day.
I will use S&P500 adjusted prices in both datasets, which therefore limits the days in the processed data to open market days. 
## Data Preparation
First we need to install and load the packages. I like to use 'lapply', but you can just as easily use the simpler 'library()' function, for all the entries in the "Packages" vector defined below:
```
Packages <- c("dplyr", "readr", "zoo",  "SentimentAnalysis", "gridExtra", "tidytext", "tidyr", "ggplot2", "quantmod", "rts")
#install.packages(Packages)
lapply(Packages, library, character.only=TRUE)
```
#### Data: Loading and sorting into data.frames
Load the data into the directory and begin formatting the columns. I prefer to use the 'mutate' function, but you can also split the function into two lines instead.
We then aggregate the data into daily frequency, which will aid us in the later analysis:
```
#Data preparation:(Data: Text, created_at, retweet_count, favorite_count, is_retweet, id_str)
DT_Tweets <- read_delim("C:/Users/PC/OneDrive/Uni/Uni 8. semester/Economic Forecasting/TrumpData.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)
TTweets1 <- TTweets2 <- DT_Tweets
TTweets1 = TTweets1 %>% mutate(created_at <-  as.Date(TTweets1$created_at, format="%m-%d-%Y"),
                               retweet_count <- as.numeric(TTweets1$retweet_count))
TTweets1[, c(1:3, 5:6)] <- NULL   #Remove "Text", "is_retweet", "id_str". To focuse on quantitative data
TTweets1 <- na.omit(TTweets1)     #Remove NA's
names(TTweets1) <- c("favorite_count", "created_at", "retweet_count")

#Aggregate tweet information to daily frequency:
Aggregate      <- aggregate(. ~created_at, data=TTweets1, sum, na.rm=TRUE)
Freq_Tweet      <- table(TTweets1$created_at)
DTrump <- cbind(Aggregate, Freq_Tweet)
DTrump[, c(4)] <- NULL```
```
Furthermore, we need to gather the stock market data using the 'quantmod' package loaded above. 
To make thing simple, I choose the S&P500, but you can easily adjust the code parameter 'SPY' to any ticker you want.
```
getSymbols("SPY", from="2016-1-11", to="2020-04-1", by="days")
Indices <- as.data.frame(na.omit(cbind(SPY$SPY.Adjusted)))
Indices_names <- rownames(Indices)
Indices_Time <- as.Date(Indices_names)
Indices$created_at <- Indices_Time

#Create index returns
SPY_ret <- as.data.frame(diff(Indices$SPY.Adjusted)/Indices$SPY.Adjusted[-length(Indices$SPY.Adjusted)])
Index_Ret <- cbind(Indices$created_at[-1], SPY_ret$Returns)
names(Index_Ret) <- c("created_at", "SPY_ret")

#Create data frames
df_Indices_DT <- left_join(Indices, DTrump, by="created_at")
df_Indices_DT <- na.omit(df_Indices_DT)
```
#### Data: Sentiments 
With the data sorted into the two respectable data frames, we can now begin extracting the sentiment scores from our data. This may look daunting, but we will take it step-by-step:

Loading the dataframe for the sentiment:
```
#Data Preparation 2: Sentiment
# Get raw data again
tweet_data <- read_delim("C:/Users/PC/Github/Data/TrumpData.csv", 
                      ";", escape_double = FALSE, trim_ws = TRUE)
tweet_data[6] <- NULL #Remove the 'is_string' variable, as it is not needed.
tweet_data$created_at = as.Date(tweet_data$created_at, format="%m-%d-%Y")  #Format date column
```
When the data has been loaded we can start cleaning the text. This is done by searching for specific non-emotional or non-expressive words, used in everyday language, and remove these from the data. So that we are left with words that are primarily interpreted as negative or positive. Furhtermore, we add some stop words specific to our case, namely 'trump', 'realdonaldtrump' (his user name on Twitter) and URL-specific words, that have no effect here (https, amp).
```
#Clean text
DT_Tweets <- tweet_data  %>% mutate(tweet_text = gsub("http://*|https://*)", "",Text),
                                    month = as.yearmon(created_at))
data("stop_words")  #Load the stop word library

#Get a list of words
DT_Tweets_Clean <- DT_Tweets %>%
  dplyr::select(tweet_text, month) %>%
  unnest_tokens(word, tweet_text) %>%
  anti_join(stop_words) %>%
  filter(!word %in% c("rt", "t.co", "trump", "realdonaldtrump", "https", "amp"))
 ```
Join by words:
We rate the words using 'Bing's and QDAP's sentiment dictionaries for negative and positive interpretations. This step can take a while depending on the computational power of your hardware.
```
bing_sentiment <- DT_Tweets_Clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, month, sort = TRUE) %>%
  group_by(sentiment) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  group_by(month, sentiment) %>%
  top_n(n = 5, wt = n) %>%
# Create a date & sentiment column for sorting
  mutate(sent_date = paste0(month, " - ", sentiment)) %>%
  arrange(month, sentiment, n)
bing_sentiment$sent_date <- factor(bing_sentiment$sent_date,
                                   levels = unique(bing_sentiment$sent_date))

sentiment <- analyzeSentiment(enc2native(DT_Tweets$Text)) # Extract dictionary-based sentiment according to the QDAP dictionary
sentiment2 <- sentiment$SentimentQDAP
sentiment3 <- convertToDirection(sentiment$SentimentQDAP) # View sentiment direction (i.e. positive, neutral and negative)

bing_DT = DT_Tweets_Clean %>% 
          inner_join(get_sentiments("bing")) %>% 
          count(word, sentiment, sort=TRUE)  %>% ungroup()   
          
DT.Over.Time <- bing_DT %>% 
                count(sentiment) %>% 
                spread(sentiment, n, fill=0) %>% 
                mutate(polarity=positive-negative,
                percent_positive=positive/(positive+negative)*100)

date <- DT_Tweets$created_at # Extract and convert 'date' column
df <- cbind(DT_Tweets, sentiment2, sentiment3, date) # Create new dataframe with desired columns
df <- df[complete.cases(df), ] # Remove rows with NA

# Calculate the average of daily sentiment score
df2 <- df %>% 
  group_by(date) %>%
  summarize(meanSentiment = mean(sentiment2, na.rm=TRUE))

# Calculate frequency of sentiments
freq <- df %>% 
  group_by(date,sentiment3) %>% 
  summarise(Freq=n())

# Convert data from long to wide
freq2 <- freq %>% 
  spread(key = sentiment3, value = Freq)

# Create Data frame for sentiments
df_Sentiments <- cbind(df2, freq2)
df_Sentiments[c(3, 4:5)] <- NULL
names(df_Sentiments) <- c("Date", "MeanSentiment", "Negative", "Neutral", "Positive")
NNData_S <- as.data.frame(df_Sentiments)

# Collecting into one to ensure equal amount of data:
NNData_Q = df_Indices_DT
NNData_Q = na.exclude(NNData_Q)
names(NNData_Q) <- c("SPY.Adjusted", "Date", "Retweet_count", "Favorite_count", "Frequency")
library(dplyr)
Total_df = left_join(NNData_Q, NNData_S, by="Date")
Total_df <- na.omit(Total_df)
```
![TSPic2](https://user-images.githubusercontent.com/69420936/91894477-09176880-ec96-11ea-83ca-515f05a09b8e.png)

#### Data: Quantitatives

When working with time series data, we want to make sure it is stationary. We therefore difference the data and normalize it.
```
# Remove 'created_at' column. It will not be of use later on.
NNData_Q = Total_df[c(1, 3:5)]
# Difference the data
d_NNData_Q =  NNData_Q %>% mutate(d_SPY = NNData_Q$SPY.Adjusted-lag(NNData_Q$SPY.Adjusted),
                                  d_Ret = NNData_Q$Retweet_count-lag(NNData_Q$Retweet_count),
                                  d_Fav = NNData_Q$Favorite_count-lag(NNData_Q$Favorite_count),
                                  d_Freq = NNData_Q$Frequency-lag(NNData_Q$Frequency))

d_NNData_Q[,1:4] <- NULL #Remove the non-differenced data. It will not be of use later on.
d_NNData_Q = as.data.frame(na.exclude(d_NNData_Q)) #Exclude any na's (The first row of differenced columns will always be NA)

# Normalize the data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
maxmindf_Q <- as.data.frame(lapply(d_NNData_Q, normalize))
```
## Analysis
#### Neural Network: Quantitatives
For our analysis, we want to split our data into test and training sets. I use 75% of the data for training and 25% for testing.

```
smp_size <- floor(0.75 * nrow(NNN))
train_ind <- sample(seq_len(nrow(maxmindf_Q)), size = smp_size)
train_Q <- maxmindf_Q[train_ind, ]
test_Q <- maxmindf_Q[-train_ind, ]
```
We can then run our simple neural network using the 'neuralnet' package. There are some rules of thumb one should consider when choosing number of layers and thresholds, however, in this introductioni the reader should already be familiar with the models used.
```
set.seed(123)
NN_Q<- neuralnet(d_SPY~ d_Ret + d_Fav + d_Freq, data=train_Q, hidden=c(3,3), linear.output=FALSE, threshold=0.01, likelihood = TRUE)
NN_Q$result.matrix
#plot(NN_Q) #
```
To see whether the neural network performed well or not, we can estimate the performance by holding the predictions against our test set:
```
temp_test <- subset(test_Q, select = c("d_Ret", "d_Fav", "d_Freq"))
NN.results <- neuralnet::compute(NN_Q, temp_test)
results <- data.frame(actual = test_Q$d_SPY, prediction = NN.results$net.result)
# Accuracy
SPY.Adjusted = d_NNData_Q$d_SPY
predicted=results$prediction * abs(diff(range(SPY.Adjusted))) + min(SPY.Adjusted)
actual=results$actual * abs(diff(range(SPY.Adjusted))) + min(SPY.Adjusted)
comparison=data.frame(predicted,actual)
deviation=((actual-predicted)/actual)
comparison=data.frame(predicted,actual,deviation)
accuracy=1-abs(mean(deviation))
accuracy
```
The accuracy was only 0.0953. To put in into more statistical terms, I can also calculate the MSE (mean squared error)
```
# Predict median value
pr.nn <- neuralnet::compute(NN_Q, test_Q[1:4])
pr.nn_ <- pr.nn$net.result*(max(d_NNData_Q$d_SPY)-min(d_NNData_Q$d_SPY))+min(d_NNData_Q$d_SPY)
test.r <- (test_Q$d_SPY)*(max(d_NNData_Q$d_SPY)-min(d_NNData_Q$d_SPY))+min(d_NNData_Q$d_SPY)
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_Q)
# Compare MSE's
print(paste(MSE.nn))
```
The MSE is 8.897. However, machine learning, as especially neural networks are prone to overfit, therefore. I test the robustness of my result using cross validation:
```
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
  pr.nn <- neuralnet::compute(nn,test.cv)
  pr.nn <- pr.nn$net.result*(max(d_NNData_Q$d_SPY)-min(d_NNData_Q$d_SPY))+min(d_NNData_Q$d_SPY)
  test.cv.r <- (test.cv$d_SPY)*(max(d_NNData_Q$d_SPY)-min(d_NNData_Q$d_SPY))+min(d_NNData_Q$d_SPY)
  cv.error_Q[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)
  pbar$step()
}
mean(cv.error_Q)
```
As expected, the mean error increased a lot during the cross validation step. This indicate overfitting of the data.
We now repeat the analysis for the sentiment data:

### Neural Network: Sentiments
Just as we did before, we normalize and differenec the data, and divide the data into test and training sets.
```
smp_size <- floor(0.75 * nrow(d_NNData_S))
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
maxmindf_S <- as.data.frame(lapply(d_NNData_S, normalize))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(maxmindf_S)), size = smp_size)
train_S <- maxmindf_S[train_ind, ]
test_S <- maxmindf_S[-train_ind, ]

set.seed(500)
NN_S<- neuralnet(d_SPY~  d_NegS + d_PosS + d_NeuS, data=train_S, hidden=c(3,3), linear.output=FALSE, threshold=0.01, likelihood = TRUE)
NN_S$result.matrix
plot(NN_S)
```
Furthermore we also re-calculate the MSE, in order to compare the error of the models (The result matrix from the nn-function contains more performance measures you can use for comaparing the models. Personally, I'm more comfortable with the MSE)
```
temp_test <- subset(test_S, select = c( "d_NegS", "d_PosS", "d_NeuS"))
NN.results <- neuralnet::compute(NN_S, temp_test)
results <- data.frame(actual = test_S$d_SPY, prediction = NN.results$net.result)
head(results)

#Accuracy
SP.Adjusted = d_NNData_S$d_SPY
predicted=results$prediction * abs(diff(range(SP.Adjusted))) + min(SP.Adjusted)
actual=results$actual * abs(diff(range(SP.Adjusted))) + min(SP.Adjusted)
comparison=data.frame(predicted,actual)
deviation=((actual-predicted)/actual)
comparison=data.frame(predicted,actual,deviation)
accuracy=1-abs(mean(deviation))
accuracy

# Predict median value
pr.nn <- neuralnet::compute(NN_S,test_S)
pr.nn_ <- pr.nn$net.result*(max(d_NNData_S$d_SPY)-min(d_NNData_S$d_SPY))+min(d_NNData_S$d_SPY)
test.r <- (test_S$d_SPY)*(max(d_NNData_S$d_SPY)-min(d_NNData_S$d_SPY))+min(d_NNData_S$d_SPY)
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_S)

# Compare MSE's
print(paste(MSE.nn))
```
The MSE is 505.71. Because we are trying to compare the models on equal terms, I also do a cross-validation for the sentiment based model:
```
# Cross-validation NN
set.seed(450)
cv.error_S <- NULL
k <- 10
pbar <- create_progress_bar('text')
pbar$init(k)
for(i in 1:k){
  train_ind <- sample(seq_len(nrow(maxmindf_S)), size = smp_size)
  train.cv <- maxmindf_S[train_ind,]
  test.cv <- maxmindf_S[-train_ind,]
  nn <- neuralnet(d_SPY~ d_NegS + d_PosS + d_NeuS, data=train.cv,hidden=c(3,3),linear.output=T)
  pr.nn <- compute(nn,test.cv)
  pr.nn <- pr.nn$net.result*(max(d_NNData_S$d_SPY)-min(d_NNData_S$d_SPY))+min(d_NNData_S$d_SPY)
  test.cv.r <- (test.cv$d_SPY)*(max(d_NNData_S$d_SPY)-min(d_NNData_S$d_SPY))+min(d_NNData_S$d_SPY)
  cv.error_S[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)
  pbar$step()
}
```
## Forecasting:

Before we go into forecasting, the parameters of the models should be re-estimated if the following is present:
- Data should appear stationary (Constant mean and autocovariance over time)
- The neural network should not fit the data perfectly (Gradient descent should not be a global optiumum = Over fitting the data)
- Number of hidden layers in neural network should be minimizing the errors, and the number should be within the rule of thumb (See "The Element of statistical learning" for more info on neural networks)

If these criterias are all met, we can start forecasting the S&P500 using our data, and compare the performance using mean errors, AIC and BIC. I will also be comparing the neural network to a simple OLS estimation:
```
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
```
From the performance measures of the forecasting, we can compare and reflect on our models to determine the most optimal one. 
We can plot the four different models, using the model below:
```
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
```
This is one way of estimating and interpreting the models, however, to add another dimension to the analysis. I add another model to the analysis, and compare it to the introductory estimation step in the '**Analysis**' chapter.

## Random Forest
```
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
```
From this code, we can again plot the MSE of our mean squared errors in the following plot:
```
windows()
par(mfrow=c(2,1))
boxplot(RF.Sentiment$mse,xlab='MSE',col='cyan',
        border='blue',names='MSE',
        main='MSE for RF Sentiment',horizontal=TRUE)
boxplot(RF.Quant$mse,xlab='MSE',col='cyan',
        border='blue',names='MSE',
        main='MSE for RF Quant',horizontal=TRUE)
```
We can see that the random forest has smaller mean squared errors than the OLS and neural network, making it a better estimation than our forecasted models above. 

### Conclusion
This code is from a project I did when learning about sentiment analysis and neural networks at Aarhus University. The project had more depth and therefore much more code attached to it. I have not intended posting all code written during the project, which is also why the code my seem a bit messy or missing certain links between the chapters above. 

The project concluded a strong correlation between Trump's negative sentiment and the stock market price, however, this could also be due to the time trend: That despite differencing and (in the full prohect) log-transformation, we still somewhat visible. For the quantitative data, there was a correlation between retweets and favorites and the S&P500, but that was can also be explained by the time-component, as you gain followers over time, and therefore also gather more reteweets and favorites. So it is uncertain whether the variable in itself or the impact of time, was the actual correlative effect. 

The goal of this resporitory is to be the main neural network-project on my account, and I will therefore revisit it often. If you have any contributions, feel free to showcase them. :) 
## Final remarks

Thank you for reading so far. As stated above, this resporitory is a work in progress, and will therefore be updated continously.
I hope I made myself understandable grammatically and the code easy to interpret. 
I'm not a fan of copy-paste codes, as I do not beleive you will learn anything from that, however, I am ready to answer any questions regarding the code above.




### Sources:

- Javier E. David (2019), Trump tweets and market volatility have a 'statistically significant'
relationship, BofA finds, Yahoo Finance.
