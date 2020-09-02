# Forecasting The Stock Market Using Tweets (Using OLS, Neural Network & Random Forest)
### Motivation: A brief introduction to sentiment analysis.
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
Packages <- c("dplyr", "readr", "zoo",  "SentimentAnalysis", "gridExtra", "tidytext", "tidyr", "ggplot2", "quantmod")
#install.packages(Packages)
lapply(Packages, library, character.only=TRUE)
```
#### Loading and sorting into data.frames
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
df_Quant <- df_Indices_DT
```
#### Sentiment data 
With the data sorted into the two respectable data frames, we can now begin extracting the sentiment scores from our data. This may look daunting, but we will take it step-by-step:

Loading the dataframe for the sentiment:
```
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
We can now plot to see which words are most used for Donald Trump:
```
windows()
DT_Tweets_Clean %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in 4 year's worth of tweets")
```
![TSPic1](https://user-images.githubusercontent.com/69420936/91894474-074da500-ec96-11ea-92b2-9e19df0e22f8.png)

We can now rate the words ,using 'Bing's and QDAP's sentiment dictionaries, in negative and positive interpretations. This step can take a while depending of computational power of your hardware.
```
bing_sentiment <- DT_Tweets_Clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, month, sort = TRUE) %>%
  group_by(sentiment) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  group_by(month, sentiment) %>%
  top_n(n = 5, wt = n) %>%
  #Create a date & sentiment column for sorting
  mutate(sent_date = paste0(month, " - ", sentiment)) %>%
  arrange(month, sentiment, n)

bing_sentiment$sent_date <- factor(bing_sentiment$sent_date,
                                   levels = unique(bing_sentiment$sent_date))

sentiment <- analyzeSentiment(enc2native(DT_Tweets$Text))
#Extract dictionary-based sentiment according to the QDAP dictionary
sentiment2 <- sentiment$SentimentQDAP
#View sentiment direction (i.e. positive, neutral and negative)
sentiment3 <- convertToDirection(sentiment$SentimentQDAP)

bing_DT = DT_Tweets_Clean %>% inner_join(get_sentiments("bing")) %>% count(word, sentiment, sort=TRUE)  %>% ungroup()
windows()
bing_DT %>% group_by(sentiment) %>% 
  top_n(10) %>% ungroup() %>% 
  mutate(word=reorder(word, n)) %>% 
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend =FALSE) + facet_wrap(~sentiment, scales="free_y") +
  labs(title="Words by Twitter account @realDonaldTrump", 
       y = "Contribution to sentiment",
       x = NULL) + coord_flip() + theme_bw()
```
![TSPic2](https://user-images.githubusercontent.com/69420936/91894477-09176880-ec96-11ea-83ca-515f05a09b8e.png)

Using the ggplot-package, we can play around with plots and diagrams of our choice: (codes for the plots are listed in the file "Forecasting code part 1.2; Intro)

![TSPic3](https://user-images.githubusercontent.com/69420936/91894482-0a489580-ec96-11ea-97b9-3aca52b89bcd.png)

#### Quantitative Data

When working with time series data, we want to make sure it is stationary. We therefore difference the data and normalize it using the simple function:
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
We then want to split our data into test and training sets. I use 75% of the data for training and 25% for testing.

```
smp_size <- floor(0.75 * nrow(NNN))
train_ind <- sample(seq_len(nrow(maxmindf_Q)), size = smp_size)
train_Q <- maxmindf_Q[train_ind, ]
test_Q <- maxmindf_Q[-train_ind, ]
```


### Sources:

- Javier E. David (2019), Trump tweets and market volatility have a 'statistically significant'
relationship, BofA finds, Yahoo Finance.
