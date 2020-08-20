#Data Preparation: Sentiment
#Library
library(readr)
library(zoo)
library(SentimentAnalysis)
library(gridExtra)
library(tidytext)
library(tidyr)
library(ggplot2)
library(readr)
#Get data:
TTweets <- read_delim("C:/Users/PC/OneDrive/Uni/Uni 8. semester/Economic Forecasting/TrumpData.csv", 
                      ";", escape_double = FALSE, trim_ws = TRUE)
TTweets[6] <- NULL
tweet_data = TTweets
tweet_data$created_at = as.Date(tweet_data$created_at, format="%m-%d-%Y")
tail(tweet_data$created_at)

#Clean text
DT_Tweets <- tweet_data  %>% mutate(tweet_text = gsub("http://*|https://*)", "",Text),
                                    month = as.yearmon(created_at))
tail(DT_Tweets)

data("stop_words")

#Get a list of words
DT_Tweets_Clean <- DT_Tweets %>%
  dplyr::select(tweet_text, month) %>%
  unnest_tokens(word, tweet_text) %>%
  anti_join(stop_words) %>%
  filter(!word %in% c("rt", "t.co", "trump", "realdonaldtrump", "https", "amp"))

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

DT.Over.Time <- bing_DT %>% count(sentiment) %>% spread(sentiment, n, fill=0) %>% mutate(polarity=positive-negative, 
                                                                                         percent_positive=positive/(positive+negative)*100)
DT.Over.Time

#Extract and convert 'date' column
date <- DT_Tweets$created_at

#Create new dataframe with desired columns
df <- cbind(DT_Tweets, sentiment2, sentiment3, date)
#Remove rows with NA
df <- df[complete.cases(df), ]

#Calculate the average of daily sentiment score
df2 <- df %>% 
  group_by(date) %>%
  summarize(meanSentiment = mean(sentiment2, na.rm=TRUE))

freq <- df %>% 
  group_by(date,sentiment3) %>% 
  summarise(Freq=n())

#Convert data from long to wide
freq2 <- freq %>% 
  spread(key = sentiment3, value = Freq)

windows()
ggplot() + 
  geom_bar(mapping = aes(x = freq$date, y = freq$Freq, fill = freq$sentiment3), stat = "identity") +
  ylab('Sentiment Frequency') +
  xlab('Date')

#Calculate z-Scores of Amazon closing stock prices
Adjusted_close <- as.data.frame(Indices)

mu <- mean(Adjusted_close$SPY.Adjusted)
sd <- sd(Adjusted_close$SPY.Adjusted)
SPY2 <- Adjusted_close %>% 
  mutate(zScore = (SPY.Adjusted-mu)/sd)

#Plot mean sentiment scores
p1 <- ggplot(data=df2, aes(x=date,y=meanSentiment, group=1)) +
  geom_line()+
  geom_point() +
  ylab("Mean Twitter Sentiment Score")

#plot Amazon Nasdaq z-score prices
p2 <- ggplot(data=SPY2, aes(x=created_at,y=zScore, group=1)) +
  geom_line()+
  geom_point() +
  ylab("Z-Score of closing stock price")+
  xlab('Date')

scale_x_date(date_breaks = "1 day", 
             limits = as.Date(c('2019-05-03','2019-05-12')))
plot1 <- p1
plot2 <- p2
windows()
grid.arrange(plot1, plot2)
windows()
grid.arrange(plot2, plot3)

df_Sentiments <- cbind(df2, freq2)
df_Sentiments[c(3, 4:5)] <- NULL
names(df_Sentiments) <- c("Date", "MeanSentiment", "Negative", "Neutral", "Positive")
NNData_S <- as.data.frame(df_Sentiments)

#Collecting into one to ensure equal amount of data:
NNData_Q = df_Indices_DT
NNData_Q = na.exclude(NNData_Q)
names(NNData_Q) <- c("SPY.Adjusted", "Date", "Retweet_count", "Favorite_count", "Frequency")
library(dplyr)
Total_df = left_join(NNData_Q, NNData_S, by="Date")
Total_df <- na.omit(Total_df)

Appendix2 <- Total_df[, c(1, 6:9)]
sum(is.na(Appendix2))
windows()
source("http://www.sthda.com/upload/rquery_cormat.r")
rquery.cormat(Appendix2)

