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
## Analysis
First we need to install and load the packages. I like to use 'lapply', but you can just as easily use the simpler 'library()' function, for all the entries in the "Packages" vector defined below:
```
Packages <- c("dplyr", "readr", "zoo",  "SentimentAnalysis", "gridExtra", "tidytext", "tidyr", "ggplot2", "quantmod")
lapply(Packages, library, character.only=TRUE)
```
#### Preparing the data
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

### Sources:

- Javier E. David (2019), Trump tweets and market volatility have a 'statistically significant'
relationship, BofA finds, Yahoo Finance.
