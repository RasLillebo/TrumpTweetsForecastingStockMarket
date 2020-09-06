#Plotsfrom analysis

#Plot 1: Count of unique words and frequency
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

#Plot 2: Count of negative and positive words
windows()
bing_DT %>% group_by(sentiment) %>% 
  top_n(10) %>% ungroup() %>% 
  mutate(word=reorder(word, n)) %>% 
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend =FALSE) + facet_wrap(~sentiment, scales="free_y") +
  labs(title="Words by Twitter account @realDonaldTrump", 
       y = "Contribution to sentiment",
       x = NULL) + coord_flip() + theme_bw()

#Plot 3: Time series over word sentiment
windows()
ggplot() + 
  geom_bar(mapping = aes(x = freq$date, y = freq$Freq, fill = freq$sentiment3), stat = "identity") +
  ylab('Sentiment Frequency') +
  xlab('Date')

#Plot 4: Mean sentiment scores
p1 <- ggplot(data=df2, aes(x=date,y=meanSentiment, group=1)) +
  geom_line()+
  geom_point() +
  ylab("Mean Twitter Sentiment Score")

#Plot 5: Z-score of adjusted returns
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
Appendix2 <- Total_df[, c(1, 6:9)]
sum(is.na(Appendix2))
windows()
source("http://www.sthda.com/upload/rquery_cormat.r")
rquery.cormat(Appendix2)
