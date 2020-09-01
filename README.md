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
I will be using President Donald Trump’s twitter history from 1-11-2016, the month he was inaugurated, up until 01-04-2020. This data will thereafter be split into text- and
activity-based data, and then tested separately. This is to further argue whether sentiment analysis or regular analysis of activity show proof of higher correlation. 

### Sources:

- Javier E. David (2019), Trump tweets and market volatility have a 'statistically significant'
relationship, BofA finds, Yahoo Finance.
