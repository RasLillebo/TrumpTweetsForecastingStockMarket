#Packages:
library(dplyr)
library(readr)
library(quantmod)

#Data preparation:
#TTweets <- read_delim("C:/Users/ASUS/OneDrive/Uni/Uni 8. semester/Economic Forecasting/TTweets 13-03-2020.csv", 
#                      ";", escape_double = FALSE, trim_ws = TRUE)
DT_Tweets <- read_delim("C:/Users/PC/OneDrive/Uni/Uni 8. semester/Economic Forecasting/TrumpData.csv", 
                      ";", escape_double = FALSE, trim_ws = TRUE)
head(DT_Tweets)
TTweets <- DT_Tweets
TTweets[, c(1, 5:6)] <- NULL
TTweets$created_at <- as.Date(TTweets$created_at, format="%m-%d-%Y")
TTweets$retweet_count <- as.numeric(TTweets$retweet_count)
#Remove NA's
sum(is.na(TTweets))
TTweets <- na.omit(TTweets)
sum(is.na(TTweets))

#Aggregate tweet information to daily frequency:
Aggregate      <- aggregate(. ~created_at, data=TTweets, sum, na.rm=TRUE)
Freq_Tweet      <- table(TTweets$created_at)
DTrump <- cbind(Aggregate, Freq_Tweet)
DTrump[, c(4)] <- NULL


#Correlation test
cor(DTrump$retweet_count, DTrump$favorite_count)
cor(DTrump$Freq, DTrump$favorite_count)
cor(DTrump$Freq, DTrump$retweet_count)

#Introduce Indices
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

#Is there correlation to exploint in forecasting tweet activity on indices?
windows()
source("http://www.sthda.com/upload/rquery_cormat.r")
Cor1.1 <- df_Indices_DT[c(1, 3:5)]
rquery.cormat(Cor1.1)


#Quick model
lm_Trump1 <- lm(Cor1.1$SPY.Adjusted~. , data=Cor1.1)
windows()
par(mfrow=c(3,1))
plot(scale(lm_Trump1$residuals), type="l", xlab="Time (852 days)", ylab="Price (Scaled)", col="blue")
lines(scale(Cor1.1$SPY.Adjusted), type="l", col="red", lwd=2)
title("Trump Twitter Quantitatives Vs. S&P500")

#What if we use the same threshold as Meryll Lynch
ML_Significance_Upper    <-  df_Indices_DT[df_Indices_DT$Freq >= 5 , ]
Cor2.1 <- ML_Significance_Upper[, c(1, 3:5)]
lm_Trump2 <- lm(Cor2.1$SPY.Adjusted~. , data=Cor2.1)

ML_Significance_lower    <-  df_Indices_DT[df_Indices_DT$Freq <=5 , ]
Cor2.2 <- ML_Significance_lower[, c(1, 3:5)]
lm_Trump3 <- lm(Cor2.2$SPY.Adjusted~. , data=Cor2.2)

par(mfrow=c(3,1))
plot(scale(lm_Trump1$residuals), type="l", xlab="Time (852 days)", ylab="Price (Scaled)", col="blue")
lines(scale(Cor1.1$SPY.Adjusted), type="l", col="red", lwd=2)
title("Trump Twitter Quantitatives Vs. S&P500")
legend("topleft", legend=c("Red=S&P Adjusted", "Blue=Model"), col=c("red", "blue"), lty = 1, cex=0.9)
plot(scale(lm_Trump2$residuals), type="l", col="blue", xlab="Observation above or equal to 5")
lines(scale(Cor2.1$SPY.Adjusted), type="l", col="red", lwd=2)
title("Meryll Lynch Upper")
plot(scale(lm_Trump3$residuals), type="l", col="blue", xlab = "Observations below 5")
lines(scale(Cor2.2$SPY.Adjusted), type="l", col="red", lwd=2)
title("Meryll Lynch Lower")

#Full mode & Meryll Lynch inspired models
summary(lm_Trump1)
summary(lm_Trump2)
summary(lm_Trump3)

windows()
par(mfrow=c(2,1))
rquery.cormat(Cor2.1)
rquery.cormat(Cor2.2)
