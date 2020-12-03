#Yusuke Shiota
# Yep

#read the CSV file
yelpData <- read.csv("yelp_reviews.csv")
head(yelpData)


# Q2

#Get summary, box plot, standard deviation for stars
summary(yelpData$stars)
boxplot(yelpData$stars)
sd(yelpData$stars)

#Get summary, box plot, standard deviation for review_length
summary(yelpData$review_length)
boxplot(yelpData$review_length)
sd(yelpData$review_length)

#Get summary, box plot, standard deviation for pos_words
summary(yelpData$pos_words)
boxplot(yelpData$pos_words)
sd(yelpData$pos_words)

#Get summary, box plot, standard deviation for neg_words
summary(yelpData$neg_words)
boxplot(yelpData$neg_words)
sd(yelpData$neg_words)

#Get summary, box plot, standard deviation for net_sentiment 
summary(yelpData$net_sentiment)
boxplot(yelpData$net_sentiment)
sd(yelpData$net_sentiment)



# Q3

#Extract pos_words from yelpData
positivecounts <- select(yelpData, pos_words)

#Get the count by number of positive words
res1 <- positivecounts %>% group_by(pos_words) %>% summarise(count = n())

#Sort the count 
res1<-arrange(res1,desc(count))

#Get first 20 entries
res1 <- head(res1,20)

res1
#Visualize res1
ggplot(res1, aes(x=pos_words, y=count)) + geom_col(color='#56B4E9')+ xlab("number of positive words") + ylab("count of messages with this number of positive words")


#Extract neg_words from yelpData
negativecounts <- select(yelpData, neg_words)

#Get the count by number of negative words
res2 <-negativecounts %>% group_by(neg_words) %>% summarise(count = n())

#Get first 20 entries
res2 = head(res2,20)
res2
#Visualize res2
ggplot(res2, aes(x=neg_words, y=count)) + geom_col(color='#E69F00') + xlab("number of negative words") + ylab("count of messages with this number of negative words")

####visualize grouped bar between res1 and res2####
res1$pos_or_neg <- c("positive", "positive", "positive","positive","positive",
                     "positive","positive","positive","positive","positive",
                     "positive","positive","positive","positive","positive",
                     "positive","positive","positive","positive","positive")

res1

res2$negative <-  c("negative", "negative", "negative","negative","negative",
                    "negative","negative","negative","negative","negative",
                    "negative","negative","negative","negative","negative",
                    "negative","negative","negative","negative","negative")

res2
colnames(res2) <- c("pos_words","count" ,"pos_or_neg")
res2

#Merge res1 and res2
res4 <- rbind(res1, res2)
res4

#Visualize res4 as a grouped bar
ggplot(res4, aes( x=pos_words,y=count, fill=pos_or_neg)) + geom_bar(position="dodge", stat="identity")+ xlab("number of positive and negative words") 



# Q4

#Extract net_sentiment from yelpData
netsenti <- select(yelpData, net_sentiment)
head(netsenti)

#Get the count by net_sentiments
res3 <-netsenti %>% group_by(net_sentiment) %>% summarise(count = n())

#Sort the count
res3<-arrange(res3,desc(count))

#Get first 20 entries
res3 <- head(res3,20)
res3
#Visualize res3
ggplot(res3, aes(x=net_sentiment, y=count)) + geom_col(color='yellow')


# Q5

#Extract stars, review_length from yelpData
starlength <- select(yelpData, stars, review_length )
head(starlength)

#Get summary, box plot, standard deviation for review_length 
#in order to determine Mean or Median
summary(starlength$review_length)
boxplot(starlength$review_length)
sd(starlength$review_length)

#Get the median of the length by stars
lengthstars <- aggregate(starlength$review_length, by=list(Stars=starlength$stars), FUN=median,na.rm=TRUE)
lengthstars

require(dplyr)

#Get the sum of length by stars
cou <- starlength %>% count(stars)
cou
#Store n to another column
lengthstars$countofreviews <- cou$n

#Store x to another column
lengthstars$medianLength <- lengthstars$x

#Delete the column x
lengthstars <- subset( lengthstars, select = -x )
lengthstars

#Select lengthstars, Stars, medianLength
lengthstars <- select(lengthstars, Stars, medianLength )
lengthstars

#Convert to data matrix
lengthstars = data.matrix(lengthstars)
lengthstars = t(lengthstars)
lengthstars

#Visualize lengthstars
barplot(lengthstars, main="MedianLength",
        xlab="Stars", col=c("darkblue"),ylim=range(pretty(c(0, 120))))


# Q6

#Extract stars, votes_useful from yelpData
usefulStars <- select(yelpData, stars, votes_useful)
head(usefulStars)


#Visualize usefulStars
ggplot(usefulStars, aes(x=stars, y=votes_useful)) + 
  geom_point()

#Get the mean of useful votes by stars
usefulStars <- aggregate(usefulStars$votes_useful, by=list(Stars=usefulStars$stars), FUN=mean,na.rm=TRUE)
usefulStars

#Extract length of reviews, votes_useful from yelpData
usefulLengh <- select(yelpData, review_length, votes_useful)
head(usefulLengh)

#Visualize usefulLengh
ggplot(usefulLengh, aes(x=review_length, y=votes_useful)) + 
  geom_point()

#Get the mean of useful votes by review_length
usefulLengh <- aggregate(usefulLengh$votes_useful, by=list(review_length=usefulLengh$review_length), FUN=mean,na.rm=TRUE)
usefulLengh

#Store x to another column
usefulLengh$meanuseful <- usefulLengh$x

#Delete the column x
usefulLengh <- subset( usefulLengh, select = -x )
usefulLengh

#Sort the meanuseful
usefulLengh<-arrange(usefulLengh,desc(meanuseful))

#Get first 20 entries
usefulLengh <- head(usefulLengh,5)
usefulLengh
