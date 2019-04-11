#install and load all the require package
library(twitteR)
library(purrr)
library(dplyr)
library(plyr)
library(stringr)
#install.packages(c("ROAuth","RCurl"))

require('ROAuth')
require('RCurl')

#function which is applied to the twitter data to calculate score of each tweet 
#that it contain a some positive words or negative words
score.sentiment <- function(sentences, pos.words, neg.words, .progress = 'none') 
{
  require(plyr)
  require(stringr)
  scores<- laply(sentences, function(sentence, pos.words, neg.words) {
    sentence<- gsub('[[:punct:]]',"",sentence)
    sentence<- gsub('[[:cntrl:]]',"",sentence)
    sentence<- gsub('\\d+',"",sentence)
    sentence<- tolower(sentence)
    word.list<- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    pos.matches<- match(words,pos.words)
    neg.matches<- match(words,neg.words)
    pos.logical<- !is.na(pos.matches)
    neg.logical<- !is.na(neg.matches)
    score<-sum(pos.logical) - sum(neg.logical)
    return(score)
  },pos.words, neg.words, .progress = .progress)
  scores.df<- data.frame(score = scores,text = sentences)
  return(scores.df)
}

#load the files with all the positive and negative words
pos.words <- scan("E:/Shankar/college/6th sem/data science/project/positive-words.txt",what='character')
neg.words <- scan("E:/Shankar/college/6th sem/data science/project/negative-words.txt",what='character')

#Applying the sentiment function to the tweets data
bjp.score <- score.sentiment(tweet_df$text,pos.words,neg.words,.progress = 'text')
cong.score <- score.sentiment(tweet2_df$text,pos.words,neg.words,.progress = 'text')

#plotting the histogram graph to analyse which party is using
#more positive words in his tweets
hist(bjp.score$score)
hist(cong.score$score)

#All the required keys for the twitter api access
consumerKey <- 'xxxx'
consumerSecret <- 'xxxx'
accessToken <- 'xxxx'
accessTokenSecret <- 'xxxx'

#connecting and authrization to the twitter api
setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessTokenSecret)

#access the last 1000 tweets of the bjp official twitter account
tweet1 <- userTimeline('@BJP4India',n = 1000)

#access the last 1000 tweets of the congress official twitter account
tweet2 <- userTimeline('@INCIndia',n = 1000)

#changing both tweets data to the data frame
tweet_df <- tbl_df(map_df(tweet1,as.data.frame))
tweet2_df <- tbl_df(map_df(tweet2,as.data.frame))

