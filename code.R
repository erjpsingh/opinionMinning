# install the basic packages
install.packages("ROAuth")
install.packages("twitteR")
install.packages("stringr")


"Now lets implement a modular approach"

#load necessary libraries
load.libraries <- function(){
  library("ROAuth")
  library("twitteR")
  library("stringr")
  print("libraries loaded sucessfully")
}

#function to establish connection with Twitter
establish.connection <- function(){
  consumer_key <-'86HF6yYj3hGwpdGVFOmlnsUeX'
  consumer_secret <-'PYjVFZitzBsrr1wfp1QRzidXVa6OOqEbAET4ri4HBfIu1HIS8Q'
  access_token <-'1111878264-X8NAa8lknRcoVwUcyeiE9RwMAJg6qiJCJ87WOmY'
  access_secret <-'kLd90mJmv2jGYvvRXowVnsz8Sl6p1G7xTZwSUzMLMtWxZ' 
  
  setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
  print("connection established sucessfully")
}

#function to fetch tweets and prepare them
fetch.prepare.tweets <- function(keyword,num_tweets){
  tweets <- searchTwitter(keyword, num_tweets, lang = "en")
  #preparing tweets
  tweets_noReTweets<- strip_retweets(tweets, strip_manual = TRUE, strip_mt = TRUE) #eleminating re tweets
  tweets_text <<- sapply(tweets_noReTweets, function(x)x$getText())  # converting to text
  tweets_text <<- gsub("@\\w+", "", tweets_text)       # Removing all twitter handle
  tweets_text <<- gsub("[[:digit:]]", "", tweets_text) # Removing all the digits
  tweets_text <<- gsub("[[:punct:]]", "", tweets_text) # removing All the punctuation
  tweets_text <<- gsub("http\\w+", "", tweets_text)    # removing http link
  tweets_text <<- sapply(tweets_text, tolower)         # to lower chrachters
  tweets_text <<- iconv(tweets_text, 'UTF-8', 'ASCII') # converting to ASCII and hence removing emoji.
  tweets_text <<- tweets_text[!is.na(tweets_text)]     # removing any missing values if they exist
  names(tweets_text) <- NULL                          # removing the names of Array
  
  return(tweets_text)
  
  print("tweets fetched sucessfully")
}

#function to read necessary files
read.files <- function(){
  pos <<- scan('positive-words.txt', what = 'character', comment.char = ';')
  neg <<- scan('negative-words.txt', what = 'character', comment.char = ';')
  print("files read sucessfully")
}

#function analyse positive and negative opinions

  positive.negative.opiniion <- function(refined.tweets, pos, neg){
    words.list <<- str_split(refined.tweets, " ")
    w <<- unlist(words.list)
    pos.matches <<- match(w , pos)
    neg.matches <<- match(w, neg)
    
    total.pos <<- sum(table(pos.matches))
    total.neg <<- sum(table(neg.matches))
    barplot(c(total.pos, total.neg), ylim = c(0,500), names.arg = c("Positive" , "Negative"), main = "Opinion Minning")
    print("opinion generated sucessfully")
    return( (total.pos - total.neg)*100 / (total.pos + total.neg) )
    
    
  }
  
  
  
  
  ##################################################################################################
  # trying to make histogram of words.
  tbl <- table(pos.matches)
  hdr <- as.integer(names(tbl))
  barplot(rev(tbl), names.arg = pos[hdr], horiz = TRUE)
  
  #Simple barplot of positive and negative opinions
  barplot(c(total.pos, total.neg), ylim = c(0,500))
  


# Word Map
#http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know

#Installing packages
  
install.packages("wordcloud")
install.packages("tm")  
install.packages("SnowballC")
#loading necessary libraries

library(wordcloud)
library(tm)
library(SnowballC)  

# code

generate.wordcloud <- function(){
  
  docs <- Corpus(VectorSource(tweets_text))
  docs <- tm_map(x = docs, removeWords, c('the', 'this', stopwords('english')))
  tdm <- TermDocumentMatrix(docs)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  head(d, 10)
  
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            max.words=200, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
  
  
}




docs <- Corpus(VectorSource(tweets_text))
tdm <- TermDocumentMatrix(docs)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

temp <- wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
  
  
  
  
  
  
  
  
  
  
  
  