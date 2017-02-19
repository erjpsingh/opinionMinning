# install the basic packages
install.packages("ROAuth")
install.packages("twitteR")

# load the respective packages
library("ROAuth")
library("twitteR")

# Fetching data from twitter
consumer_key <-'86HF6yYj3hGwpdGVFOmlnsUeX'
consumer_secret <-'PYjVFZitzBsrr1wfp1QRzidXVa6OOqEbAET4ri4HBfIu1HIS8Q'
access_token <-'1111878264-X8NAa8lknRcoVwUcyeiE9RwMAJg6qiJCJ87WOmY'
access_secret <-'kLd90mJmv2jGYvvRXowVnsz8Sl6p1G7xTZwSUzMLMtWxZ' 

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
tweets <- searchTwitter("@airtelindia", n=100, lang = "en")
# removing retweets
tweets_noReTweets<- strip_retweets(tweets, strip_manual = TRUE, strip_mt = TRUE)
#Exploring Tweets.
length(tweets_noReTweets)
class(tweets_noReTweets)

#Now fetching the text of these tweets.
tweets_text <- sapply(tweets_noReTweets, function(x)x$getText())
#We wil  have all of these as "chracter"
class(tweets_text)

#Removing unnecessary data

tweets_text <- gsub("@\\w+", "", tweets_text)       # Removing all twitter handle
tweets_text <- gsub("[[:digit:]]", "", tweets_text) # Removing all the digits
tweets_text <- gsub("[[:punct:]]", "", tweets_text) # removing All the punctuation
tweets_text <- gsub("http\\w+", "", tweets_text)    # removing http link
tweets_text <- sapply(tweets_text, tolower)         # to lower chrachters

# emotion icons pose a challenge in minning tweets , on eplausible solution is filtering tweets on basis of regular expression
# Here is a function for the same.
"rmNonAlphabet <- function(str) {
words <- unlist(strsplit(str, " "))
in.alphabet <- grep(words, pattern = "[a-z|0-9]", ignore.case = T)
nice.str <- paste(words[in.alphabet], collapse = " ")
nice.str
}"

# trying another approach to remove emoji

tweets_text <- iconv(tweets_text, 'UTF-8', 'ASCII')

"tweets_text <- sapply(tweets_text, rmNonAlphabet)"
tweets_text <- tweets_text[!is.na(tweets_text)] # removing any missing values if they exist
names(tweets_text) <- NULL

#have a look at refined Tweets
head(tweets_text)
#temp <- "admire admire accurate admire"

# Finally our Data is ready to perform Analysis

"let us convert the twitter tweets_texts into words so that they can be compared and judged"
# we need stringR package
install.packages("stringr")
library(stringr)

words.list <- str_split(tweets_text, " ")   # changed to temp

pos <- scan('positive-words.txt', what = 'character', comment.char = ';')
neg <- scan('negative-words.txt', what = 'character', comment.char = ';')

words <- unlist(words.list)

pos.matches <- match(words , pos)
neg.matches <- match(words, neg)

"pos.matches <- !is.na(pos.matches)
neg.matches <- !is.na(neg.matches)"

score <- sum(pos.matches) - sum(neg.matches)


# trying to make histogram of words.
tbl <- table(pos.matches)
hdr <- as.integer(names(tbl))
barplot(rev(tbl), names.arg = pos[hdr], horiz = TRUE)


##################################################################################################
"now that we have some logic lets simplify the things by making certain functions"


