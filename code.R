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
Punjab_tweets <- searchTwitter("punjab", n=10, lang = "en") 
Punjab_tweets