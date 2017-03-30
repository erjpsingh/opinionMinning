## Minning Twitter data with R
### Introduction
We all have been using Twitter for broadcasting text , that text may be a news item or promotional status of a product. We often like following significant people on twitter . So twitter is something which has become an inevitable part of our sophisticated life.But, you may not know it's a platform for Sentiment Analysis.
### About this Project
In this repository i have tried to find out a way in which Sentiment Analysis can be performed.I have tried to keep things as simple as possible for a beginer to understand, if some has basic knowledge in Programming then its really easy.You can have a look at [LIVE Shiny App](https://erjpsingh.shinyapps.io/opinion_analysis/)

### Twitter Search API
It's a part of Twitter REST API.This API searches against a sampling of recent tweets published in last 7 days.
Note : this API is focussed on relevance and not completeness ie some tweets and user may be missing from search results.
### Algorithmia 
This is an amazing  service where we can find various NLP algorithms and snetiment analysis is one of them.
	
#### Software Requirement's
 * Latest version of R [At the time of compilation it was R-3.3.2]
 * IDE R-Studio [Although optional but its always good to have it
 
#### Programming Language used
* R statistical language [_you might have already guessed it_]
#### Algorithmic Flow
I have tried to implement modular approach in my code ie i have created different functions for performing different task, below is a list of all functions and brief description.

* load.libraries() - load all the required libraries
* establish.connection() - connects with twitter API 
* fetch.prepare.tweets() - this function performs data cleansing for fetched tweets
* generate.wordcloud() - generates dynamic word cloud 
* generate.opinion() - connects with ALGORITHMIA API and fetches score for tweets and generate barplot
* ui - User Interface of Shiny App
* server -  background processing of Shiny App
* shinyApp - execute Shiny App

#### Acknowledgements
I am thankfull to Twitter Developers for providing data and also for Algorithmia who designed algorithms which enabled me to start this side project 


##### NOTE
The Sentiment Analysis algo provided by ALGORITHMIA is not open source 
