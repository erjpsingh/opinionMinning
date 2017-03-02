opinion.mining <- function(keyword){
  load.libraries()
  establish.connection()
  fetch.prepare.tweets(keyword)
  generate.opinion()
 }

#load necessary libraries
load.libraries <- function(){
  library("ROAuth")
  library("twitteR")
  library("stringr")
  library(wordcloud)
  library(tm)
  library(SnowballC) 
  library(algorithmia)
  print("Required Libraries Loaded")
}

#function to establish connection with Twitter

establish.connection <- function(){
  consumer_key <-'86HF6yYj3hGwpdGVFOmlnsUeX'
  consumer_secret <-'PYjVFZitzBsrr1wfp1QRzidXVa6OOqEbAET4ri4HBfIu1HIS8Q'
  access_token <-'1111878264-X8NAa8lknRcoVwUcyeiE9RwMAJg6qiJCJ87WOmY'
  access_secret <-'kLd90mJmv2jGYvvRXowVnsz8Sl6p1G7xTZwSUzMLMtWxZ' 
  
  setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
  print("Connection Established")
}

#function to fetch tweets and prepare them
fetch.prepare.tweets <- function(keyword,num_tweets = 50){
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
  tweets_text <<- tweets_text
  n <<- length(tweets_text)
  return(tweets_text)
  
  print("tweets fetched sucessfully")
}

#function to generate opinion
generate.opinion <- function(){
  
  # Setting up initialls for ALGORITHMIA
  client <- getAlgorithmiaClient("simj7yggQAPt2GS/yKhxZlnbHXp1")
  algo <- client$algo("nlp/SentimentAnalysis/1.0.3")
  
  # Initialising values
  result <<- NULL
  very_negative_tweets <<- NULL
  negative_tweets <<- NULL
  neutral_tweets <<- NULL
  positive_tweets <<- NULL
  very_positive_tweets <<- NULL
  
  # this logic adds sentiments to a array   
  for(i in 1:length(tweets_text)){
    input <- list(document=tweets_text[i])
    result[i] <<- algo$pipe(input)$result
    print(result)
  }
  
  # apply counters
  # -1 : -0.5 very negative
  # -0.5 : 0  negative
  # 0 : 0.5   positive
  # 0.5 : 1   very positive
  very_negative <<- 0
  negative <<-0
  neutral <<- 0
  positive <<-0
  very_positive <<-0
  
  for(i in 1:length(tweets_text)){
    #print(result[[i]]$sentiment)
    ifelse(result[[i]]$sentiment >=-1 & result[[i]]$sentiment < -0.5 ,very_negative<<-1 + very_negative ,NA )
    ifelse(result[[i]]$sentiment >=-0.5 & result[[i]]$sentiment < 0 ,negative<<- 1 + negative ,NA )
    ifelse(result[[i]]$sentiment == 0  ,neutral<<-1 + neutral ,NA )
    ifelse(result[[i]]$sentiment > 0 & result[[i]]$sentiment <= 0.5 ,positive<<- 1 + positive ,NA )
    ifelse(result[[i]]$sentiment > 0.5 & result[[i]]$sentiment <= 1 ,very_positive<<- 1 + very_positive ,NA )
    
    # Adding origional tweets itself
    ifelse(result[[i]]$sentiment >=-1 & result[[i]]$sentiment < -0.5 ,very_negative_tweets[i]<<-result[[i]]$document,"NA" )
    ifelse(result[[i]]$sentiment >=-0.5 & result[[i]]$sentiment < 0 ,negative_tweets[i]<<-result[[i]]$document,"NA" )
    ifelse(result[[i]]$sentiment == 0  , neutral_tweets[i]<<-result[[i]]$document, "NA")
    ifelse(result[[i]]$sentiment >0 & result[[i]]$sentiment <= 0.5 ,positive_tweets[i]<<-result[[i]]$document,"NA" )
    ifelse(result[[i]]$sentiment > 0.5 & result[[i]]$sentiment <= 1 ,very_positive_tweets[i]<<-result[[i]]$document, "NA")
  }
  
  # further processing the tweets.
  very_negative_tweets <<- very_negative_tweets[!is.na(very_negative_tweets)]
  negative_tweets <<- negative_tweets[!is.na(negative_tweets)]
  neutral_tweets <<- neutral_tweets[!is.na(neutral_tweets)]
  positive_tweets <<- positive_tweets[!is.na(positive_tweets)]
  very_positive_tweets <<- very_positive_tweets[!is.na(very_positive_tweets)]
  
  array.output <<- c(very_negative, negative, neutral, positive, very_positive)
  barplot(array.output, ylim = c(0,length(tweets_text)), names.arg = c("Very Negative", "Negative", " Neutral" , "Positive", "Very Positive"), main = "Opinion Minning")
  
}


#function to generate word cloud
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



library(shiny)

ui <- fluidPage(
   
   # Application title
   titlePanel("Opinion Minning with Tweets"),
   
   # Sidebar with a slider input  
   sidebarLayout(
      sidebarPanel(
                    helpText("Enter any trending keyword you are curious about eg: #Apple or @NarenderModi and see the magic"),
                    textInput(inputId = "keyword", label = "Enter Keyword", value = ""),
                    actionButton(inputId = "submit_btn", label = "Generate Opinion"),
                    actionButton(inputId = "wordcloud_btn", label = "Generate Word Cloud"),
                    selectInput(inputId = "option", label = "Select Category", choices = c("Choose one" = "","Very Negative" = "very_negative_tweets", "Negative" = "negative_tweets","Neutral" = "neutral_tweets", "Positive" = "positive_tweets", "Very Positive" = "very_positive_tweets"),
                                multiple = FALSE)
                   
                    
                   
                   
                   ),
      
      # Show a main output here
      mainPanel(
        
        plotOutput(outputId = "bar_plot"),
        textOutput(outputId = "outputId"),
        plotOutput(outputId = "wc_img"), 
        textOutput(outputId = "temp_output"),
        htmlOutput(outputId = "tweets_text")
      
        )
      
                
   )
)

# Define server logic 
server <- function(input, output) {
  
   data <- reactive({input$keyword})
   
   
   output$outputId <- renderText({data()})
   
   
   observeEvent(input$submit_btn, {
                                  
                                  
                                  opinion.mining(data())
       output$bar_plot <- renderPlot({barplot(c(very_negative, negative, neutral, positive, very_positive), ylim = c(0,length(tweets_text)), names.arg = c("Very Negative", "Negative", " Neutral" , "Positive", "Very Positive"), main = "Opinion Minning")})
                                })
   
   observeEvent(input$wordcloud_btn, {
                                  
    output$wc_img <-  renderPlot({
      generate.wordcloud()
    }
   )
     })
   observeEvent(input$option,{
     for(i in 1:length(very_negative_tweets)){
     output$tweets_text <- renderText({very_negative_tweets[i]})
     }
   })
   
  
   
   "output$tweets_text <- renderUI({
     print(input$option)
     
     
     for(i in 1:length(very_negative_tweets)){
       HTML(very_negative_tweets[i])
     }
   })"
}

# Run the application 
shinyApp(ui = ui, server = server)

