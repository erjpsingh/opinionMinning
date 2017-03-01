opinion.mining <- function(keyword){
  load.libraries()
  read.files()
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
fetch.prepare.tweets <- function(keyword,num_tweets = 500){
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
generate.opinion <- function(){
  
  client <- getAlgorithmiaClient("simj7yggQAPt2GS/yKhxZlnbHXp1")
  algo <- client$algo("nlp/SentimentAnalysis/1.0.3")
  
  result <- NULL
  # this logic adds sentiments to a array   
  for(i in 1:length(tweets_text)){
    input <- list(document=tweets_text[i])
    result[i] <- algo$pipe(input)$result
    print(result)
  }
  
  # aplly counters
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
    print(result[[i]]$sentiment)
    ifelse(result[[i]]$sentiment >=-1 & result[[i]]$sentiment < -0.5 ,very_negative<<-1 + very_negative ,NA )
    ifelse(result[[i]]$sentiment >=-0.5 & result[[i]]$sentiment < 0 ,negative<<- 1 + negative ,NA )
    ifelse(result[[i]]$sentiment == 0  ,neutral<<-1 + neutral ,NA )
    ifelse(result[[i]]$sentiment > 0 & result[[i]]$sentiment <= 0.5 ,positive<<- 1 + positive ,NA )
    ifelse(result[[i]]$sentiment > 0.5 & result[[i]]$sentiment <= 1 ,very_positive<<- 1 + very_positive ,NA )
    
    
  }
  
 
  #bar.output <<-barplot(c(very_negative, negative, neutral, positive, very_positive), ylim = c(0,n), names.arg = c("Very Negative", "Negative", " Neutral" , "Positive", "Very Positive"), main = "Opinion Minning")
  
}

#function to read necessary files
read.files <- function(){
  pos <<- scan('positive-words.txt', what = 'character', comment.char = ';')
  neg <<- scan('negative-words.txt', what = 'character', comment.char = ';')
  print("Files Loaded")
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
   
   # Sidebar with a slider input f 
   sidebarLayout(
      sidebarPanel(
                    textInput(inputId = "keyword", label = "Enter Keyword", value = ""),
                    actionButton(inputId = "submit_btn", label = "Generate Opinion"),
                    actionButton(inputId = "wordcloud_btn", label = "Generate Word Cloud")
                   
                    
                   
                   
                   ),
      
      # Show a main output here
      mainPanel(
        
        plotOutput(outputId = "bar_plot"),
        textOutput(outputId = "outputId"),plotOutput(outputId = "wc_img")
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
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

