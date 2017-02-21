opinion.mining <- function(keyword){
  load.libraries()
  read.files()
  establish.connection()
  fetch.prepare.tweets(keyword)
 }

#load necessary libraries
load.libraries <- function(){
  library("ROAuth")
  library("twitteR")
  library("stringr")
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
fetch.prepare.tweets <- function(keyword,num_tweets = 1500){
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
  
  words.list <<- str_split(tweets_text, " ")
  w <<- unlist(words.list)
  pos.matches <<- match(w , pos)
  neg.matches <<- match(w, neg)
  
  total.pos <<- sum(table(pos.matches))
  total.neg <<- sum(table(neg.matches))
  barplot(c(total.pos, total.neg), ylim = c(0,500), names.arg = c("Positive" , "Negative"), main = "Opinion Minning")
  print("Opinion Analysis performed")
  
  return( (total.pos - total.neg)*100 / (total.pos + total.neg) )
  
 
  
}

#function to read necessary files
read.files <- function(){
  pos <<- scan('positive-words.txt', what = 'character', comment.char = ';')
  neg <<- scan('negative-words.txt', what = 'character', comment.char = ';')
  print("Files Loaded")
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
                    plotOutput(outputId = "bar_plot")
                   
                   
                   ),
      
      # Show a main output here
      mainPanel(textOutput(outputId = "outputId"))
                
   )
)

# Define server logic 
server <- function(input, output) {
  
   data <- reactive({input$keyword})
   
   output$outputId <- renderText({data()})
   
   observeEvent(input$submit_btn, {
                                  
                                  
                                  opinion.mining(data())
     output$bar_plot <- renderPlot({barplot(c(total.pos, total.neg), ylim = c(0,500), names.arg = c("Positive" , "Negative"), main = "Opinion Minning")})
                                })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

