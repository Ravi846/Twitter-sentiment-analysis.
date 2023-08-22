# Twitter Sentimental Analysis

#Loading required libraries
library(readr)
library(tidyr)
library(stringr)
library(dplyr)
library(tibble)
library(knitr)
library(factoextra)
library(fpc)
library(clValid)
library(cluster)
library(tidytext)
library(lubridate)
library(ggplot2)
library(DT)
library(plotly)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(nonlinearTseries)
library(tm)
library(tidyverse)
library(rsconnect)

#Reading twitter.csv data file
#rawData4Tweets <- read.csv("C:/Users/meria/Desktop/ARVRTRend/Dataset/tweets.csv", header = FALSE)

rawData4Tweets <- read.csv("tweets.csv", header = FALSE)
#Adding column names
colnames(rawData4Tweets) <- c("target","ids","date","flag","user","text")

kable(
  rawData4Tweets %>%
    select(date,text) %>%
    slice(0:5),
  caption = "Previewing few columns of Twitter user data set"
)

#Reading daily-website-visitors.csv data file
#page <- read.csv("C:/Users/meria/Desktop/ARVRTRend/Dataset/daily-website-visitors.csv", header = TRUE, sep = ',')
page <- read.csv("daily-website-visitors.csv", header = TRUE, sep = ',')
kable(
  page %>%
    select(Row,Day,Date,Page.Loads,Unique.Visits) %>%
    slice(0:5),
  caption = "Previewing few columns of Daily time series data set."
)
#Removing unnecessary words
clean_reg <- "&amp;|&lt;|&gt;"
neat_tweets <- rawData4Tweets %>% 
  filter(!str_detect(text, "^(RT|@)")) %>%
  mutate(text = str_remove_all(text, clean_reg)) %>%
  unnest_tokens(word, text, token = "tweets", strip_url = TRUE) %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"))

tweet_freq <- neat_tweets %>%
  count(word, sort = TRUE) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  group_by(word) %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))

# the lexicon
tweets_lexicon <- get_sentiments("nrc")

# now the job
neat_tweets <- neat_tweets %>%
  left_join(tweets_lexicon, by="word")

# remove NA's
neat_tweets <- neat_tweets %>%
  filter(sentiment!= "NA")



#Adding the month column to the dataset
neat_tweets <- neat_tweets %>%
  mutate(elements = str_split(date, fixed(" "), n=6)) %>% 
  mutate(Month = map_chr(elements, 2),
         Day = map_chr(elements, 1),
         date = map_chr(elements, 3),
         Time = map_chr(elements, 4), .keep="unused")
neat_tweets$date <- as.integer(neat_tweets$date)

#Calculating PMF and CDF on number of tweets
neat_tweets
tweets_freq <- neat_tweets %>%
  select(Month, Day, Time) %>%
  group_by(Month, Day, Time) %>%
  summarise(count = n()) %>%
  group_by(count) %>%
  summarise(num_days = n()) %>%
  mutate(pickup_pmf = num_days/sum(num_days)) %>%
  mutate(pickup_cdf = cumsum(pickup_pmf))

kable(
  tweets_freq %>%
    select(pickup_pmf) %>%
    slice(0:5),
  caption = "First 5 records of PMF of the tweet frequency."
)
kable(
  tweets_freq %>%
    select(pickup_cdf)  %>%
    slice(0:5),
  caption = "First 5 records of CDF of the tweet frequency"
)


#UI Page
ui <- dashboardPage(
  dashboardHeader(title = "Twitter Sentiment Analysis", titleWidth = 300),
  dashboardSidebar(width = 300,
                   sidebarMenu(
                     id = "menu",
                     menuItem("User Information", icon = icon('folder'), tabName = "user"),
                     menuItem("Unique words in tweeets", icon = icon('file'), tabName = "freq"),
                     menuItem("Sentiments vs Counts", icon = icon('cube'), tabName = "Count"),
                     menuItem("Number of tweets in a week", icon = icon('envelope-open-text'), tabName = "tweets_scores"),
                     menuItem("Sentiments on a week", icon = icon("code"),tabName = "sentiment"),
                     menuItem("Different sentiments vs days", icon = icon("clipboard"), tabName = "sentiments_days"),
                     menuItem("Different sentiments vs Date", icon = icon("calendar"), tabName = "date"),
                     menuItem("PMF of tweets vs Time", icon = icon('timeline'), tabName = "PMF")
                   )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "sentiments_days",
              fluidRow(
                box(
                  title = "Different sentiments over days",
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 10,
                  plotOutput("plot1", height = 300,width = 1000),
                  plotOutput("plot2", height = 300,width = 1000),
                  plotOutput("plot3", height = 300,width = 1000),
                  plotOutput("plot4", height = 300,width = 1000),
                  plotOutput("plot5", height = 300,width = 1000)
                )
              )),
      tabItem(tabName = 'user',
              fluidRow(
                box(title = "Twitter User Information", status = "primary", solidHeader = TRUE, width = 10,
                    DT::dataTableOutput("user_info"))
              )
      ),
      tabItem(tabName = 'tweets_scores',
              fluidRow(
                box(title = "Number of Tweets", status = "primary", solidHeader = TRUE, width = 10,
                    plotOutput("no_of_tweets"))
              )
      ),
      tabItem(tabName ="sentiment",
              fluidRow(
                box(
                  title = "Different sentiment on a Week",
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 10,
                  selectizeInput(
                    inputId = "sentiment",
                    label = "Select a Sentiment",
                    choices = unique(neat_tweets$sentiment)),
                  plotOutput("plot6", height = 300,width = 1000)
                )
              )
      ),
      tabItem(tabName = 'freq',
              fluidRow(
                box(title = "Top 15 Words", status = "primary", solidHeader = TRUE, width = 10,
                    plotOutput("top_freq"))
              )
      ),
      tabItem(tabName = 'PMF',
              fluidRow(
                box(title = "PMF of tweets vs Time", status = "primary", solidHeader = TRUE, width = 10,
                    plotOutput("tweets_time"))
              )
      ),
      tabItem(tabName = 'Count',
              fluidRow(
                box(title = "Different Sentiments vs Count", status = "primary", solidHeader = TRUE, width = 10,
                    plotOutput("sentiment_Count"))
              )
      ),
      tabItem(tabName = "date",
              fluidRow(
                box(
                  title = "Different sentiments vs Date",
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 10,
                  selectizeInput(
                    inputId = "date",
                    label = "Select atleast one date from the dropdown",
                    choices = unique(neat_tweets$Day),
                    multiple = TRUE
                  ),
                  plotOutput("plot7", height = 600,width = 1000)
                )
              )
      )
    )
  )
)


#Server
server <- function(input, output) {
  # User Info
  output$user_info <- DT::renderDataTable({
    rawData4Tweets
    
  })
  
  #Top frequently used words
  output$top_freq <- renderPlot({
    neat_tweets %>%
      count(word, sort = TRUE) %>%
      top_n(15) %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(x = word, y = n, fill= word)) +
      geom_col() +
      theme(legend.position="none")+
      theme(plot.title = element_text(hjust = 0.5)) +
      xlab(NULL) +
      coord_flip() +
      labs(y = "Count",
           x = "Unique words",
           title = "Frequently used unique words in tweets")
  })
  
  #Different sentiments vs counts
  output$sentiment_Count <-renderPlot({
    neat_tweets %>%
      count(sentiment) %>%
      ggplot(aes(x = sentiment, y = n)) +
      geom_bar(aes(fill=sentiment),stat = "identity")+
      theme(legend.position="none")+
      theme(plot.title = element_text(hjust = 0.5)) +
      xlab("Sentiments") +
      ylab("Count")+
      ggtitle("Different Sentiments vs Count")
    
  })
  
  #Number of tweets
  output$no_of_tweets <-renderPlot({
    neat_tweets %>%
      count(Day) %>%
      ggplot(aes(x = Day, y = n)) +
      geom_bar(aes(fill=Day),stat = "identity")+
      theme(legend.position="none")+
      xlab("Day") +
      ylab("Count")+
      ggtitle("Different Day vs Count")
    
  })
  
  #Sentiments on a week
  output$plot6 <- renderPlot({
    neat_tweets %>%
      filter(sentiment %in% input$sentiment) %>%
      count(Day) %>%
      ggplot(mapping=aes(x= Day, y = n), color = 'blue') +
      geom_bar(stat = "identity") +
      ggtitle(paste0('Sentiment :',input$sentiment))
  })
  
  #Different sentiments vs days
  output$plot1 <- renderPlot({
    pos <-
      neat_tweets %>%
      group_by(Day,sentiment) %>%
      filter(sentiment=='positive') %>%
      count(sentiment='positive')
    ggplot(data=pos,mapping=aes(x=Day, y=n, group=1)) + geom_line() + xlab('Day') + geom_point()+
      ggtitle("Positive Sentiment over the days")
  })
  output$plot2 <- renderPlot({
    neg <-
      neat_tweets %>%
      group_by(Day,sentiment) %>%
      filter(sentiment=='negative') %>%
      count(sentiment='negative')
    ggplot(data=neg,mapping=aes(x=Day, y=n, group=1)) + geom_line() + xlab('Day') + geom_point()+
      theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle("Negative Sentiment over the days")
  })
  output$plot3 <- renderPlot({
    ant <-
      neat_tweets %>%
      group_by(Day,sentiment) %>%
      filter(sentiment=='anticipation') %>%
      count(sentiment='anticipation')
    ggplot(data=ant,mapping=aes(x=Day, y=n, group=1)) + geom_line() + xlab('Day') + geom_point()+
      theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle("Anticipation Sentiment over the days")
  })
  output$plot4 <- renderPlot({
    joy <-
      neat_tweets %>%
      group_by(Day,sentiment) %>%
      filter(sentiment=='joy') %>%
      count(sentiment='joy')
    ggplot(data=joy,mapping=aes(x=Day, y=n, group=1)) + geom_line() + xlab('Day') + geom_point()+
      theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle("Joy Sentiment over the days")
  })
  output$plot5 <- renderPlot({
    trust <-
      neat_tweets %>%
      group_by(Day,sentiment) %>%
      filter(sentiment=='trust') %>%
      count(sentiment='trust')
    ggplot(data=trust,mapping=aes(x=Day, y=n, group=1)) + geom_line() + xlab('Day') + geom_point()+
      theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle("Trust Sentiment over the days")
  })
  
  #Different sentiments vs date
  output$plot7 <- renderPlot({
    neat_tweets %>%
      filter(Day %in% input$date) %>%
      group_by(Day) %>%
      count(sentiment) %>%
      ggplot(mapping=aes(x=Day, y=n, fill=sentiment), color = 'blue') +
      geom_bar(stat="identity") +
      ggtitle('Sentiment over different days') +
      facet_wrap(~sentiment)
  })
  
  #PMF
  output$tweets_time <-renderPlot({
    ggplot(tweets_freq, aes(count, pickup_pmf)) +
      geom_bar(stat="identity", fill="steelblue")+
      theme_bw() +
      labs( y = ' Probability') +
      theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle("PMF of tweets vs Time")+
      scale_x_continuous("Time", labels = as.character(tweets_freq$count),
                         breaks = tweets_freq$count*4)
    
  })
}


shinyApp(ui=ui, server=server)

