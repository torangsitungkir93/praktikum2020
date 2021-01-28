library(shiny)
library(shinydashboard)
library(rtweet)
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(textdata)
library(purrr)
library(SentimentAnalysis)
library(e1071) 
library(caret)
library(vroom)
library(here)

#### Shiny ####

ui <- fluidPage(
  headerPanel("Analisis Sentimen Lockdown Pada Rakyat Amerika dengan Twitter"),
  headerPanel("Menggunakan Algoritma Naive Bayes"),
  mainPanel(
    tabsetPanel(
      tabPanel("Data Twitter", DT::dataTableOutput('dataTwitter')),
      tabPanel("Data Cleaned", DT::dataTableOutput('dataCleaned')),
      tabPanel("Data Sentimen", DT::dataTableOutput('tbl')),
      tabPanel("Perbandingan Sentimen", plotOutput("sent1")),
      tabPanel("Kata yang paling sering muncul", plotOutput("sent2")),
      tabPanel("World Cloud", plotOutput("sent3"))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  cuitan<- vroom(here("raw-data","cuitan.csv"))
  cuitan<- data.frame(cuitan)
  # Output Data
  output$dataTwitter = DT::renderDataTable({
    DT::datatable(cuitan ,options = list(lengthChange = FALSE))
  })
  
  cleanedKata<- vroom(here("raw-data","cleanedData.csv"))
  cleanedKata2 <- cleanedKata
  cleanedKata<- data.frame(cleanedKata$word)
  # Output Data
  output$dataCleaned = DT::renderDataTable({
    DT::datatable(cleanedKata ,options = list(lengthChange = FALSE))
  })
  
  bingKata<- vroom(here("raw-data","bingKata.csv"))
  bingKata<- data.frame(bingKata)
  # Output Data
  output$tbl = DT::renderDataTable({
    DT::datatable(bingKata ,options = list(lengthChange = FALSE))
  })
  
  # plot distribution of emotions
  plotSentiments2 <- function(sentiment_dataframe, title) 
  {
    library(ggplot2)
      cleanedKata3 <- data.frame(cleanedKata2)
      cleanedKata3 %>%
      count(word, sort = TRUE) %>%
      top_n(15) %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(x=word, y = n)) +
      geom_col() +
      coord_flip() +
      theme_classic() +
      labs(x="Count",
           y="Unique word",
           tittle="Unique word counts")
  }
  #plotting tweets emotions
  output$sent2 <- renderPlot({
    plotSentiments2(cleanedKata3, "Kata yang paling populer")
  })
  
  # plot distribution of emotions
  plotSentiments1 <- function(sentiment_dataframe, title) 
  {
    library(ggplot2)
      bingKata %>% group_by(sentiment) %>% top_n(5) %>% ungroup() %>% 
      mutate(word = reorder(word, n)) %>% ggplot(aes(word, n, fill = sentiment))+ 
      geom_col(show.legend = FALSE) + facet_wrap(~sentiment, scales = "free_y") + 
      labs(title = "Tweets containing '#Lockdown'", y = "Contribution to sentiment", 
           x = NULL) + coord_flip() + theme_bw()
  }
  #plotting tweets emotions
  output$sent1 <- renderPlot({
    plotSentiments1(bingKata, "Sentiment Analysis American Lockdown")
  })
  
  
  
  # plot distribution of emotions
  plotSentiments3 <- function(sentiment_dataframe, title) 
  {
    library(tm)
    library(wordcloud)
    library(RColorBrewer)
    
    # Generate word-cloud
    wordcloud(bingKata$word, bingKata$n, random.order=FALSE, rot.per=0.25, colors=brewer.pal(8, "Dark2"))
  }
  #plotting tweets emotions
  output$sent3 <- renderPlot({
    plotSentiments3(bingKata$word, "World Cloud")
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)