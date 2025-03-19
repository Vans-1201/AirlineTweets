### server.R
# Define server logic
server <- function(input, output) {
  source("dependencies.R") # Ensure dependencies are loaded
  
  tweet_data <- reactive({
    req(input$tweet_file)
    read.csv(input$tweet_file$datapath, stringsAsFactors = FALSE)
  })
  
  filtered_tweets <- reactive({
    data <- tweet_data()
    if (input$airline_choice != "All") {
      data <- data[grepl(input$airline_choice, data$text, ignore.case = TRUE), ]
    }
    data
  })
  
  output$tweet_table_raw <- renderDataTable({
    filtered_tweets()
  })
  
  # Sentiment Analysis Logic
  detailed_sentiment <- reactive({
    req(input$analyze_button)
    tweets <- filtered_tweets()
    afinn_lexicon <- get_sentiments("afinn")
    tweet_sentiment <- tweets %>%
      unnest_tokens(word, text) %>%
      inner_join(afinn_lexicon, by = "word") %>%
      group_by(text) %>%
      summarise(sentiment_score = sum(value, na.rm = TRUE)) %>%
      mutate(sentiment_classification = case_when(
        sentiment_score >= 2 ~ "Positive",
        sentiment_score <= -2 ~ "Negative",
        TRUE ~ "Neutral"
      ))
    tweet_sentiment
  })
  
  output$detailed_sentiment_table <- renderDataTable({
    req(input$analyze_button)
    detailed_sentiment()
  })
  
  output$sentiment_bar_chart <- renderPlot({
    req(input$analyze_button)
    sentiment_data <- detailed_sentiment()
    ggplot(sentiment_data, aes(x = sentiment_classification, fill = sentiment_classification)) +
      geom_bar() +
      labs(title = "Sentiment Distribution", x = "Sentiment", y = "Count") +
      theme_minimal()
  })
  
  output$positive_wordcloud <- renderPlot({
    req(input$analyze_button)
    wordcloud(words = c("happy", "good", "great", "love"), freq = c(10, 8, 6, 5), colors = brewer.pal(8, "Greens"))
  })
  
  output$negative_wordcloud <- renderPlot({
    req(input$analyze_button)
    wordcloud(words = c("delay", "bad", "worst", "angry"), freq = c(12, 9, 7, 6), colors = brewer.pal(8, "Reds"))
  })
}

shinyApp(ui = ui, server = server)
