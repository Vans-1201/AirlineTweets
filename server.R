library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(tidytext)
library(stringr)
library(tidyr)
library(DT)
library(RColorBrewer)
library(sentimentr)  # For refined sentiment analysis

# Define UI for the application
ui <- dashboardPage(
  dashboardHeader(title = "Review Comment Sentiment Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Input", tabName = "data_input", icon = icon("upload")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-bar")),
      menuItem("Summary", tabName = "summary", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "data_input",
              fluidRow(
                box(
                  title = "Upload Review Comments Data",
                  width = 12,
                  fileInput("review_file", "Choose CSV File",
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")),
                  selectInput("airline_choice", "Select Airline",
                              choices = c("All", "United", "American", "Delta", "Southwest", "JetBlue", "Spirit", "Alaska")),
                  actionButton("analyze_button", "Run Analysis")
                )
              ),
              fluidRow(
                box(
                  title = "Uploaded & Filtered Review Data",
                  width = 12,
                  dataTableOutput("review_table_raw")
                )
              )
      ),
      tabItem(tabName = "analysis",
              fluidRow(
                box(
                  title = "Overall Sentiment Distribution",
                  width = 6,
                  plotOutput("sentiment_bar_chart")
                ),
                box(
                  title = "Word Cloud of Positive Words",
                  width = 6,
                  plotOutput("positive_wordcloud")
                )
              ),
              fluidRow(
                box(
                  title = "Word Cloud of Negative Words",
                  width = 6,
                  plotOutput("negative_wordcloud")
                ),
                box(
                  title = "Detailed Review Sentiment",
                  width = 12,
                  dataTableOutput("detailed_sentiment_table")
                )
              )
      ),
      tabItem(tabName = "summary",
              fluidRow(
                box(
                  title = "Overall Sentiment Summary",
                  width = 12,
                  htmlOutput("overall_sentiment_summary")
                )
              ),
              fluidRow(
                box(
                  title = "Sentiment Confidence Score Distribution",
                  width = 12,
                  plotOutput("sentiment_confidence_histogram")
                )
              ),
              fluidRow(
                box(
                  title = "Top 10 Positive Words",
                  width = 6,
                  verbatimTextOutput("top_positive_words")
                ),
                box(
                  title = "Top 10 Negative Words",
                  width = 6,
                  verbatimTextOutput("top_negative_words")
                )
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {

  # Reactive function to read the uploaded CSV file
  review_data <- reactive({
    req(input$review_file)
    tryCatch(
      read.csv(input$review_file$datapath, stringsAsFactors = FALSE),
      error = function(e) { stop(safeError(e)) }
    )
  })

  # Filter reviews based on the selected airline input
  filtered_reviews <- reactive({
    data <- review_data()
    if (input$airline_choice != "All") {
      data <- data[grepl(input$airline_choice, data$text, ignore.case = TRUE), ]
    }
    data
  })

  # Output the uploaded & filtered review data table
  output$review_table_raw <- renderDataTable({
    filtered_reviews()
  })

  # Perform detailed sentiment analysis using sentimentr (runs after button click)
  detailed_sentiment <- reactive({
    req(input$analyze_button)
    reviews <- filtered_reviews()
    if (nrow(reviews) > 0) {
      # Assume the CSV has a column named "text" for review comments.
      cleaned_reviews <- reviews %>%
        mutate(review_id = row_number(),
               original_text = text,
               text = str_replace_all(text, "can't", "cannot"),
               text = str_replace_all(text, "won't", "will not"))
      
      # Use sentimentr to calculate sentiment. sentiment() returns a score per sentence.
      sentiment_results <- sentiment(cleaned_reviews$original_text)
      
      # Aggregate sentiment by review (assuming each review is a single text element)
      sentiment_summary <- sentiment_results %>%
        group_by(element_id) %>%
        summarise(sentiment_score = mean(sentiment))
      
      # Merge the sentiment scores with the original data.
      merged_df <- merge(cleaned_reviews, sentiment_summary, by.x = "review_id", by.y = "element_id")
      
      # Classify sentiment based on the adjusted thresholds.
      merged_df <- merged_df %>%
        mutate(sentiment_classification = case_when(
          sentiment_score >= 0.05 ~ "Positive",
          sentiment_score <= -0.05 ~ "Negative",
          TRUE ~ "Neutral"
        ),
        confidence_score = abs(sentiment_score))
      
      merged_df
    } else {
      NULL
    }
  })

  # Prepare data for word clouds and top words (this still uses lexicon-based tokenization)
  sentiment_words <- reactive({
    req(input$analyze_button)
    reviews <- filtered_reviews()
    if (nrow(reviews) > 0) {
      manual_stopwords_wc <- c(
        "i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours",
        "yourself", "yourselves", "he", "him", "his", "himself", "she", "her", "hers",
        "herself", "it", "its", "itself", "they", "them", "their", "theirs", "themselves",
        "what", "which", "who", "whom", "this", "that", "these", "those", "am", "is", "are",
        "was", "were", "be", "been", "being", "have", "has", "had", "having", "do", "does",
        "did", "doing", "a", "an", "the", "and", "but", "if", "or", "because", "as", "until",
        "while", "of", "at", "by", "for", "with", "about", "against", "between", "into",
        "through", "during", "before", "after", "above", "below", "to", "from", "up", "down",
        "in", "out", "on", "off", "over", "under", "again", "further", "then", "once", "here",
        "there", "when", "where", "why", "how", "all", "any", "both", "each", "few", "more",
        "most", "other", "some", "such", "no", "nor", "not", "only", "own", "same", "so",
        "than", "too", "very", "s", "t", "can", "will", "just", "don", "should", "now"
      )
      afinn_lexicon <- get_sentiments("afinn")
      reviews %>%
        mutate(text = str_replace_all(text, "can't", "cannot"),
               text = str_replace_all(text, "won't", "will not")) %>%
        unnest_tokens(word, text) %>%
        filter(!(word %in% manual_stopwords_wc)) %>%
        inner_join(afinn_lexicon, by = "word")
    } else {
      NULL
    }
  })

  # Output detailed sentiment analysis table
  output$detailed_sentiment_table <- renderDataTable({
    req(input$analyze_button)
    detailed_sentiment()
  })

  # Generate sentiment distribution bar chart
  output$sentiment_bar_chart <- renderPlot({
    req(input$analyze_button)
    sentiment_data <- detailed_sentiment()
    if (!is.null(sentiment_data)) {
      sentiment_counts <- sentiment_data %>%
        group_by(sentiment_classification) %>%
        count()
      
      ggplot(sentiment_counts, aes(x = sentiment_classification, y = n, fill = sentiment_classification)) +
        geom_bar(stat = "identity") +
        labs(title = "Overall Sentiment Distribution of Reviews",
             x = "Sentiment",
             y = "Number of Reviews") +
        theme_minimal() +
        scale_fill_manual(values = c("Positive" = "green", "Negative" = "red", "Neutral" = "grey"))
    } else {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No data or analysis not yet run.", size = 6) +
        theme_void()
    }
  })

  # Generate word cloud for positive words
  output$positive_wordcloud <- renderPlot({
    req(input$analyze_button)
    word_data <- sentiment_words()
    if (!is.null(word_data)) {
      top_positive_words_df <- word_data %>%
        filter(value > 0) %>%
        count(word, wt = value) %>%
        arrange(desc(n)) %>%
        head(40)
      
      if (nrow(top_positive_words_df) > 0) {
        wordcloud(words = top_positive_words_df$word, freq = top_positive_words_df$n,
                  scale = c(2, 0.5), max.words = 80, random.order = FALSE,
                  colors = brewer.pal(8, "Greens"))
      } else {
        plot(NULL, xaxt = 'n', yaxt = 'n', bty = 'n', xlab = '', ylab = '')
        text(x = 0.5, y = 0.5, "No positive words found.", cex = 1.2)
      }
    } else {
      plot(NULL, xaxt = 'n', yaxt = 'n', bty = 'n', xlab = '', ylab = '')
      text(x = 0.5, y = 0.5, "Analysis not yet run.", size = 6)
    }
  })

  # Generate word cloud for negative words
  output$negative_wordcloud <- renderPlot({
    req(input$analyze_button)
    word_data <- sentiment_words()
    if (!is.null(word_data)) {
      top_negative_words_df <- word_data %>%
        filter(value < 0) %>%
        count(word, wt = abs(value)) %>%
        arrange(desc(n)) %>%
        head(40)
      
      if (nrow(top_negative_words_df) > 0) {
        wordcloud(words = top_negative_words_df$word, freq = top_negative_words_df$n,
                  scale = c(2, 0.5), max.words = 80, random.order = FALSE,
                  colors = brewer.pal(8, "Reds"))
      } else {
        plot(NULL, xaxt = 'n', yaxt = 'n', bty = 'n', xlab = '', ylab = '')
        text(x = 0.5, y = 0.5, "No negative words found.", cex = 1.2)
      }
    } else {
      plot(NULL, xaxt = 'n', yaxt = 'n', bty = 'n', xlab = '', ylab = '')
      text(x = 0.5, y = 0.5, "Analysis not yet run.", size = 6)
    }
  })

  # Generate overall sentiment summary
  output$overall_sentiment_summary <- renderUI({
    req(input$analyze_button)
    sentiment_data <- detailed_sentiment()
    if (!is.null(sentiment_data)) {
      total_reviews <- nrow(sentiment_data)
      positive_count <- sum(sentiment_data$sentiment_classification == "Positive")
      negative_count <- sum(sentiment_data$sentiment_classification == "Negative")
      neutral_count <- sum(sentiment_data$sentiment_classification == "Neutral")
      
      positive_percentage <- round((positive_count / total_reviews) * 100, 2)
      negative_percentage <- round((negative_count / total_reviews) * 100, 2)
      neutral_percentage <- round((neutral_count / total_reviews) * 100, 2)
      
      HTML(paste(
        "<h3>Overall Sentiment Breakdown:</h3>",
        "<p><b>Total Reviews Analyzed:</b> ", total_reviews, "</p>",
        "<p><b>Positive Sentiment:</b> ", positive_count, " (", positive_percentage, "%)</p>",
        "<p><b>Negative Sentiment:</b> ", negative_count, " (", negative_percentage, "%)</p>",
        "<p><b>Neutral Sentiment:</b> ", neutral_count, " (", neutral_percentage, "%)</p>"
      ))
    } else {
      HTML("<p>No sentiment analysis data available.</p>")
    }
  })

  # Generate sentiment confidence score histogram
  output$sentiment_confidence_histogram <- renderPlot({
    req(input$analyze_button)
    sentiment_data <- detailed_sentiment()
    if (!is.null(sentiment_data)) {
      ggplot(sentiment_data, aes(x = confidence_score, fill = sentiment_classification)) +
        geom_histogram(binwidth = 0.02, alpha = 0.7) +
        facet_wrap(~sentiment_classification, scales = "free_y") +
        labs(title = "Sentiment Confidence Score Distribution",
             x = "Confidence Score (Magnitude of Sentiment Score)",
             y = "Frequency",
             fill = "Sentiment") +
        theme_minimal()
    } else {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No sentiment analysis data available.", size = 6) +
        theme_void()
    }
  })

  # Output top 10 positive words
  output$top_positive_words <- renderPrint({
    req(input$analyze_button)
    word_data <- sentiment_words()
    if (!is.null(word_data)) {
      top_positive <- word_data %>%
        filter(value > 0) %>%
        count(word, wt = value) %>%
        arrange(desc(n)) %>%
        head(10)
      print(top_positive)
    } else {
      cat("No positive words found or analysis not run.")
    }
  })

  # Output top 10 negative words
  output$top_negative_words <- renderPrint({
    req(input$analyze_button)
    word_data <- sentiment_words()
    if (!is.null(word_data)) {
      top_negative <- word_data %>%
        filter(value < 0) %>%
        count(word, wt = abs(value)) %>%
        arrange(desc(n)) %>%
        head(10)
      print(top_negative)
    } else {
      cat("No negative words found or analysis not run.")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
