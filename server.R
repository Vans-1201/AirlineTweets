# server.R
# Server logic definition

server <- function(input, output) {

  tweet_data <- shiny::reactive({
    shiny::req(input$tweet_file) # Ensure a file is uploaded first
    tryCatch(
      read.csv(input$tweet_file$datapath, stringsAsFactors = FALSE),
      error = function(e) {
        shiny::stop(shiny::safeError(e))
      }
    )
  })

  filtered_tweets <- shiny::reactive({
    data <- tweet_data()
    if (input$airline_choice != "All") {
      data <- data[grepl(input$airline_choice, data$text, ignore.case = TRUE), ]
    }
    data
  })

  output$tweet_table_raw <- DT::renderDataTable({
    filtered_tweets()
  })

  detailed_sentiment <- shiny::reactive({
    shiny::req(input$analyze_button)
    tweets <- filtered_tweets()
    if (nrow(tweets) > 0) {
      # --- Text Preprocessing ---
      cleaned_tweets <- tweets %>%
        dplyr::mutate(text = stringr::str_replace_all(text, "can't", "cannot")) %>%
        dplyr::mutate(text = stringr::str_replace_all(text, "won't", "will not"))

      # --- Manual Stopwords List ---
      manual_stopwords <- c(
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
        "than", "too", "very", "s", "t", "can", "will", "just", "don", "should", "now",
        "jetblue", "southwestair", "united", "americanair", "delta", "spiritairlines", "alaskaair" # Airline names
      )

      tweet_sentiment <- cleaned_tweets %>%
        tidytext::unnest_tokens(word, text) %>%
        dplyr::filter(!(word %in% manual_stopwords))

      # --- Emoji Sentiment Analysis (Placeholder - requires an emoji lexicon) ---
      # You would need to implement logic here to extract emojis and map them to sentiment scores

      # --- ML-Based Sentiment Classification (Placeholder - requires labeled data and training) ---
      # If you have a trained ML model, you would load it here and use it to predict sentiment.
      # For now, we'll fall back to lexicon-based sentiment.
      afinn_lexicon <- tidytext::get_sentiments("afinn")
      tweet_sentiment <- tweet_sentiment %>%
        dplyr::left_join(afinn_lexicon, by = "word") %>%
        dplyr::group_by(index = 1:n()) %>%
        dplyr::summarise(
          text = first(cleaned_tweets$text[index]), # Keep original text (after basic preprocessing)
          sentiment_score = sum(value, na.rm = TRUE)
        ) %>%
        dplyr::mutate(
          sentiment_classification = dplyr::case_when(
            sentiment_score >= 2 ~ "Positive",
            sentiment_score <= -2 ~ "Negative",
            sentiment_score > -2 & sentiment_score < 2 ~ "Neutral",
            TRUE ~ "Neutral"
          ),
          confidence_score = abs(sentiment_score) # Basic confidence based on score magnitude
        )

      # --- Sarcasm Detection (Basic Rule-Based) ---
      tweet_sentiment <- tweet_sentiment %>%
        dplyr::mutate(sentiment_classification = ifelse(grepl("yeah right|of course|thanks for nothing", text, ignore.case = TRUE) & sentiment_classification == "Positive", "Negative", sentiment_classification))

      # --- Contextual Keyword Handling (Placeholder - requires more advanced logic) ---
      # You would implement rules here to adjust sentiment based on the proximity of positive words to negative event keywords.

      tweet_sentiment
    } else {
      NULL
    }
  })

  sentiment_words <- shiny::reactive({
    shiny::req(input$analyze_button)
    tweets <- filtered_tweets()
    if (nrow(tweets) > 0) {
      # --- Manual Stopwords List for word clouds ---
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
        "than", "too", "very", "s", "t", "can", "will", "just", "don", "should", "now",
        "jetblue", "southwestair", "united", "americanair", "delta", "spiritairlines", "alaskaair" # Airline names
      )
      afinn_lexicon <- tidytext::get_sentiments("afinn")
      tweets %>%
        dplyr::mutate(text = stringr::str_replace_all(text, "can't", "cannot")) %>%
        dplyr::mutate(text = stringr::str_replace_all(text, "won't", "will not")) %>%
        tidytext::unnest_tokens(word, text) %>%
        dplyr::filter(!(word %in% manual_stopwords_wc)) %>%
        dplyr::inner_join(afinn_lexicon)
    } else {
      NULL
    }
  })

  output$detailed_sentiment_table <- DT::renderDataTable({
    shiny::req(input$analyze_button)
    detailed_sentiment()
  })

  output$sentiment_bar_chart <- shiny::renderPlot({
    shiny::req(input$analyze_button)
    sentiment_data <- detailed_sentiment()
    if (!is.null(sentiment_data)) {
      sentiment_counts <- sentiment_data %>%
        dplyr::group_by(sentiment_classification) %>%
        dplyr::count()

      ggplot2::ggplot(sentiment_counts, ggplot2::aes(x = sentiment_classification,
