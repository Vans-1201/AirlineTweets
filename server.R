# server.R
# Server logic definition

server <- function(input, output) {
  source("sentiment_functions.R") # source the file that contains all the functions.

  tweet_data <- shiny::reactive({
    shiny::req(input$tweet_file)
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
    analyze_sentiment(filtered_tweets())
  })

  sentiment_words <- shiny::reactive({
    shiny::req(input$analyze_button)
    prepare_word_data(filtered_tweets())
  })

  output$detailed_sentiment_table <- DT::renderDataTable({
    shiny::req(input$analyze_button)
    detailed_sentiment()
  })

  output$sentiment_bar_chart <- shiny::renderPlot({
    shiny::req(input$analyze_button)
    plot_sentiment_distribution(detailed_sentiment())
  })

  output$positive_wordcloud <- shiny::renderPlot({
    shiny::req(input$analyze_button)
    plot_positive_wordcloud(sentiment_words())
  })

  output$negative_wordcloud <- shiny::renderPlot({
    shiny::req(input$analyze_button)
    plot_negative_wordcloud(sentiment_words())
  })

  output$overall_sentiment_summary <- shiny::renderUI({
    shiny::req(input$analyze_button)
    generate_sentiment_summary(detailed_sentiment())
  })

  output$sentiment_by_airline_table <- DT::renderDataTable({
    shiny::req(input$analyze_button)
    generate_airline_sentiment_table(detailed_sentiment())
  })

  output$sentiment_comparison_chart <- shiny::renderPlot({
    shiny::req(input$analyze_button)
    plot_airline_sentiment_comparison(detailed_sentiment())
  })

  output$sentiment_confidence_histogram <- shiny::renderPlot({
    shiny::req(input$analyze_button)
    plot_confidence_histogram(detailed_sentiment())
  })

  output$top_positive_words <- shiny::renderPrint({
    shiny::req(input$analyze_button)
    print_top_positive_words(sentiment_words())
  })

  output$top_negative_words <- shiny::renderPrint({
    shiny::req(input$analyze_button)
    print_top_negative_words(sentiment_words())
  })
}
