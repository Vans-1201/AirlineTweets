# ui.R
# User interface definition

ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "Airline Tweet Sentiment Analysis"),
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Data Input", tabName = "data_input", icon = shiny::icon("upload")),
      shinydashboard::menuItem("Analysis", tabName = "analysis", icon = shiny::icon("chart-bar")),
      shinydashboard::menuItem("Summary", tabName = "summary", icon = shiny::icon("info-circle"))
    )
  ),
  shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      shinydashboard::tabItem(tabName = "data_input",
        shiny::fluidRow(
          shinydashboard::box(
            title = "Upload Tweet Data",
            width = 12,
            shiny::fileInput("tweet_file", "Choose CSV File",
              accept = c("text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")),
            shiny::selectInput("airline_choice", "Select Airline",
              choices = c("All", "United", "American", "Delta", "Southwest", "JetBlue", "Spirit", "Alaska")),
            shiny::actionButton("analyze_button", "Run Analysis")
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Uploaded Tweet Data",
            width = 12,
            shiny::dataTableOutput("tweet_table_raw")
          )
        )
      ),
      shinydashboard::tabItem(tabName = "analysis",
        shiny::fluidRow(
          shinydashboard::box(
            title = "Overall Sentiment Distribution",
            width = 6,
            shiny::plotOutput("sentiment_bar_chart")
          ),
          shinydashboard::box(
            title = "Word Cloud of Positive Words",
            width = 6,
            shiny::plotOutput("positive_wordcloud")
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Word Cloud of Negative Words",
            width = 6,
            shiny::plotOutput("negative_wordcloud")
          ),
          shinydashboard::box(
            title = "Detailed Tweet Sentiment",
            width = 12,
            shiny::dataTableOutput("detailed_sentiment_table")
          )
        )
      ),
      shinydashboard::tabItem(tabName = "summary",
        shiny::fluidRow(
          shinydashboard::box(
            title = "Overall Sentiment Summary",
            width = 12,
            shiny::htmlOutput("overall_sentiment_summary")
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Sentiment by Airline",
            width = 12,
            shiny::dataTableOutput("sentiment_by_airline_table")
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Comparison of Overall Sentiment Across Airlines",
            width = 12,
            shiny::plotOutput("sentiment_comparison_chart")
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Sentiment Confidence Score Distribution",
            width = 12,
            shiny::plotOutput("sentiment_confidence_histogram")
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Top 10 Positive Words",
            width = 6,
            shiny::verbatimTextOutput("top_positive_words")
          ),
          shinydashboard::box(
            title = "Top 10 Negative Words",
            width = 6,
            shiny::verbatimTextOutput("top_negative_words")
          )
        )
      )
    )
  )
)
