# ui.R
# User interface definition

library(shiny)
library(shinydashboard)

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
