# dependencies.R

# List of required packages
required_packages <- c("shiny", "shinydashboard", "dplyr", "ggplot2",
                       "wordcloud", "tidytext", "stringr", "tidyr",
                       "DT", "RColorBrewer", "sentimentr", "textdata")

# Function to check and install missing packages
install_missing_packages <- function(package_list) {
  new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
  if(length(new_packages)) {
    install.packages(new_packages, repos = "https://cloud.r-project.org")
  }
}

# Install missing packages
install_missing_packages(required_packages)

# Load required packages (optional here, can be done in ui.R and server.R as well)
# library(shiny)
# library(shinydashboard)
# library(dplyr)
# library(ggplot2)
# library(wordcloud)
# library(tidytext)
# library(stringr)
# library(tidyr)
# library(DT)
# library(RColorBrewer)
# library(sentimentr)
