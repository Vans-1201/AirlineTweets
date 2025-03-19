### dependencies.R
# This script installs required packages if missing and loads them

# List of required packages
required_packages <- c("shiny", "shinydashboard", "dplyr", "ggplot2", "wordcloud", "tidytext", "stringr", "tidyr", "DT")

# Function to check and install missing packages
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Install and load all required packages
invisible(sapply(required_packages, install_if_missing))
