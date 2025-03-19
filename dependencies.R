# dependencies.R
# This script checks for and installs required packages.

required_packages <- c(
  "shiny",
  "shinydashboard",
  "dplyr",
  "ggplot2",
  "wordcloud",
  "tidytext",
  "stringr",
  "tidyr",
  "DT",
  "RColorBrewer"
)

# Check if packages are installed, and install if not
for (package in required_packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
}

# Load the packages
lapply(required_packages, require, character.only = TRUE)
