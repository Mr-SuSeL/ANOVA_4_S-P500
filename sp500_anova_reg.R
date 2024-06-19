# Data source link: https://www.kaggle.com/datasets/paytonfisher/sp-500-companies-with-financial-information/data?select=financials.csv
library(dplyr) 
library(ggplot2) 
# install.packages("plotly")
library(plotly)


# Floating decimal points = 3
options(digits = 3)

# Load data.
stocks <- read.csv(
  file = "financials.csv",
  header = TRUE,
  stringsAsFactors = TRUE # To sector analysis.
)

head(stocks, n = 5)
stocks <- subset(stocks, select = -SEC.Filings) # to remove col with links


