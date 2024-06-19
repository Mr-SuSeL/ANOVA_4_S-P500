# Data source link: https://www.kaggle.com/datasets/paytonfisher/sp-500-companies-with-financial-information/data?select=financials.csv
# dataset ends in July 2020
library(dplyr) 
library(ggplot2) 
# install.packages("plotly")
library(plotly)
library(tidyverse)


# floating decimal points = 3
options(digits = 3)

# load data.
stocks <- read.csv(
  file = "financials.csv",
  header = TRUE,
  stringsAsFactors = TRUE # to sector analysis.
)

head(stocks, n = 5)
stocks <- subset(stocks, select = -SEC.Filings) # to remove col with links
colnames(stocks)
#[1] "Symbol"         "Name"           "Sector"         "Price"          "Price.Earnings"
#[6] "Dividend.Yield" "Earnings.Share" "X52.Week.Low"   "X52.Week.High"  "Market.Cap"    
#[11] "EBITDA"         "Price.Sales"    "Price.Book"
# shorten names of columns
names(stocks) <- c("symbol", "name", "sector", "price", "pe", "div_yield",
                   "eps", "low", "high", "market_cap", "ebitda", "ps", "pb")

str(stocks)
sum(is.na(stocks))

# dataframe grouping sectors together
df <- stocks %>%
  group_by(sector) %>%
  summarise(
    count = n(),
    avg.price = as.integer(mean(price)),
    med.pe = median(pe, na.rm = TRUE),
    avg.eps = mean(eps),
    cap = median(market_cap),
    ebitda = median(ebitda),
    ps = mean(ps),
    pb = median(pb, na.rm = TRUE)
  )
df # 11 sectors in U.S. economy

str(df)
nrow(stocks)
median(stocks$market_cap) # 21,4 mld $

# chart with grouped sectors from the biggest
t <- df %>%
  arrange(count) %>%
  mutate(sector = factor(sector, levels = sector)) %>%
  ggplot(aes(x = sector, y = count)) +
  geom_segment(aes(xend = sector, yend = 0)) +
  geom_point(size = 4, color = "black", fill = alpha("green", 0.4), alpha = 0.7, 
             shape = 21, stroke = 3) + coord_flip() + xlab("") + theme_bw()
t
# IT bigger than Fin :)

## Barplot of the sectors with the highest prices.
# Order stock prices from lowest to highest.
df.price <- df[order(df$avg.price),]

# Make sure sector names are ordered with prices.
df.price$sector <- factor(df.price$sector, levels = df.price$sector)
df.price

# Create the graph.
ggplot(data = df.price, aes(x = sector, y = avg.price)) +
  geom_bar(stat="identity", fill = "#999999", color = "black") +
  scale_x_discrete(labels=c("Telecom", "Utilities", "Energy", "Staples", "RealEstate",
                            "Financials", "Materials", "Industrials", "IT",
                            "Cons. Disc", "Healthcare")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=55, vjust=0.5))
# Healthcare seems to be overvalued

## Barplot of highest market cap.
df.cap <- df[order(df$cap),]
df.cap$sector <- factor(df.cap$sector, levels = df.cap$sector)

# telecom...
ggplot(data = df.cap, aes(x = sector, y = cap)) +
  geom_bar(stat="identity", fill = "#999999", color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=55, vjust=0.5))
# only 3 telecom companies -> delete that sector
# Capitalisation without telecom
df.cap.new <- subset(df.cap, sector != "Telecommunication Services")
df.cap.new

ggplot(data = df.cap.new, aes(x = sector, y = cap)) +
  geom_bar(stat="identity", fill = "#999999", color = "black") +
  scale_x_discrete(labels=c("Cons. Disc", "Real Estate", "Materials",
                            "Utilities", "Industrials","Energy",
                            "Financials","IT", "Staples", "Healthcare")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=55, vjust=0.5))


## Barplot of highest EBITDA.
# EBITDA (ang. earnings before interest, taxes, depreciation and amortization) 
# – zysk operacyjny przedsiębiorstwa przed potrąceniem odsetek od zaciągniętych
# kredytów i obligacji...
df.ebit <- df[order(df$ebitda),]
df.ebit$sector <- factor(df.ebit$sector, levels = df.ebit$sector)

df.ebit.new <- subset(df.ebit, sector != "Telecommunication Services")
df.ebit.new

ggplot(data = df.ebit.new, aes(x = sector, y = ebitda)) +
  geom_bar(stat="identity", fill = "#999999", color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=55, vjust=0.5))

# Info source: https://rc2e.com/linearregressionandanova









































