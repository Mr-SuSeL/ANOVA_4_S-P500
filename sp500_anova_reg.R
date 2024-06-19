# Data source link: https://www.kaggle.com/datasets/paytonfisher/sp-500-companies-with-financial-information/data?select=financials.csv
# dataset ends in July 2020
library(dplyr) 
library(ggplot2) 
# install.packages("plotly")
library(plotly)
library(tidyverse)
# install.packages("ggcorrplot")
library(ggcorrplot)
# install.packages("car") # to VIF
library(car)


# floating decimal points = 3
options(digits = 3)

# load data.
stocks <- read.csv(
  file = "financials.csv",
  header = TRUE,
  stringsAsFactors = TRUE # to sector analysis.
)

# ----------------- cleaning data ----------------------------------------------

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

## ------------ understanding sector's data (charts)----------------------------

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
# that's mean DEBT Economy :)

# Custom colors.
v_color <- viridis::viridis(n = nrow(stocks))
stocks$color <- v_color[Matrix::invPerm(
  p = order(
    x = stocks$ebitda
  )
)]

pairs(
  formula = ebitda ~ pe + div_yield + eps + sector +
    market_cap + ps + pb,
  data = stocks,
  pch = 20,
  col = stocks$color
)

stocks.cor <- subset(stocks, select=-c(1,2,3,8,9,14))
corr <- round(cor(stocks.cor), 2)
corr

# Correlation chart
ggcorrplot::ggcorrplot(corr,
                       type = "lower",
                       lab = TRUE,
                       lab_size = 3,
                       method = "circle",
                       colors = c("red", "yellow", "green"),
                       title = "Correlation of Variables",
                       ggtheme = theme_bw
)

## ----------------- REGRESSION MODEL ------------------------------------------
# Jaki sektor ma istotny statystycznie udział w EBITDA...
fit.ebit <- lm(
  formula = ebitda ~ market_cap + pe + ps + eps + sector +
    div_yield  + pb,
  data = stocks
)
summary(fit.ebit)

# VIF: Variance Inflation Factor from 'car' package
car::vif(fit.ebit)
#GVIF              Df         GVIF^(1/(2*Df))
#market_cap 1.15  1            1.07
#pe         1.11  1            1.05
#ps         1.67  1            1.29
#eps        1.16  1            1.08
#sector     3.22 10            1.06
#div_yield  1.80  1            1.34
#pb         1.05  1            1.03
# stopa dywidendy prowadzi

# diagnostic charts plot
plot(x = fit.ebit, col = stocks$color, pch = 20, which = 1:6)
names(fit.ebit)
# Residuals:
plot(fit.ebit$residuals)
# Q-Q Residuals:
plot(x = fit.ebit, col = stocks$color, pch = 20, which = 2)
#hist(fit.ebit$fitted.values)

## Boxplot
boxplot(x = stocks$ebitda, ylab = "EBITDA")

## Q-Q EBITDA graph
# Rozkład międzykwartylowy
qqnorm(y = stocks$ebitda, col = stocks$color, pch = 20, ylab = "EBITDA")
qqline(y = stocks$ebitda, lty = 2, col = 2)

## -------------- power transformations - transformata potęgowa ----------------
# Info source: https://rc2e.com/linearregressionandanova
stocks2 <- stocks
# Count number of negative values in market_cap.
nrow(stocks2[stocks2$market_cap<0,])

# Check powerTransform() on market_cap
pt_marketCap <- car::powerTransform(
  object = market_cap ~ 1, data = stocks2)
summary(pt_marketCap) # -0.33 Rounded power

# Count number of negative, 0, and NA values
nrow(stocks2[stocks2$pe < 0,]) # 13
nrow(stocks2[stocks2$pe == 0,]) # 2

# powerTransform() on pe
pt_pe <- car::powerTransform(object = pe ~ 1, data = stocks2, family = "bcnPower")
summary(object = pt_pe) #  0.191 rounded power now positive

# powerTransform() on ps
pt_ps <- car::powerTransform(object = ps ~ 1, data = stocks2)
summary(object = pt_ps) #  (0 == log transformation)

# powerTransform() on eps
nrow(stocks2[stocks2$eps<0,]) # 51

pt_eps <- car::powerTransform(object = eps ~ 1, data = stocks2, family = "bcnPower")
summary(object = pt_eps) # 0.229 rounded power

# powerTransform() on pb
nrow(stocks2[stocks2$pb<0,]) #8

#pt_pb <- car::powerTransform(object = pb ~ 1, data = stocks2, family = "bcnPower")
pt_pb <- car::powerTransform(object = pb ~ 1, data = stocks2)
summary(object = pt_pb) # <- -0.33 rounded power

# powerTransform() on div_yield
nrow(stocks2[stocks2$div_yield<0,])
nrow(stocks2[stocks2$div_yield==0,]) # 86

pt_div <- car::powerTransform(object = div_yield ~ 1, data = stocks2,
  family = "bcnPower")
summary(object = pt_div) # 0.5 rounded power

# Build powerTransform() model on ebitda
nrow(stocks[stocks$ebitda<0,]) # 9
nrow(stocks[stocks$ebitda==0,]) # 58

pt_ebit <- car::powerTransform(
  object = ebitda ~ I(market_cap^-0.33) + I(pe^0.191) + log(ps) + 
    I(eps^0.229) + I(div_yield^0.5) + I(pb^-0.33), data = stocks2,
  family = "bcnPower")
summary(object = pt_ebit) # r. power = 0 -> log

## --------- New regression model with transformed data ------------------------
new_fit <- lm(formula = ebitda ~ I(market_cap^-0.33) + I(pe^0.191) + log(ps) + 
    I(eps^0.229) + sector + I(div_yield^0.5) + I(pb^-0.33), data = stocks)
summary(new_fit)
car::vif(new_fit)
#                    GVIF Df GVIF^(1/(2*Df))
#I(market_cap^-0.33) 1.32  1            1.15
#I(pe^0.191)         1.82  1            1.35
#log(ps)             1.95  1            1.40
#I(eps^0.229)        1.23  1            1.11
#sector              3.76 10            1.07
#I(div_yield^0.5)    1.90  1            1.38
#I(pb^-0.33)         1.77  1            1.33
# tym razem najwyższa wartość dla ps (price-to-sell)

plot(
  x = new_fit,
  col = stocks$color,
  pch = 20,
  which = 1:6
)

na.sums <- colSums(is.na(stocks2))
miss.vals <- data.frame(
  na = na.sums
)
miss.vals
# pe and pb

# median imputancy
sum(is.na(stocks2))

stocks2$pe <- Hmisc::impute(stocks2$pe, median)
stocks2$pb <- Hmisc::impute(stocks2$pb, median)
str(stocks2)
sum(is.na(stocks2$pb))
# is.na(stocks2$pe)

simple_fit <- lm(
  formula = ebitda ~ market_cap + pe + ps +
    eps + sector + div_yield,
  data = stocks2
)
summary(simple_fit)
car::vif(simple_fit)

anova(simple_fit)





























































