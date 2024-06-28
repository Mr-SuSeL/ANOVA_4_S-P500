# one-way ANOVA S&P 500 analysis
We are looking for macroeconomic factors which has influence to EBITDA for companies from S&P 500 index

## What we do:
- download data from kaggle: https://www.kaggle.com/datasets/paytonfisher/sp-500-companies-with-financial-information/data?select=financials.csv
- cleaning dataset
- choose model - multi linear regression
- VIF measures inflantion of variance and looking for coolinear in variuables
- correlation matrix
- elimination outliers (telecom etc.)
- step algo looking for best Akaik's metrics (aic with step from MASS package)
- Cook's distance and other plots
- one-way ANOVA report telling as which param has important influence for S&P500 price