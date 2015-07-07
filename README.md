# momentum
Momentum Investment Framework in R for my masters thesis. Updates will be pushed after the thesis defense!
- Financial Data collection from Yahoo finance (MENA financial markets)
- Descriptive statistics 
- Momentum Strategy Backtesting (1998-2014)
- Strategy Performance Analytics (using annualized returns and financial ratios such as Sharpe and Calmar)

Enhanced momentum strategy ranking framework using the following second order characteristics
- Return momentum
- Stepwise Correlation
- Volatility 

# Prerequisites
Packages include
- Quantmod
- FinancialInstrument
- Performance Analytics

# Running the scripts
## Strategies
```
r -f main.r
```

##Data
Data should be formatted in csv format and put in Data/master.csv

##Descriptive stats
in R/functions
```
r -f stats.r
```






