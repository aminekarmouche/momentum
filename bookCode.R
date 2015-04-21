library(FinancialInstrument)
library(TTR)
library(PerformanceAnalytics)
library(fPortfolio)
require(xts)
require(timeSeries)

#getting the data
# Preping the csv file
dataFrame <- read.csv("~/data.csv", sep=";")
data <- dataFrame[order(nrow(dataFrame):1),]
data$Date <- format(as.Date(data$Date, "%m/%d/%Y"))
write.csv(data, "~/data3.csv", row.names=FALSE, quote=FALSE)
dataCsv <- readSeries("~/data3.csv", header=TRUE, sep=",", format="%Y-%m-%d")
dataCsv <- as.xts(dataCsv)


#Vanguard Short-Term Treasury Fund
mutualFunds <- c("VFISX", "S&P 500")

#all the period
getSymbols(mutualFunds, from="1997-12-31", to="2015-02-18")
tmp <- list()
for(fund in mutualFunds) {
  tmp[[fund]] <- Ad(get(fund))
}

#risk free asset is VFISX
riskFree <- do.call(cbind, args = tmp)
colnames(riskFree) <- gsub(".Adjusted", "", colnames(riskFree))
alignedVFISX <- align(x = as.timeSeries(riskFree), by = "1d", method = "before", include.weekends = FALSE)

#merge risk free
mergedData <- merge(as.timeSeries(dataCsv),alignedVFISX)

#BOOK BACKTEST
returns <- Return.calculate(mergedData)*100
returns <- returns[-1,]
specs <- portfolioSpec()
const <- "LongOnly"
backtest <- portfolioBacktest()
setWindowsHorizon(backtest) <- "18m"
setSmootherLambda(backtest) <- "6m"
backtestFormula <-  S.P.500 ~ MOR + TUN + EGY + KSA + LEB + JOR + TUR + VFISX

Portfolio <- portfolioBacktesting(formula = backtestFormula, data = returns, spec = specs, constraints = const, trace = FALSE)
# weights <- round(100 * Portfolio$weights, 2) [1:194, ]

portfolioSmooth <- portfolioSmoothing(object = Portfolio, backtest = backtest, trace = FALSE)
backtestPlot(portfolioSmooth, cex = 0.6, font = 1, family = "mono")

# Strategies
# feasiblePortfolio
# cmlPortfolio
# tangencyPortfolio
# minvariancePortfolio
# efficientPortfolio
setStrategyFun(backtest) <- "efficientPortfolio"
portfolioSmooth <- portfolioSmoothing(object = Portfolio, backtest = backtest, trace = FALSE)
backtestPlot(portfolioSmooth, cex = 0.6, font = 1, family = "mono")

