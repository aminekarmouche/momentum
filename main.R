                                                #####################
                                                ######Main code######
                                                #####################
## ---- main
library(FinancialInstrument)
library(TTR)
library(PerformanceAnalytics)
library(fPortfolio)
library(xts)
library(timeSeries)
library(stargazer)
library(quantmod)
source("R/functions.R")
source("R/utilities.R")

# Preping the csv file (refer to datafile.R)
dataCsv <- readSeries("~/master.csv", header=TRUE, sep=",", format="%Y-%m-%d")
dataCsv <- as.xts(dataCsv) 

                                                
#Vanguard Short-Term Treasury Fund
mutualFunds <- c("VFISX")

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

#in and out of sample periods
insampleStart <- "1998-01-04"
insampleEnd <- "2004-12-31"                                               
                                                
outsampleStart <- "2005-01-01"
outsampleEnd <- "2015-02-18"

                                                
inSample <- window(mergedData, start = insampleStart, end = insampleEnd)
outSample <- window(mergedData, start = outsampleStart, end = outsampleEnd)
                                                

# benchmark is S&P500                                                
benchmarkIndex <- c("%5EGSPC")

#get benchmark data for all the period
getSymbols(benchmarkIndex, from="1997-12-31", to="2015-02-18")
  tmp <- list()
  for(fund in benchmarkIndex) {
  tmp[[fund]] <- Ad(get(fund))
}
benchmark <- do.call(cbind, args = tmp)
colnames(benchmark) <- gsub(".Adjusted", "", colnames(benchmarkIndex))
benchmark <- alignedVFISX <- align(x = as.timeSeries(benchmark), by = "1d", method = "before", include.weekends = FALSE)                                      
benchmarkReturns <- Return.calculate(benchmark)

  originalIn1 <- momStrat(inSample, riskFreeName="VFISX", monthLookback = 1, bestN = 4)
  originalIn3 <- momStrat(inSample, riskFreeName="VFISX", monthLookback = 3, bestN = 4)
  originalIn4 <- momStrat(inSample, riskFreeName="VFISX", monthLookback = 4, bestN = 4)                                              
  originalIn6 <- momStrat(inSample, riskFreeName="VFISX", monthLookback = 6, bestN = 4)
  originalIn9 <- momStrat(inSample, riskFreeName="VFISX", monthLookback = 9, bestN = 4)
                                                
  benchmarkinSample <- as.xts(window(benchmarkReturns, start = insampleStart, end = insampleEnd))
  insamplePlot <- cbind(originalIn1, originalIn3, originalIn4, originalIn6, originalIn9)
  names(insamplePlot) <- c("1 month","3 months", "4 months","6 months", "9 months")
  charts.PerformanceSummary(insamplePlot,  main="")
  
  statsTable <- data.frame(t(rbind(Return.annualized(insamplePlot)*100,
                                   maxDrawdown(insamplePlot)*100,
                                   SharpeRatio.annualized(insamplePlot),
                                   CalmarRatio(insamplePlot))))
  statsTable$ReturnDrawdownRatio <- statsTable[,1]/statsTable[,2]
  print(statsTable)
  stargazer(as.matrix(statsTable), title = "Correlation coefficients of the daily market returns, 1998-2014",
            font.size = "tiny",
            column.sep.width = "0.5pt")                                                                
  
  # Out of sample backtest with optimal lookback period
  originalOut <- momStrat(outSample, riskFreeName="VFISX", monthLookback = 4, bestN = 4)
  #swcOut <- FAA(outSample, riskFreeName="VFISX", stepCorRank = TRUE, monthLookback = 9, weightMom = 1, weightVol = 0, weightCor = 0)
  benchmarkoutSample <- as.xts(window(benchmarkReturns, start =outsampleStart, end = outsampleEnd))
  outsamplePlot <- cbind(originalOut, benchmarkoutSample)
  names(outsamplePlot) <- c("Momentum Strategy", "S&P500 Buy & Hold")
  charts.PerformanceSummary(outsamplePlot, main="")
  
  statsTable <- data.frame(t(rbind(Return.annualized(outsamplePlot)*100,
                                   maxDrawdown(outsamplePlot)*100,
                                   SharpeRatio.annualized(outsamplePlot),
                                   CalmarRatio(outsamplePlot))))
  statsTable$ReturnDrawdownRatio <- statsTable[,1]/statsTable[,2]
  print(statsTable)   
  stargazer(as.matrix(statsTable), title = "Correlation coefficients of the daily market returns, 1998-2014",
    font.size = "tiny",
    column.sep.width = "1pt")
## ---- statsoutsample                                                
  stats <- stat.desc(outsamplePlot[-1,], basic = F)
  stargazer(as.matrix(stats),
            title = "Statistics summary, 1998-2014",
            font.size = "tiny",
            column.sep.width = "1pt")
                                          