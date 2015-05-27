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



                                                
momStrat <- function(prices, monthLookback,
                weightMom = 1, weightVol = 0, weightCor = 0, 
                riskFreeName = NULL, bestN,
                stepCorRank = TRUE, stepStartMethod = c("best", "default"),
                geometric = TRUE, ...) {
  stepStartMethod <- stepStartMethod[1]
  if(is.null(riskFreeName)) {
    prices$zeroes <- 0
    riskFreeName <- "zeroes"
    warning("No risk-free security specified. Recommended to use one of: quandClean('CHRIS/CME_US'), SHY, or VFISX. 
            Using vector of zeroes instead.")
  }
  returns <- Return.calculate(prices,method = "discrete")
  monthlyEps <- endpoints(prices, on = "months")
  riskFreeCol <- grep(riskFreeName, colnames(prices))
  tmp <- list()
  dates <- list()
  
  
  for(i in 2:(length(monthlyEps) - monthLookback)) {
    #subset data
    priceData <- prices[monthlyEps[i]:monthlyEps[i+monthLookback],]
    returnsData <- returns[monthlyEps[i]:monthlyEps[i+monthLookback],]
    
    #perform computations
    momentum <- data.frame(t(t(priceData[nrow(priceData),])/t(priceData[1,]) - 1))
    momentum <- momentum[,!is.na(momentum)]
    #momentum[is.na(momentum)] <- -1 #set any NA momentum to negative 1 to keep R from crashing
    priceData <- priceData[,names(momentum)]
    returnsData <- returnsData[,names(momentum)]
    
    momRank <- rank(momentum)
    vols <- data.frame(StdDev(returnsData))
    volRank <- rank(-vols)
    cors <- cor(returnsData, use = "complete.obs")
    if (stepCorRank) {
      if(stepStartMethod=="best") {
        weightVol = 0.5
        compositeMomVolRanks <- weightMom*momRank + weightVol*volRank
        maxRank <- compositeMomVolRanks[compositeMomVolRanks==max(compositeMomVolRanks)]
        corRank <- stepwiseCorRank(corMatrix=cors, startNames = names(maxRank), 
                                   bestHighestRank = TRUE, ...)
        
      } else {
        corRank <- stepwiseCorRank(corMatrix=cors, bestHighestRank=TRUE, ...)
      }
    } else {
      corRank <- rank(-rowSums(cors))
    }
    
    totalRank <- rank(weightMom*momRank + weightVol*volRank + weightCor*corRank)
    
    upper <- length(names(returnsData))
    lower <- max(upper-bestN+1, 1)
    topNvals <- sort(totalRank, partial=seq(from=upper, to=lower))[c(upper:lower)]
    
    #compute weights
    longs <- totalRank %in% topNvals #invest in ranks length - bestN or higher (in R, rank 1 is lowest)
    longs[momentum < 0] <- 0 #in previous algorithm, removed momentums < 0, this time, we zero them out at the end.
    longs <- longs/sum(longs) #equal weight all candidates
    longs[longs > 1/bestN] <- 1/bestN #in the event that we have fewer than top N invested into, lower weights to 1/top N
    names(longs) <- names(totalRank)
    
    
    #append removed names (those with momentum < 0)
    removedZeroes <- rep(0, ncol(returns)-length(longs))
    names(removedZeroes) <- names(returns)[!names(returns) %in% names(longs)]
    longs <- c(longs, removedZeroes)
    
    #reorder to be in the same column order as original returns/prices
    longs <- data.frame(t(longs))
    longs <- longs[, names(returns)]
    
    #append lists
    tmp[[i]] <- longs
    dates[[i]] <- index(returnsData)[nrow(returnsData)]
    
  }
  
  weights <- do.call(rbind, tmp)
  dates <- do.call(c, dates)
  
  
  weights <- xts(weights, order.by=as.Date(dates)) 
  weights[, riskFreeCol] <- weights[, riskFreeCol] + 1-rowSums(weights)
  strategyReturns <- Return.portfolio(R = returns, weights = weights, geometric = geometric)
  colnames(strategyReturns) <- paste(monthLookback, weightMom, weightVol, weightCor, sep="_")
  return(strategyReturns)
  }


stepwiseCorRank <- function(corMatrix, startNames=NULL, stepSize=1, bestHighestRank=FALSE) {
  #edge cases
  if(dim(corMatrix)[1] == 1) {
    return(corMatrix)
  } else if (dim(corMatrix)[1] == 2) {
    ranks <- c(1.5, 1.5)
    names(ranks) <- colnames(corMatrix)
    return(ranks)
  }
  
  if(is.null(startNames)) {
    corSums <- rowSums(corMatrix)
    corRanks <- rank(corSums)
    startNames <- names(corRanks)[corRanks <= stepSize]
  }
  nameList <- list()
  nameList[[1]] <- startNames
  rankList <- list()
  rankCount <- 1
  rankList[[1]] <- rep(rankCount, length(startNames))
  rankedNames <- do.call(c, nameList)
  
  while(length(rankedNames) < nrow(corMatrix)) {
    rankCount <- rankCount+1
    subsetCor <- corMatrix[, rankedNames]
    if(class(subsetCor) != "numeric") {
      subsetCor <- subsetCor[!rownames(corMatrix) %in% rankedNames,]
      if(class(subsetCor) != "numeric") {
        corSums <- rowSums(subsetCor)
        corSumRank <- rank(corSums)
        lowestCorNames <- names(corSumRank)[corSumRank <= stepSize]
        nameList[[rankCount]] <- lowestCorNames
        rankList[[rankCount]] <- rep(rankCount, min(stepSize, length(lowestCorNames)))
      } else { #1 name remaining
        nameList[[rankCount]] <- rownames(corMatrix)[!rownames(corMatrix) %in% names(subsetCor)]
        rankList[[rankCount]] <- rankCount
      }
    } else {  #first iteration, subset on first name
      subsetCorRank <- rank(subsetCor)
      lowestCorNames <- names(subsetCorRank)[subsetCorRank <= stepSize]
      nameList[[rankCount]] <- lowestCorNames
      rankList[[rankCount]] <- rep(rankCount, min(stepSize, length(lowestCorNames)))
    }    
    rankedNames <- do.call(c, nameList)
  }
  
  ranks <- do.call(c, rankList)
  names(ranks) <- rankedNames
  if(bestHighestRank) {
    ranks <- 1+length(ranks)-ranks
  }
  ranks <- ranks[colnames(corMatrix)] #return to original order
  return(ranks)
}

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

                                              
                                                

## ---- cortable
#returns <- (Return.calculate(mergedData))
#correlationTable <- cor(returns[-1,])
#stargazer(correlationTable, title = "Correlation coefficients of the daily market returns, 1998-2014")

## ---- stats
#statistics
# basicStats(mergedData)

## ---- FinalIn
  # in-sample backtest
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
                                                
## ---- buy and hold
#   BH <- function(prices, weights){
#     returns <- Return.calculate(prices,method = "discrete")
#     strategy <- Return.portfolio(returns, weights = weights, wealth.index = TRUE, verbose = TRUE)
#     return(strategy)
#   }
#   weights <-  c(1/7, 1/7, 1/7, 1/7, 1/7, 1/7, 1/7)
#   buyandhold <- BH(outSample[,1:7], as.xts(weights)) 
#   charts.PerformanceSummary(as.xts(buyandhold))
#                                                                                                 
## ---- test
# Other tests
# originalIn <- FAA(inSample, riskFreeName="VFISX", monthLookback = 6)
# swcIn <- FAA(inSample, riskFreeName="VFISX", stepCorRank = TRUE, monthLookback = 6)
# benchmarkinSample <- as.xts(window(benchmarkReturns, start = insampleStart, end = insampleEnd))
# charts.PerformanceSummary(insamplePlot,  main=6)
# originalOut <- FAA(outSample, riskFreeName="VFISX", monthLookback = 6)
# swcOut <- FAA(outSample, riskFreeName="VFISX", stepCorRank = TRUE, monthLookback = 6)
# benchmarkoutSample <- as.xts(window(benchmarkReturns, start =outsampleStart, end = outsampleEnd))
# outsamplePlot <- cbind(originalOut, swcOut, benchmarkoutSample)
# names(outsamplePlot) <- c("original", "swc", "benchmarkoutSample")
# charts.PerformanceSummary(outsamplePlot, main=6)
# print(Return.annualized(swcIn))
# print(Return.annualized(swcOut))                                               





