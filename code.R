## ---- main
library(FinancialInstrument)
library(TTR)
library(PerformanceAnalytics)
library(fPortfolio)
require(xts)
require(timeSeries)
require(stargazer)

################Functions flexible########
FAA <- function(prices, monthLookback = 4,
                weightMom = 1, weightVol = .5, weightCor = .5, 
                riskFreeName = NULL, bestN = 3,
                stepCorRank = FALSE, stepStartMethod = c("best", "default"),
                geometric = TRUE, ...) {
  stepStartMethod <- stepStartMethod[1]
  if(is.null(riskFreeName)) {
    prices$zeroes <- 0
    riskFreeName <- "zeroes"
    warning("No risk-free security specified. Recommended to use one of: quandClean('CHRIS/CME_US'), SHY, or VFISX. 
            Using vector of zeroes instead.")
  }
  returns <- Return.calculate(prices)
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
  strategyReturns <- Return.rebalancing(R = returns, weights = weights, geometric = geometric)
  colnames(strategyReturns) <- paste(monthLookback, weightMom, weightVol, weightCor, sep="_")
  return(strategyReturns)
  }

#'Stepwise Correlation Rank
#'@description computes a stepwise correlation ranking of vectors starting from a given subset of vectors
#'@param corMatrix a correlation matrix
#'@param startNames names of the vectors to start initial ranking from. If NULL, the stepSize amount of vectors with the lowest
#'overall correlation will be the initial vectors to start the process. Default NULL
#'@param stepSize how many vectors per ranking step (EG stepSize of 2 adds the 2 lowest correlation-ranked vectors to 
#'existing set at each step). Default 1
#'@param bestHighestRank whether or not to assign the highest rank (number of vectors) to the minimum-correlated vector(s). 
#'TRUE variant used in Flexible Asset Allocation (FAA). Default FALSE
#'@return a set of ranks for the given names of the vectors in the correlation matrix
#'@references \url{http://quantstrattrader.wordpress.com/2014/10/27/introducing-stepwise-correlation-rank/}
#'\cr \url{http://cssanalytics.wordpress.com/2014/10/27/flexible-asset-allocation-with-conditional-correlations/}
#'@export
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

# Preping the csv file
dataFrame <- read.csv("~/data.csv", sep=";")
data <- dataFrame[order(nrow(dataFrame):1),]
data$Date <- format(as.Date(data$Date, "%m/%d/%Y"))
write.csv(data, "~/data3.csv", row.names=FALSE, quote=FALSE)
dataCsv <- readSeries("~/data3.csv", header=TRUE, sep=",", format="%Y-%m-%d")
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

#merge all the data
mergedData <- merge(as.timeSeries(dataCsv),alignedVFISX)



#with and without stewise correlation ranking
original <- FAA(mergedData, riskFreeName = "VFISX")
swc <- FAA(mergedData, riskFreeName="VFISX", stepCorRank = TRUE)


original6 <- FAA(mergedData, riskFreeName = "VFISX", monthLookback = 3)  
swc6 <- FAA(mergedData, riskFreeName="VFISX", stepCorRank = TRUE, monthLookback = 3)


## ---- strategies
#plotting the data
plot <- cbind(original, swc, original6, swc6)
names(plot) <- c("original", "swc", "original6", "swc6")
charts.PerformanceSummary(plot)

## ---- cortable
#correlation table - eliminate first row
returns <- (Return.calculate(dataCsv))
correlationTable <- cor(returns[-1,])
stargazer(correlationTable, title = "Correlation coefficients of the daily market returns, 1998-2014")

## ---- stats
#statistics
basicStats(mergedData)

