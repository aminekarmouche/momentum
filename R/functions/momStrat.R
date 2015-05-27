                                               
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