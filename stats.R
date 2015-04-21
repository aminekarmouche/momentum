################################################################
################################## STATS #######################
################################################################


## ---- main
library(FinancialInstrument)
library(TTR)
library(PerformanceAnalytics)
library(fPortfolio)
require(xts)
require(timeSeries)
require(stargazer)

#correlation table
corstarsl <- function(x){ 
  require(Hmisc) 
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew) 
}

# Preping the csv file
dataFrame <- read.csv("~/data.csv", sep=";")
data <- dataFrame[order(nrow(dataFrame):1),]
data$Date <- format(as.Date(data$Date, "%m/%d/%Y"))
write.csv(data, "~/data3.csv", row.names=FALSE, quote=FALSE)
dataCsv <- readSeries("~/data3.csv", header=TRUE, sep=",", format="%Y-%m-%d")
dataCsv <- as.xts(dataCsv)


#Vanguard Short-Term Treasury Fund
benchmarkTicker <- "%5EGSPC"
mutualFunds <- c(benchmarkTicker)

#all the period
getSymbols(mutualFunds, from="1997-12-31", to="2015-02-18")
tmp <- list()
for(fund in mutualFunds) {
  tmp[[fund]] <- Ad(get(fund))
}

benchmark <- do.call(cbind, args = tmp)
colnames(benchmark) <- gsub(".Adjusted", "", colnames(benchmark))
alignedBenchmark <- align(x = as.timeSeries(benchmark), by = "1d", method = "before", include.weekends = FALSE)

mergedData <- merge(as.timeSeries(dataCsv),alignedBenchmark)
colnames(mergedData) <- c("Morocco", "Tunisia", "Egypt", "Saudi Arabia", "Lebanon", "Jordan", "Turkey", "S&P500")

insampleStart <- "1998-01-01"
insampleEnd <- "2005-12-30"
outsampleStart <- "2006-01-01"
outsampleEnd <- "2015-02-18"

inSample <- window(mergedData, start = insampleStart, end = insampleEnd)
outSample <- window(as.xts(mergedData), start = outsampleStart, end = outsampleEnd)
monthlyData <- mergedData[endpoints(mergedData, on="days", k=1), ]
returns <- Return.calculate(monthlyData, method = "log")
#corstarsl(returns)

## ---- cortable
returns <- (Return.calculate(mergeData))
correlationTable <- cor(returns[-1,])
stargazer(correlationTable, title = "Correlation coefficients of the daily market returns, 1998-2014")


## ---- summary
summary(mergedData)


## ---- statistics
#stargazer(correlationTable, title = "Correlation Table")
