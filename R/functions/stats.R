## ---- main
library(FinancialInstrument)
library(TTR)
library(PerformanceAnalytics)
library(fPortfolio)
library(xts)
library(timeSeries)
library(stargazer)
library(pastecs)


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

#getting the data from he master file
mergedData <- readSeries("~/master.csv", sep=",")

#in and out of sample periods
insampleStart <- "1998-01-01"
insampleEnd <- "2004-12-30"
outsampleStart <- "2005-01-01"
outsampleEnd <- "2015-02-18"


inSample <- window(mergedData, start = insampleStart, end = insampleEnd)
outSample <- window(as.xts(mergedData), start = outsampleStart, end = outsampleEnd)

#monhly returns data 
monthlyData <- mergedData[endpoints(mergedData, on="days", k=1), ]
returns <- Return.calculate(monthlyData, method = "log")


# tmp <- list()
# for ( i in 2:ncol(returns[-1,])){
#     tmp[[i]] <- c(mean(returns[-1,][1,i]), stdev(returns[-1,][1,i]))
# }
# 
# weights <- do.call(rbind, tmp)



#corstarsl(returns)
data <- as.xts(mergedData)
returns <- (Return.calculate(data))

## ---- summary
# stats <- basicStats(returns[-1,])
stats <- stat.desc(returns[-1,], basic = F)
stargazer(as.matrix(stats), title = "Statistics summary, 1998-2014",
          font.size = "tiny",
          column.sep.width = "1pt")
## ---- summary daily
options(scipen=100)
options(digits=2)
stats <- stat.desc(as.xts(mergedData[-1,]), basic = F)
stargazer(as.matrix(stats), title = "Statistics summary, 1998-2014",
          font.size = "tiny",
          column.sep.width = "1pt")

## ---- cortable
correlationTable <- cor(returns[-1,])
# correlationTable <- corstarsl(returns)
stargazer(correlationTable, title = "Correlation coefficients of the daily market returns, 1998-2014",
          font.size = "tiny",
          column.sep.width = "1pt")







## ---- statistics
stats1 <- as.matrix(table.Stats(returns[-1,]))
stargazer(stats1[c(10:16),], title = "Statistics summary, 1998-2014",
          font.size = "tiny",
          column.sep.width = "1pt")






