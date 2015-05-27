## ---- main
library(FinancialInstrument)
library(TTR)
library(PerformanceAnalytics)
library(fPortfolio)
require(xts)
require(timeSeries)
require(stargazer)

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
colnames(mergedData) <- c("Morocco", "Tunisia", "Egypt", "Saudi Arabia", "Lebanon", "Jordan", "Turkey", "SandP500")

#master data file
write.csv(mergedData, "~/master.csv", quote=FALSE)
# timeseriesData <- readSeries("~/master.csv", sep=",")
# finalData <- as.xts(timeseriesData)
