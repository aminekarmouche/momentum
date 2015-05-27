require(quantmod)
require(PerformanceAnalytics)   #get GSPC or S&P 500
#feel free to change to whatever you would like
#for non index do not include ^
getSymbols("^GSPC",from="1896-01-01",to=Sys.Date())   #get monthly close
sp500 <- to.monthly(GSPC)[,4]
#do this to get from mmm yyyy to yyyy-mm-dd
index(sp500) <- as.Date(index(sp500))
#get monthly returns from the monthly closes
#multiple ways of doing this
sp500.ret <- monthlyReturn(sp500)   #get 10 month Mebane Faber moving average
ma <- runMean(sp500,n=10)
#if close > 10 month moving average then 1 and 0 if <
signal <- ifelse(sp500 > ma,1,0)   #merge originial return data with this new data
#multiply the 1-month lagged signal by return
#if signal is 0 then return is 0 indicating out
returnComp <- merge(sp500.ret,lag(signal,k=1)*sp500.ret)
returnComp[is.na(returnComp[,2]),2] <- 0
colnames(returnComp) <- c("SP500","SP500.Faber")   #jpeg(filename="performance summary.jpg",
quality=100,width=6, height = 7.5,  units="in",res=96)
charts.PerformanceSummary(returnComp, ylog=TRUE,
                          colorset=c("lightcyan4","lightgoldenrod3"),
                          main="S&P 500 and Mebane Faber Moving Average System
                          Monthly Performance Since 1950")
#dev.off()   #saved this require for later
#since fPortfolio and f anything does not play well
#with PerformanceAnalytics
require(fPortfolio)   #get frontier for the combination
#of the original price and the Faber mov avg system
frontier <- portfolioFrontier(as.timeSeries(returnComp))   #most of this comes directly from the fPortfolio demo
#very slight changes have been made
#we will run for the entire period
#jpeg(filename="frontier plot.jpg",
#	quality=100,width=6, height = 6,  units="in",res=96)
#unfortunately title cannot be changed easily
frontierPlot(frontier, pch=19, risk = "CVaR")
minvariancePoints(frontier,pch=19,col="red")
tangencyPoints(frontier,pch=19,col="blue")
tangencyLines(frontier,pch=19,col="blue")
equalWeightsPoints(frontier,pch=15,col="grey")
singleAssetPoints(frontier,pch=19,cex=1.5,col=c("lightcyan4","lightgoldenrod3"))
twoAssetsLines(frontier,lty=3,col="grey")
legend("topleft",legend=colnames(returnComp),pch=19,col=c("lightcyan4","lightgoldenrod3"))
#sharpeRatioLines(frontier,col="orange",lwd=2)
#dev.off()   #now let's see what this looks like on a rolling basis
#this from and to is not well documented in the fPortfolio
#documentation so I hope it helps some people
#will use 48 month rolling window and redo every 6 months
#window needs to be large since Faber system will have lots of
#0 returns, which can be handled by adding t-bill returns while out
from <- rollingWindows(as.timeSeries(returnComp),period="48m",by="6m")$from
to <- rollingWindows(as.timeSeries(returnComp),period="48m",by="6m")$to   Spec = portfolioSpec()
setTargetReturn(Spec) = mean(colMeans(as.timeSeries(returnComp)))
Constraints = "LongOnly"
#using Tangency but can also do Cml with rollingCmlPortfolio
#or rollingMinvariancePortfolio
rollTan <- rollingTangencyPortfolio(as.timeSeries(returnComp),Spec,Constraints,
                                    from=from,to=to)
#sapply works very nicely with the lists used in fPortfolio
#get weights from each of the rolling periods
tanweights <- sapply(rollTan,getWeights)
rownames(tanweights) <- colnames(returnComp)   #jpeg(filename="rollling weight plot.jpg",
quality=100,width=6, height = 6,  units="in",res=96)
barplot(tanweights,col=c("lightcyan4","lightgoldenrod4"),
        legend.text=TRUE,names.arg=format(from,"%b %y"),
        cex.names=0.7)
#dev.off()   #do not know a slick way to do this
#repeat the weights 6 times for the 6 month by
for (i in 1:NROW(t(tanweights))) {
  for (j in 1:6) {
    if (i==1 & j==1) {
      tanweights.xts <- data.frame(t(tanweights[,i]))
    } else {
      #check to make sure we do not exceed number
      #of rows in original return series - 48
      #for the initialization
      if (NROW(tanweights.xts) <= NROW(returnComp)-48)
        tanweights.xts <- rbind(tanweights.xts,
                                data.frame(t(tanweights[,i])))
    }
  }
}
tanweights.xts <- xts(tanweights.xts,
                      order.by=index(returnComp)[48:NROW(returnComp)])   tanreturns <- lag(tanweights.xts,k=1)*returnComp[49:NROW(returnComp),]
returnComp2 <- merge(returnComp,tanreturns[,1]+tanreturns[,2])
colnames(returnComp2) <- c(colnames(returnComp)[1:2],"Cml")
#jpeg(filename="risk return.jpg",
quality=100,width=6, height = 6,  units="in",res=96)
chart.RiskReturnScatter(returnComp2)
#dev.off()
#since fPortfolio gives error on charts.PerformanceSummary
#assemble quick one-pager
#jpeg(filename="perf all.jpg",
#	quality=100,width=6, height = 7,  units="in",res=96)
layout(matrix(c(1, 2)), height = c(2.5,1.5), width = 1)
par(mar = c(1, 4, 4, 2))
chart.CumReturns(returnComp2,xaxis=FALSE,ylab="Cumulative Return",
                 colorset = c("lightcyan4","lightgoldenrod4","darkolivegreen3"),
                 main="SP500 with Faber MA and CML Combo",legend.loc="topleft")
par(mar = c(5, 4, 0, 2))
chart.Drawdown(returnComp2,main="",ylab="Drawdown",
               colorset=c("lightcyan4","lightgoldenrod4","darkolivegreen3"))
#dev.off()