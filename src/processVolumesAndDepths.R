library(ggplot2)
library(rCharts)
mapSymbolToColor <- function(aSymbol){
  mySymbols = c("AMD", "BAC", "C", "GOOG", "GRPN", "JBLU", "MSFT", "RAD")
  myColors = c("red", "orange", "yellow", "green", "blue", "purple", "black", "brown")
  myIndex = mySymbols == aSymbol
  return(myColors[myIndex])
}
createScatterDV <- function(aData, aX, aY, aFile){
  d1 <- dPlot(x=aX, y=aY,
              groups = c("startTime", "endTime", "exchange", "symbol"),
              data = aData, type = "bubble",
              height=800, width=1400)
  d1$xAxis(type = "addMeasureAxis" )
  d1$yAxis(type = "addMeasureAxis" )
  d1$legend(x = 200, y = 10, width = 500, height = 20,
            horizontalAlign = "right")
  d1
  d1$save(aFile, standalone = TRUE)
  return(d1)
}
filterFromDepthVolume <- function(mySeries){
  theFilteredSeries = mySeries[mySeries$exchange !="FINRA" & !is.nan(mySeries$depth) &
                                mySeries$depth != 0,]
  return(theFilteredSeries)
}
deriveDepthVolumeColumns <- function(myFilteredSeries){
  myFilteredSeries$logDepth = log(myFilteredSeries$depth)
  myFilteredSeries$logVolume = log(myFilteredSeries$volume)
  myFilteredSeries$dollarDepth = myFilteredSeries$depth * myFilteredSeries$bookPrice
  myFilteredSeries$dollarVolume = myFilteredSeries$volume * myFilteredSeries$tradePrice
  myFilteredSeries$logDollarDepth = log(myFilteredSeries$dollarDepth)
  myFilteredSeries$logDollarVolume = log(myFilteredSeries$dollarVolume)
  return(myFilteredSeries)
}

readDepthVolumeData <- function(mySymbols, aNum){
  for(sym in mySymbols){
    myData <- read.csv(paste(getwd(), "\\Github\\DepthVolume\\output\\", sym, "tqd",aNum,".csv", sep =""),
                       header = TRUE, stringsAsFactors = FALSE)
    if(sym == mySymbols[1]){
      mySeries <- myData
    } else{
      mySeries <- rbind(mySeries, myData)
    }
  }
  return(mySeries)
}
setwd("C:/Users/tcho/Documents/")
mySymbols = c("AMD", "BAC", "C", "GOOG", "GRPN", "JBLU", "MSFT", "RAD")
num = 6
mySeries <-readDepthVolumeData(mySymbols, num)

myFilteredSeries <- filterFromDepthVolume(mySeries)
theSeries <- deriveDepthVolumeColumns(myFilteredSeries)

d1=createScatterDV(theSeries, "depth", "volume",
                   paste(getwd(),'\\Github\\DepthVolume\\output\\perDayScatter.html', sep=""))
d2=createScatterDV(theSeries, "logDepth", "logVolume", 
                   paste(getwd(),'\\Github\\DepthVolume\\output\\perDayScatterLog.html', sep=""))
d3=createScatterDV(theSeries, "dollarDepth", "dollarVolume", 
                   paste(getwd(),'\\Github\\DepthVolume\\output\\perDayScatterDollar.html', sep=""))
d4=createScatterDV(theSeries, "logDollarDepth", "logDollarVolume", 
                   paste(getwd(),'\\Github\\DepthVolume\\output\\perDayScatterDollarLog.html', sep=""))

myAveragedSeries <- aggregate(cbind(depth, volume, dollarDepth, dollarVolume) ~ symbol + exchange, FUN = mean, data=theSeries)
myAveragedSeries$logDepth = log(myAveragedSeries$depth)
myAveragedSeries$logVolume = log(myAveragedSeries$volume)
myAveragedSeries$logDollarDepth = log(myAveragedSeries$dollarDepth)
myAveragedSeries$logDollarVolume = log(myAveragedSeries$dollarVolume)
d5=createScatterDV(myAveragedSeries, "depth", "volume",
                   paste(getwd(),'\\Github\\DepthVolume\\output\\scatter.html', sep=""))
d6=createScatterDV(myAveragedSeries, "logDepth", "logVolume", 
                  paste(getwd(),'\\Github\\DepthVolume\\output\\scatterLog.html', sep=""))
d7=createScatterDV(myAveragedSeries, "dollarDepth", "dollarVolume", 
                   paste(getwd(),'\\Github\\DepthVolume\\output\\scatterDollar.html', sep=""))
d8=createScatterDV(myAveragedSeries, "logDollarDepth", "logDollarVolume", 
                   paste(getwd(),'\\Github\\DepthVolume\\output\\scatterDollarLog.html', sep=""))

d8=createScatterDV(myAveragedSeries, "dollarDepth", "logDollarVolume", 
                   paste(getwd(),'\\Github\\DepthVolume\\output\\scatterDollarNormDepthLogVolume.html', sep=""))
d8=createScatterDV(myAveragedSeries, "logDollarDepth", "dollarVolume", 
                   paste(getwd(),'\\Github\\DepthVolume\\output\\scatterDollarLogDepthNormVolume.html', sep=""))


if(FALSE){
  mySeries$color = sapply(mySeries$symbol, function(x) mapSymbolToColor(x))
  plot(myFilteredSeries$depth, myFilteredSeries$volume, col=myFilteredSeries$color)
  plot(log(myFilteredSeries$depth), log(myFilteredSeries$volume), col=myFilteredSeries$color)
  legend("topleft", legend=levels(factor(myFilteredSeries$symbol)),
         text.col=seq_along(levels(factor(myFilteredSeries$color))))
  
  sym="AMD"
  ptm=proc.time()
  mySeries <- read.csv(paste(getwd(), "\\Github\\DepthVolume\\data\\", sym, "_mar_q.csv", sep =""),
                       header = TRUE, stringsAsFactors = FALSE)
  myCols = c("DATE", "TIME_M", "EX", "SYM_ROOT", "BIDSIZ", "ASKSIZ")
  mySeries = mySeries[,myCols]
  
  
  d3$save(paste(getwd(),'\\Github\\DepthVolume\\output\\test.html', sep=""), 
                standalone = TRUE)
  
} 