library(ggplot2)
library(plyr)
library(rCharts)
library(googleVis)
library(RJSONIO)


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
  theFilteredSeries = mySeries[mySeries$Exchange !="FINRA" &
                                 !is.na(mySeries$AverageDepth) & mySeries$AverageDepth != 0 &
                                 !is.na(mySeries$Volume & mySeries$Volume != 0),]
  return(theFilteredSeries)
}
readDepthVolumeData <- function(mySymbols){
  for(sym in mySymbols){
    myData <- read.csv(paste(getwd(), "/Data/depth vs volume/", 'depth2014_', sym,".csv", sep =""),
                       header = TRUE, stringsAsFactors = FALSE)
    if(sym == mySymbols[1]){
      mySeries <- myData
    } else{
      mySeries <- rbind(mySeries, myData)
    }
  }
  return(mySeries)
}
enrichDepthVolumeData <- function(aData){
  colnames(aData) <- c("Date", "Symbol", "Exchange", "AverageDepth", "Volume")
  code = c("A", "B", "C", "D", "I", "J", "K", "M", "N", "T", "P",
           "S", "T/Q", "Q", "W", "X", "Y", "Z", "DATE", "X_1", "O")
  exchange = c("NYSE MKT","NASDAQ BX","NSX", "FINRA", "ISE", "BATS EDGA","BATS EDGX","CHX",
               "NYSE","NASDAQ T","NYSE Arca","Consolidated Tape System","NASDAQ TQ", "NASDAQ Q", 
               "CBSX", "NASDAQ PSX", "BATS BYX", "BATS BZX", "DATE", "UNKNOWN_X_1", "UNKNOWN_O")
  aData$Exchange <- mapvalues(aData$Exchange, from = code, to = exchange)
  aData$ExchangeSymbol = paste0(aData$Exchange, ": ", aData$Symbol)
  
  aData$Date <- as.Date(sapply(aData$Date, function(x) toString(x)), format="%Y%m%d")
  return(aData)
}

setwd("C:/Users/tcho/Dropbox/Project - Platform Competition/")
mySymbols = c("AMD", "BAC", "C", "GOOG", "GRPN", "JBLU", "MSFT", "RAD")

mySeries <-readDepthVolumeData(mySymbols)
mySeries <- enrichDepthVolumeData(mySeries)
theSeries <- filterFromDepthVolume(mySeries)


myState <- '
{"yLambda":0,"xLambda":0,"showTrails":false,"playDuration":30000,
"sizeOption":"5", "colorOption":"3",
"iconKeySettings":[{"key":{"dim0":"NYSE: C"}},
{"key":{"dim0":"CHX: JBLU"}},
{"key":{"dim0":"NYSE Arca: BAC"}}]}
'
M <- gvisMotionChart(theSeries, idvar="ExchangeSymbol", timevar="Date",
                     xvar="AverageDepth", yvar="Volume",
                     options=list(width=1200, height=500, state=myState),
                     chartid="DepthVolume")
plot(M)



myAveragedSeries <- aggregate(cbind(depth, volume, dollarDepth, dollarVolume) ~ symbol + exchange, FUN = mean, data=theSeries)
myAveragedSeries$logDepth = log(myAveragedSeries$depth)
myAveragedSeries$logVolume = log(myAveragedSeries$volume)
myAveragedSeries$logDollarDepth = log(myAveragedSeries$dollarDepth)
myAveragedSeries$logDollarVolume = log(myAveragedSeries$dollarVolume)
d6=createScatterDV(myAveragedSeries, "logDepth", "logVolume", 
                  paste(getwd(),'\\Github\\DepthVolume\\output\\scatterLog.html', sep=""))
d8=createScatterDV(myAveragedSeries, "logDollarDepth", "logDollarVolume", 
                   paste(getwd(),'\\Github\\DepthVolume\\output\\scatterDollarLog.html', sep=""))


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