library(ggplot2)
library(plyr)
library(rCharts)
library(googleVis)
library(reshape)
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
    myData <- read.csv(paste(getwd(), "/Data/depth vs volume/msec/", 'depth_dollar2014_', sym,".csv", sep =""),
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
  exchange = c("NYSE.MKT","NASDAQ.BX","NSX", "FINRA", "ISE", "BATS.EDGA","BATS.EDGX","CHX",
               "NYSE","NASDAQ.T","NYSE.Arca","Consolidated.Tape.System","NASDAQ.TQ", "NASDAQ.Q", 
               "CBSX", "NASDAQ.PSX", "BATS.BYX", "BATS.BZX", "DATE", "UNKNOWN_X_1", "UNKNOWN_O")
  aData$Exchange <- mapvalues(aData$Exchange, from = code, to = exchange)
  aData$ExchangeSymbol = paste0(aData$Exchange, ": ", aData$Symbol)
  
  aData$Date <- as.Date(sapply(aData$Date, function(x) toString(x)), format="%Y%m%d")
  return(aData)
}
retrieveGraphInputData <- function(aSymbols){
  mySeries <-readDepthVolumeData(aSymbols)
  head(mySeries)
  mySeries <- enrichDepthVolumeData(mySeries)
  theSeries <- filterFromDepthVolume(mySeries)
}
averageOverSymbolExchange <- function(mySeries){
  myAveragedSeries <- aggregate(cbind(depth, volume, dollarDepth, dollarVolume) ~ symbol + exchange, FUN = mean, data=theSeries)
  myAveragedSeries$logDepth = log(myAveragedSeries$depth)
  myAveragedSeries$logVolume = log(myAveragedSeries$volume)
  myAveragedSeries$logDollarDepth = log(myAveragedSeries$dollarDepth)
  myAveragedSeries$logDollarVolume = log(myAveragedSeries$dollarVolume)
  d6=createScatterDV(myAveragedSeries, "logDepth", "logVolume", 
                     paste(getwd(),'\\Github\\DepthVolume\\output\\scatterLog.html', sep=""))
  d8=createScatterDV(myAveragedSeries, "logDollarDepth", "logDollarVolume", 
                     paste(getwd(),'\\Github\\DepthVolume\\output\\scatterDollarLog.html', sep=""))
  
}
createDancingBubblePlot <- function(aSeries){
  myState <- '
  {"yLambda":0,"xLambda":0,"showTrails":false,"playDuration":30000,
  "sizeOption":"5", "colorOption":"3",
  "iconKeySettings":[{"key":{"dim0":"NYSE: C"}},
  {"key":{"dim0":"CHX: JBLU"}},
  {"key":{"dim0":"NYSE Arca: BAC"}}]}
  '
  M <- gvisMotionChart(aSeries, idvar="ExchangeSymbol", timevar="Date",
                       xvar="AverageDepth", yvar="Volume",
                       options=list(width=1200, height=500, state=myState),
                       chartid="DollarDepthVolume")
  plot(M)
  myFileName = paste(getwd(), '/Code/DepthVolume/output/', 'timeChartDollar.html', sep="")
  print(M, file=myFileName)
  print(myFileName)
}

getCoefficientsByGroup <- function(theSeries, aGrouping){
  if(aGrouping != "Exchange"){
    myUltraFilteredSeries <- 
      theSeries[which(!(theSeries$Exchange %in% c("NSX", "CHX", "CBSX", "NASDAQ.PSX"))),]
  } else{
    myUltraFilteredSeries = theSeries
  }
  myGroups <- unique(myUltraFilteredSeries[,aGrouping])
  myCoefficientDF <- read.table(text = "", col.names = c("Date", myGroups))
  myDates <- unique(myUltraFilteredSeries$Date)
  for(i in 1:length(myDates)){
    myDate = myDates[i]
    myCoefficientDF[i, "Date"] = as.Date(myDate)
    myDateData <- myUltraFilteredSeries[which(myUltraFilteredSeries$Date == myDate),]
    myDateGroups = unique(myDateData[,aGrouping])
    for(g in myDateGroups){
      myDateGroupData <- myDateData[which(myDateData[,aGrouping] == g),]
      myFit <- lm(log(Volume) ~ log(AverageDepth), data = myDateGroupData)
      mySlope = summary(myFit)$coefficients[2]
      myCoefficientDF[i,g] = mySlope
    }
  }
  myCoefficientDF$Date <- as.Date(myCoefficientDF$Date, origin="1970-01-01")
  return(myCoefficientDF)
}

getCoefficients <- function(theSeries, aIsFilter=FALSE){
  if(aIsFilter){
    myUltraFilteredSeries <- 
      theSeries[which(!(theSeries$Exchange %in% c("NSX", "CHX", "CBSX", "NASDAQ.PSX"))),]
  } else{
    myUltraFilteredSeries = theSeries
  }
  myCoefficientDF <- read.table(text = "", col.names = c("Date", "Slope"))
  myDates <- unique(myUltraFilteredSeries$Date)
  for(i in 1:length(myDates)){
    myDate = myDates[i]
    myCoefficientDF[i, "Date"] = as.Date(myDate)
    myDateData <- myUltraFilteredSeries[which(myUltraFilteredSeries$Date == myDate),]
    
    myFit <- lm(log(Volume) ~ log(AverageDepth), data = myDateData)
    mySlope = summary(myFit)$coefficients[2]
    myCoefficientDF[i,"Slope"] = mySlope
  }
  myCoefficientDF$Date <- as.Date(myCoefficientDF$Date, origin="1970-01-01")
  return(myCoefficientDF)
}

saveGraphsForCoeffs <- function(myCoefficientDF, myGrouping, aSuffix=""){
  d <- melt(myCoefficientDF, id.vars = "Date", variable.name = 'series')
  
  ggplot(d, aes(x = value)) + 
    facet_wrap(~variable, ncol=2, scales="free") + 
    geom_histogram() +
    geom_vline(aes(xintercept=1, colour = "blue"))
  ggsave(file = paste(getwd(), "/Code/DepthVolume/output/trends/", myGrouping, "CoeffsHist", aSuffix,
                      ".png", sep=""),
         height = 10)
  
  ggplot(d, aes(Date, value)) + 
    facet_wrap(~variable, ncol=2, scales="free") +
    geom_line(aes(group=variable)) +
    geom_hline(aes(yintercept=1, colour = "blue")) +
    scale_x_date(labels = date_format("%m-%Y"))
  ggsave(file = paste(getwd(), "/Code/DepthVolume/output/trends/", myGrouping, "CoeffsTS", aSuffix,
                      ".png", sep=""),
         height = 10)
  
}
setwd("C:/Users/tcho/Dropbox/Project - Platform Competition/")
mySymbols = c("AMD", "BAC", "C", "GOOG", "GRPN", "JBLU", "MSFT", "RAD")
theSeries <- retrieveGraphInputData(mySymbols)
createDancingBubblePlot(theSeries)

myGrouping = "Exchange"
myCoefficientDF <- getCoefficientsByGroup(theSeries, myGrouping)
myCoefficientDF[mapply(is.infinite, myCoefficientDF)] <- NA
saveGraphsForCoeffs(myCoefficientDF, myGrouping)

myGrouping = "Symbol"
myCoefficientDF <- getCoefficientsByGroup(theSeries, myGrouping)
myCoefficientDF[mapply(is.infinite, myCoefficientDF)] <- NA
saveGraphsForCoeffs(myCoefficientDF, myGrouping, "WithoutRinkyDink")


myCoefficientDF <- read.table(text = "", col.names = c("Date", "Slope"))
myDates <- unique(myUltraFilteredSeries$Date)
for(i in 1:length(myDates)){
  myDate = myDates[i]
  myCoefficientDF[i, "Date"] = as.Date(myDate)
  myDateData <- myUltraFilteredSeries[which(myUltraFilteredSeries$Date == myDate),]
  myDateGroups = unique(myDateData[,aGrouping])
  for(g in myDateGroups){
    myDateGroupData <- myDateData[which(myDateData[,aGrouping] == g),]
    myFit <- lm(log(Volume) ~ log(AverageDepth), data = myDateGroupData)
    mySlope = summary(myFit)$coefficients[2]
    myCoefficientDF[i,g] = mySlope
  }
}
myCoefficientDF$Date <- as.Date(myCoefficientDF$Date, origin="1970-01-01")
