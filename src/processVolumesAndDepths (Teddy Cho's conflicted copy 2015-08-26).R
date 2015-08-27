library(ggplot2)
library(plyr)
library(rCharts)
library(googleVis)
library(reshape)
library(RJSONIO)

readDepthVolumeData <- function(mySymbols){
  for(sym in mySymbols){
    myData <- read.csv(paste(getwd(), "/Data/depth vs volume/sec/", 'depth2014_sec_', sym,".csv", sep =""),
                       header = TRUE, stringsAsFactors = FALSE)
    if(sym == mySymbols[1]){
      mySeries <- myData
    } else{
      mySeries <- rbind(mySeries, myData)
    }
  }
  return(mySeries)
}
enrichDepthVolumeData <- function(aData, myDepthStyle){
  originalCol <- c("DATE", "SYMBOL", "EX", "depth_s", "depth_d", "depth_s_min", "depth_d_min", "volume_s",
                   "volume_d")
  newCol <- c("Date", "Symbol", "Exchange", "AverageDepth_Shares_Avg", "AverageDepth_Dollars_Avg", 
              "AverageDepth_Shares_Min", "AverageDepth_Dollars_Min", "Volume_Shares", "Volume_Dollars")
  colnames(aData) <- mapvalues(colnames(aData), from = originalCol, to = newCol)
  theSeries$AverageDepth_Dollars <- theSeries[,paste("AverageDepth_Dollars_", myDepthStyle, sep="")]
  theSeries$AverageDepth_Shares <- theSeries[,paste("AverageDepth_Share_", myDepthStyle, sep="")]
  
  code = c("A", "B", "C", "D", "I", "J", "K", "M", "N", "T", "P",
           "S", "T/Q", "Q", "W", "X", "Y", "Z", "DATE", "X_1", "O")
  exchange = c("NYSE.MKT","NASDAQ.BX","NSX", "FINRA", "ISE", "BATS.EDGA","BATS.EDGX","CHX",
               "NYSE","NASDAQ.T","NYSE.Arca","Consolidated.Tape.System","NASDAQ.TQ", "NASDAQ.Q", 
               "CBSX", "NASDAQ.PSX", "BATS.BYX", "BATS.BZX", "DATE", "UNKNOWN_X_1", "UNKNOWN_O")
  aData$Exchange <- mapvalues(aData$Exchange, from = code, to = exchange)

  aData$ExchangeSymbol = paste0(aData$Exchange, ": ", aData$Symbol)
  
  aData$Date <- as.Date(sapply(aData$Date, function(x) toString(x)), format="%Y%m%d")
  
  #REMOVE LATER
  aData <- aData[aData$Exchange != ".",]
  myDates <- unique(aData$Date)
  #aData$DayIndex <- sapply(aData$Date, function(x) which(myDates==x)+100)
  
  return(aData)
}
filterFromDepthVolume <- function(mySeries){
  #theFilteredSeries = mySeries[mySeries$Exchange !="FINRA" &
  #                               !is.na(mySeries$AverageDepth) & mySeries$AverageDepth != 0 &
  #                               !is.na(mySeries$Volume) & mySeries$Volume != 0,]
  theFilteredSeries <- mySeries[!(mySeries$Exchange %in% c("FINRA", "UNKNOWN_O", ".")),]
  #theFilteredSeries$Volume_Shares[is.na(theFilteredSeries$Volume_Shares)] = 0
  
  return(theFilteredSeries)
}
retrieveGraphInputData <- function(aSymbols, myDepthStyle){
  mySeries <-readDepthVolumeData(aSymbols)
  mySeries <- enrichDepthVolumeData(mySeries, myDepthStyle)
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
createDancingBubblePlot <- function(aSeries, aSuffix=''){
  myState='
  {"yLambda":0,"xLambda":0,
  "sizeOption":"5", "colorOption":"2","dimensions":{"iconDimensions":["dim0"]},
  "iconKeySettings":[]}
  '
  aSeries<-aSeries[,-which(colnames(aSeries) %in% c("Volume_Shares", "AverageDepth_Shares", "Date"))]
  aSeries<-aSeries[c("DayIndex", setdiff(names(aSeries), "DayIndex"))]
  aSeries<-aSeries[!is.na(aSeries$Volume_Dollars),]
  M <- gvisMotionChart(aSeries, idvar="ExchangeSymbol", timevar="DayIndex",
                       xvar="AverageDepth_Dollars", yvar="Volume_Dollars",
                       options=list(width=1200, height=500, state=myState),
                       chartid="DollarDepthVolume")
  plot(M)
  myFileName = paste(getwd(), '/Code/DepthVolume/output/', 'timeChartDollar', aSuffix, '.html', sep="")
  print(M, file=myFileName)
  print(myFileName)
}

getCoefficientsByGroup <- function(theSeries, aGrouping, aIsFilter){
  myGroups <- unique(theSeries[,aGrouping])
  myCoefficientDF <- read.table(text = "", col.names = c("Date", myGroups))
  myDates <- unique(theSeries$Date)
  for(i in 1:length(myDates)){
    myDate = myDates[i]
    myCoefficientDF[i, "Date"] = as.Date(myDate)
    myDateData <- theSeries[which(theSeries$Date == myDate),]
    myDateGroups = unique(myDateData[,aGrouping])
    for(g in myDateGroups){
      myDateGroupData <- myDateData[which(myDateData[,aGrouping] == g),]
      myFit <- lm(log(Volume_Dollars) ~ log(AverageDepth_Dollars), data = myDateGroupData)
      mySlope = summary(myFit)$coefficients[2]
      myCoefficientDF[i,g] = mySlope
    }
  }
  myCoefficientDF$Date <- as.Date(myCoefficientDF$Date, origin="1970-01-01")
  return(myCoefficientDF)
}
getCoefficients <- function(theSeries, aIsFilter=FALSE){
  myCoefficientDF <- read.table(text = "", col.names = c("Date", "Slope"))
  myDates <- unique(theSeries$Date)
  for(i in 1:length(myDates)){
    myDate = myDates[i]
    myCoefficientDF[i, "Date"] = as.Date(myDate)
    myDateData <- theSeries[which(theSeries$Date == myDate),]
    
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
    facet_wrap(~variable, ncol=1, scales="free") + 
    geom_histogram() +
    geom_vline(aes(xintercept=1, colour = "blue"))
  ggsave(file = paste(getwd(), "/Code/DepthVolume/output/trends/", myGrouping, "CoeffsHist", aSuffix,
                      ".png", sep=""),
         height = 10)
  ggplot(d, aes(Date, value)) + 
    facet_wrap(~variable, ncol=1, scales="free") +
    geom_line(aes(group=variable)) +
    geom_hline(aes(yintercept=1, colour = "blue")) +
    scale_x_date(labels = date_format("%m-%Y"))
  ggsave(file = paste(getwd(), "/Code/DepthVolume/output/trends/", myGrouping, "CoeffsTS", aSuffix,
                      ".png", sep=""),
         height = 10)
}
createPlotAnim <- function(theSeries, myOutputFolder, myDepthLim, myVolumeLim, myDepthStyle){
  mySymbols = unique(theSeries$Symbol)
  myDates <- unique(theSeries$Date)
  for(sym in mySymbols){
    mySymSeries <- theSeries[which(theSeries$Symbol == sym),]
    for(d in myDates){
      myCurrDate <- as.Date(d, origin="1970-01-01")
      myDateSeries <- mySymSeries[which(mySymSeries$Date == myCurrDate),]
      plotDateSeries(myDateSeries, myCurrDate, myOutputFolder, sym, myDepthLim, myVolumeLim, myDepthStyle)
    }
    myConvertPath = '"C:\\Program Files\\ImageMagick-6.9.1-Q16\\convert.exe"'
    myPNGPath = paste(myOutputFolder, "*.png", sep="")
    setwd(paste(myOutputFolder, sep=""))
    my_command <- paste(myConvertPath, " *.png -delay 3 -loop 0 ", "DepthVolume_", sym, "_", myDepthStyle, ".gif", sep="")
    system(my_command)
    unlink('*.png')
  }
}
plotDateSeries <- function(myDateSeries, aDate, myOutputFolder, aSymbol, myDepthLim, myVolumeLim, myDepthStyle){
  myDateSeries[myDateSeries==0]<-NA ########### WHATT OR JUST EXCLudE?!?!?
  myFit <- lm(log10(Volume_Dollars) ~ log10(AverageDepth_Dollars), data = myDateSeries)
  myFitIntercept = summary(myFit)$coefficients[1]
  myFitSlope = summary(myFit)$coefficients[2]
  ###COLLECT SHIT HERE
  
  ggplot(data=myDateSeries, aes(x=AverageDepth_Dollars, y=Volume_Dollars, color=Exchange)) +
    geom_point(alpha=.5, size=3) + 
    scale_x_log10(limits = c(.1, myDepthLim)) + scale_y_log10(limits = c(.1, myVolumeLim)) +
    geom_abline(intercept = 4.2, slope = 1, colour = "green", alpha=.5) +
    geom_abline(intercept = myFitIntercept, slope = myFitSlope, colour = "red", alpha=.3) +
    ggtitle(paste(aSymbol, ": Volume v. Depth (", myDepthStyle, " Method)\n", toString(aDate), sep="")) + theme(plot.title = element_text(size=20, face="bold", vjust=2)) +
    theme(legend.position = "none") +
    geom_text(aes(label=Exchange), size=4, hjust=1.1, vjust=0)
  ggsave(file=paste(myOutputFolder, toString(aDate), '.png', sep=""), width = 12, height=10)
}

setwd("C:/Users/tcho/Dropbox/Project - Platform Competition/")
mySymbols = c("BAC", "C", "GOOG", "GRPN", "JBLU", "MSFT", "RAD")
mySymbols = c("BAC", "GRPN", "MSFT")
mySymbols = c("BAC")
myOutputFolder <- paste(getwd(), '/Code/DepthVolume/output/frames/', sep="")
myDepthStyle <- "Avg"

theSeries <- retrieveGraphInputData(mySymbols, myDepthStyle)
theSeries <- theSeries[theSeries$Date < as.Date("2014-01-31", "%Y-%m-%d"),]
myDepthLim <- 1.1 * max(na.omit(theSeries$AverageDepth_Dollars))
myVolumeLim <- 1.1 * max(na.omit(theSeries$Volume_Dollars))
createPlotAnim(theSeries, myOutputFolder, myDepthLim, myVolumeLim, myDepthStyle)


createDancingBubblePlot(theSeries, "_BAC_GOOG_min")

myFilteredSeries <- 
  theSeries[which(!(theSeries$Exchange %in% c("NSX", "CHX", "CBSX", "NASDAQ.PSX"))),]
myFilteredSeries <- theSeries
myFilteredSeries[is.na(myFilteredSeries)] <-0



myGrouping = "Symbol"
myCoefficientDF <- getCoefficientsByGroup(myFilteredSeries, myGrouping)
myCoefficientDF[mapply(is.infinite, myCoefficientDF)] <- NA
saveGraphsForCoeffs(myCoefficientDF, myGrouping, "_BAC_GOOG")


myGrouping = "Exchange"
myCoefficientDF <- getCoefficientsByGroup(theSeries, myGrouping)
myCoefficientDF[mapply(is.infinite, myCoefficientDF)] <- NA
saveGraphsForCoeffs(myCoefficientDF, myGrouping)




myCoefficientDF <- getCoefficients(theSeries, TRUE)
myCoefficientDF[mapply(is.infinite, myCoefficientDF)] <- NA
saveGraphsForCoeffs(myCoefficientDF, "All", "WithoutRinkyDink")
myCoefficientDF <- getCoefficients(theSeries, FALSE)
myCoefficientDF[mapply(is.infinite, myCoefficientDF)] <- NA
saveGraphsForCoeffs(myCoefficientDF, "All", "")

mySeries->theSeries
mySeries<-mySeries[mySeries$EX != '.',]

'
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
}'