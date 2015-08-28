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
enrichDepthVolumeData <- function(aData){
  originalCol <- c("DATE", "SYMBOL", "EX", "depth_s", "depth_d", "depth_s_min", "depth_d_min", "volume_s",
                   "volume_d")
  newCol <- c("Date", "Symbol", "Exchange", "AverageDepth_Shares_Avg", "AverageDepth_Dollars_Avg", 
              "AverageDepth_Shares_Min", "AverageDepth_Dollars_Min", "Volume_Shares", "Volume_Dollars")
  colnames(aData) <- mapvalues(colnames(aData), from = originalCol, to = newCol)
  
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
retrieveGraphInputData <- function(aSymbols){
  mySeries <-readDepthVolumeData(aSymbols)
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
createPlotAnim <- function(theSeries, myOutputFolder, myDepthStyle){
  theSeries$AverageDepth_Dollars <- theSeries[, paste("AverageDepth_Dollars_", myDepthStyle, sep="")]
  theSeries$AverageDepth_Shares <- theSeries[, paste("AverageDepth_Shares_", myDepthStyle, sep="")]
  
  myDepthLim <- 1.1 * max(na.omit(theSeries$AverageDepth_Dollars))
  myVolumeLim <- 1.1 * max(na.omit(theSeries$Volume_Dollars))
  
  mySymbols = unique(theSeries$Symbol)
  myDates <- unique(theSeries$Date)
  for(sym in mySymbols){
    mySymSeries <- theSeries[which(theSeries$Symbol == sym),]
    for(i in 1:length(myDates)){
      myCurrDate <- myDates[i]
      myDateSeries <- mySymSeries[which(mySymSeries$Date == myCurrDate),]
      plotDateSeries(myDateSeries, myCurrDate, myOutputFolder, sym, myDepthLim, myVolumeLim, myDepthStyle, i)
    }
    #myConvertPath = '"C:\\Program Files\\ImageMagick-6.9.1-Q16\\convert.exe"'
    #setwd(paste(myOutputFolder, sep=""))
    #my_command <- paste(myConvertPath, " *.png -delay 3 -loop 0 ", "DepthVolume_", sym, "_", myDepthStyle, ".gif", sep="")
    #system(my_command)
    #unlink('*.png')
  }
}
createCoeffPlot <- function(theSeries, myOutputFolder){
  mySymbols = unique(theSeries$Symbol)
  myDates <- unique(theSeries$Date)
  myCoeffDF <- data.frame()
  for(sym in mySymbols){
    mySymSeries <- theSeries[which(theSeries$Symbol == sym),]
    for(i in 1:length(myDates)){
      myCurrDate <- myDates[i]
      myDateSeries <- mySymSeries[which(mySymSeries$Date == myCurrDate),]
      myDateSeries[myDateSeries==0]<-NA ########### WHATT OR JUST EXCLudE?!?!?
      if(any(!is.na(myDateSeries$AverageDepth_Dollars_Avg))){
        myFit_Avg <- lm(log10(Volume_Dollars) ~ log10(AverageDepth_Dollars_Avg), data = myDateSeries)
        myFit_Min <- lm(log10(Volume_Dollars) ~ log10(AverageDepth_Dollars_Min), data = myDateSeries)
        myCoeff_Avg <- data.frame(Date=myCurrDate, symbol=sym, depthMethod="Avg", 
                                 Beta=summary(myFit_Avg)$coefficients[2]) 
        myCoeff_Min <- data.frame(Date=myCurrDate, symbol=sym, depthMethod="Min", 
                                  Beta=summary(myFit_Min)$coefficients[2]) 
        
        myCoeffDF <- rbind(myCoeffDF, myCoeff_Avg, myCoeff_Min)
      }
    }
  }
  
  ggplot(data=myCoeffDF, aes(x=Date, y=Beta, colour=depthMethod)) +
    geom_line(alpha=.5) +
    geom_hline(aes(yintercept=1), colour = "green") +
    facet_wrap(~symbol, ncol=2, scales="free")
  ggsave(file=paste(myOutputFolder, "BetaLines", '.png', sep=""), width = 12, height=10)
  ggplot(myCoeffDF, aes(x=Beta, fill=depthMethod)) + 
    geom_histogram(alpha=0.2, position="identity", binwidth=.05) +
    geom_vline(aes(xintercept=1), colour = "green") +
    facet_wrap(~symbol, ncol=2, scales="free")
  ggsave(file=paste(myOutputFolder, "BetaHistograms", '.png', sep=""), width = 12, height=10)
}
plotDateSeries <- function(myDateSeries, aDate, myOutputFolder, aSymbol, myDepthLim, myVolumeLim, myDepthStyle, i){
  myDateSeries[myDateSeries==0]<-NA ########### WHATT OR JUST EXCLudE?!?!?
  if(any(!is.na(myDateSeries$AverageDepth_Dollars))){
    myFit <- lm(log10(Volume_Dollars) ~ log10(AverageDepth_Dollars), data = myDateSeries)
    myFitIntercept = summary(myFit)$coefficients[1]
    myFitSlope = summary(myFit)$coefficients[2]
    ###COLLECT SHIT HERE
  
    ggplot(data=myDateSeries, aes(x=AverageDepth_Dollars, y=Volume_Dollars, color=Exchange)) +
      geom_point(alpha=.5, size=3) + 
      scale_x_log10(limits = c(.1, myDepthLim)) + scale_y_log10(limits = c(.1, myVolumeLim)) +
      geom_abline(intercept = 0, slope = 1, colour = "green", alpha=.5) +
      geom_abline(intercept = myFitIntercept, slope = myFitSlope, colour = "red", alpha=.3) +
      ggtitle(paste(aSymbol, ": Volume v. Depth (", myDepthStyle, " Method)\n", toString(aDate), sep="")) + theme(plot.title = element_text(size=20, face="bold", vjust=2)) +
      theme(legend.position = "none") +
      geom_text(aes(label=Exchange), size=4, hjust=1.1, vjust=0)
    ggsave(file=paste(myOutputFolder, aSymbol, '/', i-1, '.png', sep=""), width = 12, height=10, dpi=100)
  }
}

setwd("C:/Users/tcho/Dropbox/Project - Platform Competition/")
mySymbols = c("BAC", "C", "GOOG", "GRPN", "JBLU", "MSFT", "RAD", "TSLA", "NFLX")
myOutputFolder <- paste(getwd(), '/Code/DepthVolume/output/', sep="")

theSeries <- retrieveGraphInputData(mySymbols)
createPlotAnim(theSeries, paste(myOutputFolder, 'frames/', 'unfiltered/', 'Min', '/', sep=""), 'Min')
createPlotAnim(theSeries, paste(myOutputFolder, 'frames/', 'unfiltered/', 'Avg', '/', sep=""), 'Avg')
createCoeffPlot(theSeries, paste(myOutputFolder, 'frames/', 'unfiltered/', 'Min', '/', sep=""))
createCoeffPlot(theSeries, paste(myOutputFolder, 'frames/', 'unfiltered/', 'Avg', '/', sep=""))
theFilteredSeries <- theSeries[which(!(theSeries$Exchange %in% c("NSX", "CHX", "CBSX", "NASDAQ.PSX"))),]
createPlotAnim(theFilteredSeries, paste(myOutputFolder, 'frames/', 'filtered/', 'Min', '/', sep=""), 'Min')
createPlotAnim(theFilteredSeries, paste(myOutputFolder, 'frames/', 'filtered/', 'Avg', '/', sep=""), 'Avg')
createCoeffPlot(theFilteredSeries, paste(myOutputFolder, 'frames/', 'filtered/', 'Min', '/', sep=""))
createCoeffPlot(theFilteredSeries, paste(myOutputFolder, 'frames/', 'filtered/', 'Avg', '/', sep=""))





a=read.csv("C:\\Users\\tcho\\Dropbox\\Project - Platform Competition\\Code\\DepthVolume\\data\\googQuotes.csv")
a$DATETIME = strptime(paste0(a$DATE, a$TIME), format="%Y%m%d%H:%M:%S")
a=a[a$DATETIME > strptime("2014012710:00:00", format="%Y%m%d%H:%M:%S") & 
      a$DATETIME < strptime("2014012711:00:00", format="%Y%m%d%H:%M:%S"),]
a=a[,c("DATETIME", "SYMBOL", "EX", "BIDSIZ", "BID", "OFR", "OFRSIZ", "MMID", "MODE")]
a$SPREAD <- a$OFR - a$BID
head(a)

myToHist <- a
myToHist <- myToHist[myToHist$SPREAD >0 & myToHist$SPREAD < 500,]

a[a$SPREAD < 0,]
g=a[a$SPREAD > 0 & a$SPREAD < 2,]
hist(g$SPREAD)
myToHist<-myToHist[myToHist$BIDSIZ < 20,]


ggplot(myToHist, aes(x=BIDSIZ, fill=EX)) + 
  geom_histogram(alpha=0.2, position="identity")
myAllSpreads = a$OFR - a$BID
myAllSpreads = myAllSpreads[myAllSpreads > 0 & myAllSpreads < 20]
hist(myAllSpreads)
byx=a[a$EX == "Y",]
myBYXSpreads = byx$OFR - byx$BID
myBYXSpreads = myBYXSpreads[myBYXSpreads > 0 & myBYXSpreads < 20]
hist(myAllSpreads)
hist(myBYXSpreads)


b=read.csv("C:\\Users\\tcho\\Dropbox\\Project - Platform Competition\\Code\\DepthVolume\\data\\jbluQuotes.csv")
b$DATETIME = strptime(paste0(b$DATE, b$TIME), format="%Y%m%d%H:%M:%S")
b=b[b$DATETIME > strptime("2014012710:44:36", format="%Y%m%d%H:%M:%S") & 
      b$DATETIME < strptime("2014012710:45:00", format="%Y%m%d%H:%M:%S"),]
b=b[,c("DATETIME", "SYMBOL", "EX", "BIDSIZ", "BID", "OFR", "OFRSIZ", "MMID", "MODE")]
head(b)

write.csv(b,"b")


b=read.csv("C:\\Users\\tcho\\Dropbox\\Project - Platform Competition\\Code\\DepthVolume\\data\\bacmsfttradesjan.csv")
b=b[b$EX =="M",]
b$COND[b$COND == "@4"] <- "4"
b$COND[b$COND == "@F"] <- "F"
ggplot(b, aes(x=COND, fill=COND)) + 
  geom_bar()
head(b)

b=read.csv("C:\\Users\\tcho\\Dropbox\\Project - Platform Competition\\Code\\DepthVolume\\data\\bacmar20quotes.csv")
b$DATETIME = strptime(paste0(b$DATE, b$TIME), format="%Y%m%d%H:%M:%S")
b=b[,c("DATETIME", "SYMBOL", "EX", "BIDSIZ", "BID", "OFR", "OFRSIZ", "MMID", "MODE")]
head(b)


b=read.csv("C:\\Users\\tcho\\Dropbox\\Project - Platform Competition\\Code\\DepthVolume\\data\\tslanflxquotes.csv")
b$DATETIME = strptime(paste0(b$DATE, b$TIME), format="%Y%m%d%H:%M:%S")
b=b[,c("DATETIME", "SYMBOL", "EX", "BIDSIZ", "BID", "OFR", "OFRSIZ", "MMID", "MODE")]
head(b)
'
createDancingBubblePlot(theSeries, "_BAC_GOOG_min")
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