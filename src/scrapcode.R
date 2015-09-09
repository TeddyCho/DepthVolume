createDancingBubblePlot(theSeries, "_BAC_GOOG_min")
mapSymbolToColor <- function(aSymbol){
  mySymbols = c("AMD", "BAC", "C", "GOOG", "GRPN", "JBLU", "MSFT", "RAD")
  myColors = c("red", "orange", "yellow", "green", "blue", "purple", "black", "brown")
  myIndex = mySymbols == aSymbol
  return(myColors[myIndex])
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