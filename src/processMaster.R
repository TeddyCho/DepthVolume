library(ggplot2)
library(scales)
source('./helpers.r', chdir=T)
options(scipen=999)

getDepthVolumeData <- function(){
  myRawData = read.csv(paste(getwd(), "/../Data/depth vs volume/msec/average_depth2014m.csv", sep=""),
                       stringsAsFactors=FALSE)
  myTradeCountData = read.csv(paste(getwd(), "/../Data/depth vs volume/sec/trade_count.csv", sep=""),
                              stringsAsFactors=FALSE)
  myRawData <- merge(myRawData, myTradeCountData, by=c("SYMBOL", "EX"))
  aData <- filterData(myRawData)
  return(aData)
}
filterData <- function(aData){
  aData <- aData[!(aData$EX %in% c("D")),]
  #myRelevantColumns = c("SYMBOL", "EX", "price", "depth_d", "volume_d", 'spread')
  #aData <- aData[,myRelevantColumns]
  aData<- aData[!is.na(aData$price),]
  return(aData)
}
transformToBracket <- function(aValue, aCutoffs){
  myLowerBoundIndex <- max(which(aCutoffs < aValue))
  if(myLowerBoundIndex == length(aCutoffs)){
    return(paste(aCutoffs[myLowerBoundIndex], "+", sep=""))
  } else{
    myUpperBoundIndex <- myLowerBoundIndex + 1
    return(paste(aCutoffs[myLowerBoundIndex], " to ", aCutoffs[myUpperBoundIndex], sep=""))
  }
}
getOrderedBrackets <- function(aCutoffs){
  myStrings<- c()
  for(i in 2:length(aCutoffs)){
    myMax <- aCutoffs[i]
    myMin <- aCutoffs[i-1]
    
    myStrings <- c(myStrings, paste(myMin, " to ", myMax, sep=""))
  }
  myStrings <- c(myStrings, paste(aCutoffs[length(aCutoffs)], '+', sep=""))
  return(myStrings)
}
runThroughFilter <- function(aData, myIsValid, aOriginalSymbols){
  mySymbols = unique(aData$SYMBOL)
  
  aData <- aData[myIsValid,]
  myPointCount <- unlist(lapply(mySymbols, function(x) sum(aData$SYMBOL==x)))
  myEnoughSymbols <- mySymbols[myPointCount > 3]
  myIsEnoughPoints <- aData$SYMBOL %in% myEnoughSymbols
  myValidSymbolCount <- sum(myIsEnoughPoints)
  print(paste(length(myEnoughSymbols), " of ", length(aOriginalSymbols), " symbols valid. ", 
              length(myEnoughSymbols)/length(aOriginalSymbols), sep=""))
  print(paste(length(aOriginalSymbols) - length(myEnoughSymbols), " symbols discarded.", sep=""))
  return(aData[myIsEnoughPoints,])
}
makeDuplicates <- function(aCoeffDF){
  myBrackets = getOrderedBrackets(aVolumeCutoffs)
  for(i in length(myBrackets):2){
    myCurrentBracket = myBrackets[i]
    myLowerBracket = myBrackets[i-1]
    myRowsToDupe <- myCoeffDF[myCoeffDF$volumeBracket == myCurrentBracket,]
    myRowsToDupe$volumeBracket = myLowerBracket
    myCoeffDF <- rbind(myCoeffDF, myRowsToDupe)
  }
  myNewCategories <- sapply(aVolumeCutoffs, function(x) paste(x, '+',sep=''))
  
  myCoeffDF$volumeBracket <- sapply(myCoeffDF$volumeBracket, function(x) myNewCategories[myBrackets==x])
  
  return(myCoeffDF)
}
createCoeffDF <- function(aData){
  mySymbols <- unique(aData$SYMBOL)
  myCoeffDF <- data.frame()
  for(sym in mySymbols){
    mySymSeries <- aData[which(aData$SYMBOL == sym),]
    
    tryCatch({
      myFit <- lm(log10(volume_d) ~ log10(depth_d), data = mySymSeries)
      myCoeff <- data.frame(symbol=sym, volume_d = sum(mySymSeries[,'volume_d'], na.rm=TRUE),
                            price = sum(mySymSeries[,'volume_d'], na.rm=TRUE) / sum(mySymSeries[,'volume_s'], na.rm=TRUE),
                            Alpha=summary(myFit)$coefficients[1], 
                            Beta=summary(myFit)$coefficients[2],
                            pointCount = dim(mySymSeries)[1],
                            tradeCount = sum(mySymSeries$number_trade_avg, na.rm=TRUE))
      myCoeffDF <- rbind(myCoeffDF, myCoeff)
    }, error=function(e) NULL)
    print(sym)
  }
  myCoeffDF$priceBracket <- factor(sapply(myCoeffDF$price, function(x) transformToBracket(x, aPriceCutoffs)),
                                   levels = getOrderedBrackets(aPriceCutoffs))
  myCoeffDF$volumeBracket <- factor(sapply(myCoeffDF$volume_d, function(x) transformToBracket(x, aVolumeCutoffs)),
                                    levels = getOrderedBrackets(aVolumeCutoffs))
  return(myCoeffDF)
}
createCoeffPlot <- function(aData, myOutputFolder, aSuffix, aByColumn, aCutoffs){
  mySymbols <- unique(aData$SYMBOL)
  myCoeffDF <- data.frame()
  aByColumn='Volume'
  doPrint = TRUE
  for(sym in mySymbols){
    mySymSeries <- aData[which(aData$SYMBOL == sym),]
    
    tryCatch({
      myFit <- lm(log10(volume_d) ~ log10(depth_d), data = mySymSeries)
      if(TRUE){
        myCoeff <- data.frame(symbol=sym, volume_d = sum(mySymSeries[,'volume_d'], na.rm=TRUE),
                              price = sum(mySymSeries[,'volume_d'], na.rm=TRUE) / sum(mySymSeries[,'volume_s'], na.rm=TRUE),
                              Alpha=summary(myFit)$coefficients[1], 
                              Beta=summary(myFit)$coefficients[2],
                              pointCount = dim(mySymSeries)[1],
                              tradeCount = sum(mySymSeries$number_trade_avg, na.rm=TRUE))
        myCoeffDF <- rbind(myCoeffDF, myCoeff)
      }
      if(FALSE){
        for(cut in aVolumeCutoffs){
          if(sum(mySymSeries[,'volume_d'], na.rm=TRUE) > cut) {
            myCoeff <- data.frame(symbol=sym, volume_d = sum(mySymSeries[,'volume_d'], na.rm=TRUE),
                                  price = sum(mySymSeries[,'volume_d'], na.rm=TRUE) / sum(mySymSeries[,'volume_s'], na.rm=TRUE),
                                  Alpha=summary(myFit)$coefficients[1], 
                                  Beta=summary(myFit)$coefficients[2],
                                  pointCount = dim(mySymSeries)[1],
                                  volumeThresh = paste('>',cut,sep=''))
            myCoeffDF <- rbind(myCoeffDF, myCoeff)
          }
        }
      }
      ###
      ###
    }, error=function(e) NULL)
    if(doPrint){
      print(sym)
    }
    doPrint = !doPrint
  }
  myCoeffDF$priceBracket <- factor(sapply(myCoeffDF$price, function(x) transformToBracket(x, aPriceCutoffs)),
                                   levels = getOrderedBrackets(aPriceCutoffs))
  #myCoeffDF$priceBracket <- factor(sapply(myCoeffDF$price, function(x) transformToBracket(x, aPriceCutoffs)),
  #                                 levels = matrix(getOrderedBrackets(aPriceCutoffs), nrow=2, byrow=TRUE))
  myCoeffDF$volumeBracket <- factor(sapply(myCoeffDF$volume_d, function(x) transformToBracket(x, aVolumeCutoffs)),
                                    levels = getOrderedBrackets(aVolumeCutoffs))
  #myCoeffDF$volumeBracket <- factor(sapply(myCoeffDF$volume_d, function(x) transformToBracket(x, aVolumeCutoffs)),
  #                                  levels = matrix(getOrderedBrackets(aVolumeCutoffs), nrow=2, byrow=TRUE))
  aVolumeThreshes = unique(myCoeffDF$volumeThresh)
  myCoeffDF$volumeThresh <- factor(myCoeffDF$volumeThresh, levels = matrix(aVolumeThreshes, nrow=2, byrow=TRUE))
  myCoeffDF$volumeThresh <- factor(myCoeffDF$volumeThresh, levels = aVolumeThreshes)
  
  if(FALSE){
    sapply(getOrderedBrackets(aVolumeCutoffs), function(x) sum(myCoeffDF$volumeBracket ==x))
    ggplot(mySymSeries, aes(x=depth_d, y=volume_d)) + geom_point() + geom_text(aes(label=EX), hjust=1, vjust=1) + 
      scale_x_log10() + scale_y_log10()
  }
  
  ggplot(myCoeffDF, aes(x=Beta, fill=volumeThresh)) + 
    scale_x_continuous(limits = c(-5, 5)) +
    geom_histogram(aes(y=.1*..density..), alpha=.6, binwidth=.1) +
    geom_vline(aes(xintercept=1), colour = "green") + 
    ggtitle(paste('Histograms of Betas For Different ', aByColumn, ' Buckets', sep='')) +
    facet_wrap(~volumeThresh, ncol=2, scales="free") + 
    guides(fill=FALSE)
  ggsave(file=paste(myOutputFolder, "BetaHistogramsBy", aByColumn, aSuffix, '.png', sep=""), width = 12, height=10,dpi=100)
  
  myBetaDF <- data.frame(priceBracket = getOrderedBrackets(aPriceCutoffs),
                         meanBetas = unlist(lapply(getOrderedBrackets(aPriceCutoffs),
                                                   function(x) mean(myCoeffDF$Beta[myCoeffDF[,'priceBracket']==x], na.rm=TRUE))))
  
  myBetaDF <- data.frame(volumeBracket = unique(myCoeffDF$volumeThresh),
                         meanBetas = unlist(lapply(unique(myCoeffDF$volumeThresh),
                                                   function(x) mean(myCoeffDF$Beta[myCoeffDF[,'volumeThresh']==x], na.rm=TRUE))))
  
  ggplot(myBetaDF, aes(x=volumeBracket, y=meanBetas, group=1)) + 
    geom_point() +  geom_line() +
    ggtitle(paste('Mean Betas For Different ', aByColumn, ' Buckets', sep=''))
  ggsave(file=paste(myOutputFolder, "BetaMeansBy", aByColumn, aSuffix, '.png', sep=""), width = 12, height=10,dpi=100)
  return(myCoeffDF)
}
appendExchangeShareColumn <- function(aData){
  mySymbols = unique(aData$SYMBOL)
  mySymbolVolumes <- sapply(mySymbols, function(x) sum(aData$volume_d[aData$SYMBOL==x], na.rm=TRUE))
  
  aData$exchangeShare <- apply(aData, 1, function(row) as.numeric(row['volume_d']) / mySymbolVolumes[row['SYMBOL']])
  return(aData)
}

myOutputFolder = paste(getwd(), '/DepthVolume/output/test30MinChop/', sep="")
dir.create(myOutputFolder, showWarnings = FALSE)
aBaseData <- appendExchangeShareColumn(getDepthVolumeData())
aData<- aBaseData

aPriceCutoffs <- c(0,5,10,100)
myByColumn = 'price'
aVolumeCutoffs <- c(0, 2e5, 1e6, 5e6, 1e8)
myByColumn = 'volume_d'

myOriginalSymbols = unique(aData$SYMBOL)

aData <- runThroughFilter(aData, TRUE, myOriginalSymbols)
createCoeffPlot(aData, myOutputFolder, '', myByColumn, aCutoffs)


mySymbols = unique(aData$SYMBOL)
myPerSymbolVolume <- sapply(mySymbols, function(x) sum(aData$volume_d[aData$SYMBOL==x]))
aData$marketShare <- apply(aData, 1, function(x) as.numeric(x['volume_d'])/myPerSymbolVolume[mySymbols==x['SYMBOL']])


myIsNotRinkyDink <- !(aData$EX %in% c("M", "C", "W", "X"))
aData <- runThroughFilter(aData, myIsNotRinkyDink, myOriginalSymbols)
createCoeffPlot(aData, myOutputFolder, 'NoRinkyDink', myByColumn, aCutoffs)

myIsValidSpread <- !is.na(aData$spread) & aData$spread < .04
aData <- runThroughFilter(aData, myIsValidSpread, myOriginalSymbols)
createCoeffPlot(aData, myOutputFolder, 'NoRinkyDinkLessThan4Cents', myByColumn, aCutoffs)

myIsValidSpread <- !is.na(aData$spread) & aData$spread < .02
aData <- runThroughFilter(aData, myIsValidSpread, myOriginalSymbols)
createCoeffPlot(aData, myOutputFolder, 'NoRinkyDinkLessThan2Cents', myByColumn, aCutoffs)


##########
myIsValidExchangeShare <- aData$exchangeShare > .05
aData <- runThroughFilter(aData, myIsValidExchangeShare, myOriginalSymbols)
myCoeffDF <- createCoeffDF(aData)
aByColumn = 'Volume and Price'
aSuffix = 'Filtered for Exchange Share Greater Than 5'

myCoeffDF$share <- factor(myCoeffDF$share,
                          levels = c(">0",">10", ">5", ">15"))
ggplot(myCoeffDF, aes(x=Beta, fill=priceBracket)) + 
  scale_x_continuous(limits = c(-5, 5)) +
  geom_histogram(aes(y=.1*..density..), alpha=.6, binwidth=.1) +
  geom_vline(aes(xintercept=1), colour = "green") + 
  ggtitle(paste('Histograms of Betas For Different ', aByColumn, ' Buckets, ', '%', aSuffix, sep='')) +
  facet_grid(priceBracket~volumeBracket) + 
  guides(fill=FALSE)
ggsave(file=paste(myOutputFolder, "BetaHistogramsBy", gsub(' ', '', aByColumn), gsub(' ', '', aSuffix), 'Percent', '.png', sep=""), 
       width = 12, height=10,dpi=100)

#####
myIsValid = aData$exchangeShare>.15
aData <- runThroughFilter(aData, myIsValid, myOriginalSymbols)

myToAdd <- createCoeffDF(aData)
myToAdd$share = '>15'
myCoeffDF <- rbind(myCoeffDF, myToAdd)
ggplot(myCoeffDF, aes(x=Beta, fill=share)) + 
  scale_x_continuous(limits = c(-5, 5)) +
  geom_histogram(aes(y=.1*..density..), alpha=.6, binwidth=.1) +
  geom_vline(aes(xintercept=1), colour = "green") + 
  ggtitle(paste('Histograms of Betas For Different ', 'Share', ' Buckets', sep='')) +
  facet_wrap(~share, ncol=2, scales="free") + 
  guides(fill=FALSE)
ggsave(file=paste(myOutputFolder, "BetaHistogramsBy", 'Share', '.png', sep=""), 
       width = 12, height=10,dpi=100)
myBetaDF <- data.frame(share = unique(myCoeffDF$share),
                       meanBetas = unlist(lapply(unique(myCoeffDF$share),
                                                 function(x) mean(myCoeffDF$Beta[myCoeffDF[,'share']==x], na.rm=TRUE))))

ggplot(myBetaDF, aes(x=share, y=meanBetas, group=1)) + 
  geom_point() +  geom_line() +
  ggtitle(paste('Mean Betas For Different ', 'Share', ' Buckets', sep=''))
ggsave(file=paste(myOutputFolder, "BetaMeansBy", 'Share', '.png', sep=""), width = 12, height=10,dpi=100)

myRawData = myRawData[!(myRawData$EX %in% c("D")),]
mySymbols = unique(myRawData$SYMBOL)
myMedianSpreads <- sapply(mySymbols, function(x) sum(myRawData$volume_s[myRawData$SYMBOL==x], na.rm=TRUE))
myVolumes <- sapply(mySymbols, function(x) sum(myRawData$volume_d[myRawData$SYMBOL==x], na.rm=TRUE))

myVolumeSpread <- data.frame(mySymbols, myVolumes, myMedianSpreads)
myVolumeSpread <- myVolumeSpread[!is.na(myVolumeSpread$myVolumes) & !is.na(myVolumeSpread$myMedianSpreads)
                                 & myVolumeSpread$myVolumes != 0 & myVolumeSpread$myMedianSpreads != 0,]

plot(log(myVolumeSpread$myVolumes), log(myVolumeSpread$myMedianSpreads))

fit <- lm(log(myMedianSpreads) ~ log(myVolumes), data = myVolumeSpread)
summary(fit)
abline(fit)
