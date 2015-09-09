library(ggplot2)
library(scales)
library(rCharts)
source('./helpers.r', chdir=T)

enrichCondData <- function(aCondData){
  aCondData$EX = decodeExchange(aCondData$EX)
  aCondData$COND <- sapply(aCondData$COND, function(x) simplifyConditionCode(as.character(x)))
  colnames(aCondData) <- c("Date", "Exchange", "Condition_Code", "Volume_Shares", "Volume_Dollars")
  return(aCondData)
}
filterCondData <- function(aCondData){
  aCondData = aCondData[aCondData$Exchange != "FINRA" & aCondData$Volume_Shares != 0,]
  return(aCondData)
}
sumValuesByTwoGroups <- function(aDF, aValueName, aGroupOneName, aGroupTwoName){
  myValueSumsByTwoGroups = aggregate(aDF[,aValueName], 
                                by = list(aDF[,aGroupOneName], aDF[,aGroupTwoName]), FUN = sum)
  colnames(myValueSumsByTwoGroups) <- c(aGroupOneName, aGroupTwoName, aValueName)
  return(myValueSumsByTwoGroups)
}
createBarPlotByCond <- function(aDF, myOutputFolder, myFilename){
  ggplot(aDF, aes(x=Exchange, y=Proportion, fill=Condition_Code)) + 
    geom_bar(stat="identity") + 
    ggtitle("Volume (Dollars) By Condition Code in 2014") + 
    theme(plot.title = element_text(size=20, face="bold", vjust=2)) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(file=paste(myOutputFolder, myFilename, '.png', sep=""), width = 12, height=10, dpi=100)
  
  n1 <- nPlot(Proportion ~ Exchange, group = "Condition_Code", data = aDF, type = "multiBarChart")
  n1$yAxis(showMaxMin = FALSE)
  n1$chart(stacked = TRUE)
  n1$set(title = paste("Volume (Dollars) By Condition Code in 2014", sep=""))
  n1
  n1$save(paste(myOutputFolder, myFilename, '.html', sep=""),
          standalone = TRUE)
}

myOutputFolder = paste(getwd(), '/DepthVolume/output/', sep="")

myDailyCond = read.csv("C:\\Users\\tcho\\Dropbox\\Project - Platform Competition\\Data\\daily_trade_cond.csv")
myDailyCond <- enrichCondData(myDailyCond)
myDailyCond <- filterCondData(myDailyCond)

myVolumesByExchangeCond <- sumValuesByTwoGroups(myDailyCond, "Volume_Dollars", "Exchange", "Condition_Code")
myVolumesByExchangeCond$Proportion <- convertToProportionByColumn(myVolumesByExchangeCond,
                                                                  'Volume_Dollars', 'Exchange')
myVolumesByExchangeCond$Volume_Dollars <- NULL

myVolumesByExchangeCond <- fillInRows(myVolumesByExchangeCond, 'Exchange', 'Condition_Code', 'Proportion')
myVolumesByExchangeCond <- myVolumesByExchangeCond[with(myVolumesByExchangeCond, order(Exchange, Condition_Code)),]

createBarPlotByCond(myVolumesByExchangeCond[myVolumesByExchangeCond$Exchange=="CHX",],
                    myOutputFolder, 'volumeByCondCHX')
createBarPlotByCond(myVolumesByExchangeCond, myOutputFolder, 'volumeByCond')
