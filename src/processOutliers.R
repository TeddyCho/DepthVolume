library(ggplot2)
library(scales)
source('./Code/helpers.r', chdir=T)

myOutputFolder = paste(getwd(), '/Code/DepthVolume/output/', sep="")

myDailyCond = read.csv("C:\\Users\\tcho\\Dropbox\\Project - Platform Competition\\Data\\daily_trade_cond.csv")
myDailyCond$EX = decodeExchange(myDailyCond$EX)
myDailyCond$COND <- sapply(myDailyCond$COND, function(x) simplifyConditionCode(as.character(x)))
myDailyCond = myDailyCond[myDailyCond$EX != "FINRA" & myDailyCond$volume_s != 0,]

myPerExchangeCond = aggregate(myDailyCond$volume_d, by = list(myDailyCond$EX, myDailyCond$COND), FUN = sum)
colnames(myPerExchangeCond) <- c("Exchange", "Condition_Code", "Volume_Dollar")
myPerExchangeCond$Proportion <- convertToProportionByColumn(myPerExchangeCond, 'Volume_Dollar', 'Exchange')
myPerExchangeCond$Volume_Dollar <- NULL

myPerExchangeCond <- fillInRows(myPerExchangeCond, 'Exchange', 'Condition_Code', 'Proportion')


  
ggplot(myPerExchangeCond, aes(x=Exchange, y=Proportion, fill=Condition_Code)) + 
  geom_bar(stat="identity") + 
  ggtitle("Volume (Dollars) By Condition Code") +
  theme(plot.title = element_text(size=20, face="bold", vjust=2)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file=paste(myOutputFolder, 'volumeByCond', '.png', sep=""), width = 12, height=10, dpi=100)


n1 <- nPlot(Proportion ~ Exchange, group = "Condition_Code", data = myPerExchangeCond, type = "multiBarChart")
n1$yAxis(showMaxMin = FALSE)
n1$chart(stacked = TRUE)
n1
brkPlot$set(title = paste(sym, "'s Exchange Shares over 2014", sep=""))
brkPlot$save(paste(getwd(),'\\output\\exchangeShares\\line', sym, '.html', sep=""),
             standalone = TRUE)
}