import os
import zipfile
import csv
import datetime

class DepthHistory:
    def __init__(self, aOrderBook, aStartTime=datetime.datetime.strptime("19000101 00:00:01", "%Y%m%d %H:%M:%S"),
                 aEndTime=datetime.datetime.strptime("19000101 00:00:02", "%Y%m%d %H:%M:%S")):
        self.symbol = aOrderBook.symbol
        self.exchange = aOrderBook.exchange
        self.startTime = aStartTime
        self.endTime = aEndTime
        self.price = (aOrderBook.bid + aOrderBook.offer)/2
        self.depthHistory = [aOrderBook]
    def update(self, aOrderBook):
        self.depthHistory.append(aOrderBook)
    def containsTime(self, aTime):
        return(self.startTime <= aTime and aTime < self.endTime)
class Volume:
    def __init__(self, aTrade, aStartTime=datetime.datetime.strptime("19000101 00:00:01", "%Y%m%d %H:%M:%S"),
                 aEndTime=datetime.datetime.strptime("19000101 00:00:02", "%Y%m%d %H:%M:%S")):
        self.symbol = aTrade.symbol
        self.exchange = aTrade.exchange
        self.price = aTrade.price
        self.startTime = aStartTime
        self.endTime = aEndTime
        self.volume = aTrade.volume
    def update(self, aTrade):
        self.volume = self.volume + aTrade.volume
    def containsTime(self, aTime):
        return(self.startTime <= aTime and aTime < self.endTime)
class OrderBook:
    def __init__(self, aRow):
        self.dateTime = datetime.datetime.strptime(aRow["DATE"] + " " + aRow["TIME"], "%Y%m%d %H:%M:%S")
        self.symbol = aRow["SYMBOL"]
        self.exchange = inferExchange(aRow["EX"])
        self.bid = float(aRow["BID"])
        self.bidSize = int(aRow["BIDSIZ"])
        self.offer = float(aRow["OFR"])
        self.offerSize = int(aRow["OFRSIZ"])
        self.mode = aRow["MODE"]
        #self.mmId = aRow["MMID"]
class MilliOrderBook:
    def __init__(self, aRow):
        self.dateTime = datetime.datetime.strptime(aRow["DATE"] + " " + aRow["TIME_M"]+"000", "%Y%m%d %H:%M:%S.%f")
        self.symbol = aRow["SYM_ROOT"]
        self.exchange = inferExchange(aRow["EX"])
        self.bid = float(aRow["BID"])
        self.bidSize = int(aRow["BIDSIZ"])
        self.offer = float(aRow["ASK"])
        self.offerSize = int(aRow["ASKSIZ"])
        #self.mode = aRow["MODE"]
        #self.mmId = aRow["MMID"]
class Trade:
    def __init__(self, aRow):
        self.symbol = aRow["SYMBOL"]
        self.dateTime = datetime.datetime.strptime(aRow["DATE"] + " " + aRow["TIME_M"], "%Y%m%d %H:%M:%S.%f")
        self.price = float(aRow["PRICE"])
        self.volume = int(aRow["SIZE"])
        self.saleCondition = aRow["COND"]
        self.exchange = inferExchange(aRow["EX"])
def unzipFile(aFileName):
    myFilePath = os.path.join(os.getcwd(), "..") + '\\data\\' + aFileName + '_csv.zip'
    print("Unzipping " + myFilePath + "...")
    myFile = open(myFilePath, 'rb')
    z = zipfile.ZipFile(myFile)
    for name in z.namelist():
        z.extract(name, os.path.join(os.getcwd(), "..") + "\\data\\")
        myFile.close()
    return(os.path.join(os.getcwd(), "..") + "\\data\\" + aFileName + ".csv")
def inferExchange(aExchangeString):
    myCodeDict = {"A":"NYSE MKT", "B":"NASDAQ OMX BX", "C":"National",
                  "D":"FINRA", "I":"ISE", "J":"Direct Edge A",
                  "K":"Direct Edge X", "M":"Chicago", "N":"NYSE",
                  "T":"NASDAQ OMX", "P":"NYSE Arca SM", "S":"Consolidated Tape System",
                  "T/Q":"NASDAQ", "Q":"NASDAQ", "W":"CBOE", "X":"NASDAQ OMX PSX",
                  "Y":"BATS Y", "Z":"BATS"}
    try:
        return myCodeDict[aExchangeString]
    except:
        return "Invalid Exchange"
def asSeconds(aDateTime):
    return(aDateTime - datetime.datetime(1970, 1, 1)).total_seconds()
def asDateTime(aSeconds):
    return(datetime.datetime(1970, 1, 1) + datetime.timedelta(seconds=aSeconds))
def getFrameInfo(aOrderBuffer):
    myLastRow = aOrderBuffer.pop()
    return(myLastRow)
def carryValuesThroughZeros(aList):
    myLastValue = 9e9
    for i in range(len(aList)):
        myValue = aList[i]
        if myValue == 0:
            aList[i] = myLastValue
        else:
            myLastValue = myValue
    return(aList)
def matchSymbolExchangeToEntry(aCandidate, aEntry):
    return aEntry.symbol == aCandidate.symbol and\
        aEntry.exchange == aCandidate.exchange and\
        aEntry.containsTime(aCandidate.dateTime)
def aggregateFromFile(aFileName, aType, aPeriods):
    theListOfAggregations = list()
    i=0
    t=0
    mySymbolColumn = "SYM_ROOT"
    myStartTime = aPeriods[t][0]
    myEndTime = aPeriods[t][1]
    myLastPeriodEnd = aPeriods[len(aPeriods)-1][1]
    with open(aFileName, 'r') as file: 
        myReader = csv.DictReader(file)
        for myRow in myReader:
            if aType == "trade":
                myRow["TIME_M"] = myRow["TIME"] + ".000" # Gross, I know.
                mySymbolColumn = "SYMBOL"                
            myRowTime = datetime.datetime.strptime(myRow["DATE"] + " " + myRow["TIME_M"], "%Y%m%d %H:%M:%S.%f")    
            
            if myRowTime >= myStartTime: #filter condition on myRow["EX"] != "FINRA" and 
                myCandidate = Trade(myRow) if aType == "trade" else MilliOrderBook(myRow)
                myMatched = False
                for j in range(len(theListOfAggregations)):
                    if matchSymbolExchangeToEntry(myCandidate, theListOfAggregations[j]):
                        theListOfAggregations[j].update(myCandidate)
                        myMatched = True
                        break
                if not myMatched:
                    myAggregation = Volume(myCandidate, myStartTime, myEndTime) if aType=="trade" else DepthHistory(myCandidate,myStartTime,myEndTime)
                    theListOfAggregations.append(myAggregation)
            if i%10000 == 0:
                print(myRow[mySymbolColumn] + " " + myRow["DATE"] + " "  + myRow["TIME_M"])
            if myRowTime > myLastPeriodEnd:
                theListOfAggregations = sorted(theListOfAggregations, key=lambda k: k.symbol + k.exchange) 
                return(theListOfAggregations)
            if myRowTime > myEndTime:
                t=t+1
                myStartTime = aPeriods[t][0]
                myEndTime = aPeriods[t][1]
            i=i+1
    theListOfAggregations = sorted(theListOfAggregations, key=lambda k: k.symbol + k.exchange) 
    return(theListOfAggregations)   
def summarizeDepthHistory(aDepthHistory):
    if aDepthHistory == 9e9 or len(aDepthHistory) < 2:
        return(float("nan"))
    myTimeWeights = [(x.dateTime - aDepthHistory[i-1].dateTime).total_seconds() for i,x in enumerate(aDepthHistory) if i>0]
    aDepthHistory.pop()
    if sum(myTimeWeights) == 0:
        return(float("nan"))
    else:
        theSummarizedValue = sum([(x.bidSize + x.offerSize)*myTimeWeights[i]/2 for i,x  in enumerate(aDepthHistory)]) / sum(myTimeWeights)
        return theSummarizedValue
def returnVolume(aSymbol, aExchange, aStartTime, aEndTime, aVolumes):
    for v in aVolumes:
        if v.symbol == aSymbol and v.exchange == aExchange and v.startTime == aStartTime and v.endTime == aEndTime:
            return(v.volume)
    return(9e9)
def returnPrice(aSymbol, aExchange, aStartTime, aEndTime, aAggregates):
    for v in aAggregates:
        if v.symbol == aSymbol and v.exchange == aExchange and v.startTime == aStartTime and v.endTime == aEndTime:
            return(v.price)
    return(9e9)
def returnDepthHistory(aSymbol, aExchange, aStartTime, aEndTime, aDepthHistories):
    for dh in aDepthHistories:
        if dh.symbol == aSymbol and dh.exchange == aExchange and dh.startTime == aStartTime and dh.endTime == aEndTime:
            return(dh.depthHistory)
    print(aSymbol+" "+ aExchange)
    return(9e9)
def joinVolumesAndDepthHistories(aVolumes, aDepthHistories):
    aDepthHistoriesSEPairs = [(x.symbol, x.exchange, x.startTime, x.endTime) for x in aDepthHistories]
    mySymbolExchangePairs = set([(x.symbol, x.exchange, x.startTime, x.endTime) for x in aVolumes \
                                 if (x.symbol, x.exchange, x.startTime, x.endTime) in aDepthHistoriesSEPairs])
    theVolumesandDepthHistories = [{"symbol": x[0], "exchange": x[1], "startTime": x[2], "endTime": x[3],
                                    "volume": returnVolume(x[0], x[1], x[2], x[3], aVolumes),
                                    "tradePrice": returnPrice(x[0], x[1], x[2], x[3], aVolumes),
                                    "bookPrice": returnPrice(x[0], x[1], x[2], x[3], aDepthHistories),
                                    "depthHistory": returnDepthHistory(x[0], x[1], x[2],x[3], aDepthHistories)}
                                   for x in mySymbolExchangePairs]
    return(theVolumesandDepthHistories)
if __name__ == '__main__':
    mySymbols = ["AMD", "BAC", "C", "GOOG", "GRPN", "JBLU", "MSFT", "RAD"]
    mySymbols = ["GOOG", "GRPN", "JBLU", "MSFT", "RAD", "C"]
    mySymbols = ["C"]
    myDays=5
    
    myStartTime = datetime.datetime.strptime("20140301 10:00:00", "%Y%m%d %H:%M:%S")
    myStartTimes = [(myStartTime + c*datetime.timedelta(hours=24)) for c in range(myDays)]
    myEndTimes = [(t + datetime.timedelta(hours=5, minutes=30)) for t in myStartTimes]
    myPeriods = [(myStartTimes[i], myEndTimes[i]) for i in range(len(myStartTimes))]
    
    for sym in mySymbols:
        myOrderFileName = os.path.join(os.getcwd(), "..\\data\\") + sym + "_mar_q" + ".csv"
        myTradeFileName = os.path.join(os.getcwd(), "..\\data\\") + sym + "_mar_t" + ".csv"
        myOutputFolder = os.path.join(os.getcwd(), "..\\output\\")
        
        print("Parsing out volumes")
        myVolumes = aggregateFromFile(myTradeFileName, "trade", myPeriods)

        print("Parsing out depth histories")
        myDepthHistories = aggregateFromFile(myOrderFileName, "order", myPeriods)
        
        print("Joining volumes and depths")
        myJoinedVolumesAndDepthHistories = joinVolumesAndDepthHistories(myVolumes, myDepthHistories)
        
        myVolumesAndDepths = [{"exchange":x["exchange"], "symbol": x["symbol"], "volume":x["volume"],
                               "startTime": x["startTime"], "endTime": x["endTime"],
                               "tradePrice": x["tradePrice"], "bookPrice": x["bookPrice"],
                               "depth": summarizeDepthHistory(x["depthHistory"])} for x in myJoinedVolumesAndDepthHistories]
        print(myVolumesAndDepths)
            
        print("Writing to csv")
        keys = myVolumesAndDepths[0].keys()
        aFileName = myOutputFolder + sym + "tqd" +str(myDays) + ".csv"
        with open(aFileName, 'wt') as aFile:
            dict_writer = csv.DictWriter(aFile, keys)
            dict_writer.writeheader()
            dict_writer.writerows(myVolumesAndDepths)
        """except:
            print("hi")"""
    print("done")