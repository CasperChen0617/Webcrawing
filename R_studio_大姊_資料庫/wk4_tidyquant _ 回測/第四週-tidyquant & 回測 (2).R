# 中山大學財管所 李建龍
rm(list = ls());gc()
library(tidyquant)
library(quantmod)
library(tidyverse)
library(data.table)
library(lubridate)

# 用quantmod 套件爬取股價資料
# getSymbols
stock2330 <- getSymbols(Symbols = "2330.TW" , from ="2019-1-1" , src= "yahoo")
stock2330.xts <- get(stock2330) %>% na.omit() 

str(stock2330.xts)

stock2330 <- data.frame(date = index(stock2330.xts),  # index 為提取xts檔案的rownames，也就是日期
                        coredata(stock2330.xts))      # coredata 就是只取cell裡面的值

str(stock2330)

plotData <- xts(stock2330[,-1], order.by= stock2330$date) # 轉成xts形式來畫K線圖

plotData <- stock2330.xts

# 技術分析圖形
chartSeries(plotData,
            name = "2330",                 # 左上圖片標題
            subset = "2019::2020",         # 畫圖指定期間
            theme = chartTheme('black'),     
            up.col= "red",                 # K棒上漲為紅色(與國外相反)
            dn.col = "green")              # K棒下跌為綠色  

################## TTR 套件介紹 #####################
# TTR 計算技術指標
# 1.均線 (SMA)

MA5 <- SMA(stock2330$X2330.TW.Close, n = 5)     # MA5 五日均價(收盤價) 
head(MA5, 10)

MA10 <- SMA(stock2330$X2330.TW.Close, n = 10)   # MA10 十日均價(收盤價)
head(MA10, 10)

MA20 <- SMA(stock2330$X2330.TW.Close, n = 20)   # MA20 二十日均價(收盤價) 
head(MA20,20)

plotData$MA5 <- SMA(stock2330$X2330.TW.Close, n = 5)    # 在plotData 中新增MA5的欄位
plotData$MA10 <- SMA(stock2330$X2330.TW.Close, n = 10)  # 在plotData 中新增MA10的欄位
plotData$MA20 <- SMA(stock2330$X2330.TW.Close, n = 20)  # 在plotData 中新增MA20的欄位

#在chartSeris中加入技術因子的圖
addTA(plotData$MA5 ,type="l", on = 1, col = "yellow", lwd = 1.5)
addTA(plotData$MA10 ,type="l", on = 1, col = "blue", lwd = 1.5)
addTA(plotData$MA20 ,type="l", on = 1, col = "purple", lwd = 1.5)

# 2.RSI  ()
rsi6 <- RSI(stock2330$X2330.TW.Close, n = 6)

head(rsi6,10)

# 3.macd
macd <- MACD(stock2330$X2330.TW.Close)

# 4.kd
kd <- stoch(stock2330[,3:5] , 9,3,3)

################## 自製計時器 #################

ptm <- proc.time()  # 紀錄現在時間
#..... 等程式碼
proc.time() - ptm   # 現在時間 - ptm 所記錄時候的時間

#################  用 quantmod 抓取多檔股票 ##################
# 爬取多檔股票
stockCodeList <- c("AAPL","AMZN","GOOG","AABA","AAON","ACAD")   # 要抓取的股票代號
ETFCodeList <- c("BND","VOO","VTI")                             # 要抓取的ETF代號


# 抓取股票資料
stockTable <- NULL    # 新建一個空表

ptm <- proc.time()   # 計時開始
# 用迴圈爬多檔資料
for (i in 1:length(stockCodeList)) {
  stock <- getSymbols(Symbols = stockCodeList[i] , from = "2018-01-01", src = "yahoo")
  stock.xts <- get(stock) %>% na.omit()
  
  stock.dataframe <- data.frame(date = index(stock.xts),  # index 為提取xts檔案的rownames，也就是日期
                                 coredata(stock.xts))      # coredata 就是只取cell裡面的值
  colnames(stock.dataframe) <- c("date", "open", "high", "low", "close", "volume", "adjusted")  # 重新命名欄位因欄位名稱為股票代號+價格無法 bind_rows
  
  stock.dataframe <- stock.dataframe %>% 
    mutate(code = stockCodeList[i]) %>%        # 原本沒有股票代號欄位，新增股票代號欄位
    select(code, everything())                 # 欄位排序為先code其餘照原本順序

  stockTable <- stockTable %>%
    bind_rows(stock.dataframe)                 # 將下載下來的資料存入table中
}
proc.time()-ptm     # 計時結束


# 抓取 ETF資料
stockTable <- NULL     # 新建一個空表

ptm <- proc.time()  # 計時開始
# 用迴圈爬多檔資料
for (i in 1:length(ETFCodeList)) {
  stock <- getSymbols(Symbols = stockCodeList[i] , from = "2018-1-1", src = "yahoo")
  stock.xts <- get(stock) %>% na.omit()
  
  stock.dataframe <- data.frame(date = index(stock.xts),  # index 為提取xts檔案的rownames，也就是日期
                                coredata(stock.xts))      # coredata 就是只取cell裡面的值
  colnames(stock.dataframe) <- c("date", "open", "high", "low", "close", "volume", "adjusted")  # 重新命名欄位因欄位名稱為股票代號+價格無法 bind_rows
  
  stock.dataframe <- stock.dataframe %>% 
    mutate(code = stockCodeList[i]) %>%        # 原本沒有股票代號欄位，新增股票代號欄位
    select(code, everything())                 # 欄位排序為先code其餘照原本順序
  
  stockTable <- stockTable %>%
    bind_rows(stock.dataframe)                 # 將下載下來的資料存入table中
}

proc.time()-ptm     # 計時結束


# 用 map 的寫法，抓取股票
stockDataSpider <- function(stockCodeList){
  stock <- getSymbols(Symbols = stockCodeList , from = "2018-1-1", src = "yahoo")
  stock.xts <- get(stock) %>% na.omit()
  
  stock.dataframe <- data.frame(date = index(stock.xts),  # index 為提取xts檔案的rownames，也就是日期
                                coredata(stock.xts))      # coredata 就是只取cell裡面的值
  colnames(stock.dataframe) <- c("date", "open", "high", "low", "close", "volume", "adjusted")  # 重新命名欄位因欄位名稱為股票代號+價格無法 bind_rows
  
  stock.dataframe <- stock.dataframe %>% 
    mutate(code = stockCodeList[i]) %>%        # 原本沒有股票代號欄位，新增股票代號欄位
    select(code, everything())                 # 欄位排序為先code其餘照原本順序
  
  return(stock.dataframe)
}

ptm <- proc.time() # 計時開始
stockTableMap <- map_dfr(stockCodeList,stockDataSpider)   # 以data.frame用rbind黏起來
proc.time() - ptm    # 計時結束    

######################  tidyquant  ##################
# 載入套件
# library(tidyquant)

stockCodeList <- c("AAPL","AMZN","GOOG","AABA","AAON","ACAD")   # 要抓取的股票代號

# 下載資料
stockData <- c("AAPL","AMZN","GOOG","AABA","AAON","ACAD") %>%
  tq_get(get = "stock.price", from = "2018-01-01")

ptm <- proc.time() # 開始計時

stockData1 <- stockCodeList %>%
  tq_get(get = "stock.price", from = "2018-01-01")    

proc.time() - ptm  # 結束計時


#######################  回測部分  #######################
rm(list=ls());gc()
library(tidyquant)

# 下載股價資料(Import)
stockData <- c("AAPL", "AMZN") %>%
  tq_get(get = "stock.price", from = "2001-01-01")

stockData <- stockData %>%
  rename(code = symbol) %>%
  group_by(code) %>%
  arrange(code,date) %>% 
  filter(n()>20) %>%                 # 篩選掉日期較少的股票             
  mutate(MA5 = SMA(close,5),
         MA10 = SMA(close,10),
         MA20 = SMA(close,20)) %>%
  na.omit() %>%
  mutate(date = as.character(date),
         longInsite = as.numeric(MA5 > MA10 & MA10 > MA20), #進場退場訊號
         longOutsite = as.numeric( MA5 < MA10),
         shortInsite = as.numeric(MA5 < MA10 & MA10 < MA20),
         shortOutsite = as.numeric( MA5 > MA10)) 

# 將處理完的資料存成訊號表
tradeSignalTable <- stockData

#股票個數
stockCode <- unique(stockData$code)

#起始部位 = 0
holdNow <- 0            # 是否持有
position <- 0           # 持有部位數量
shortPrice <-NA   # 下面程式碼有控制是否做空，NA 為不做空

#建立交易表
tradeDetailTable <- NULL

#由於是單支，先不對所有股票建立交易表，只針對第一隻做紀錄
#1:length(stockCode) 全部做紀錄改成 1:1

i = 1
for ( i in 1:1){
  
  indData <- tradeSignalTable %>% filter(code == stockCode[i])  # 篩選出要做回測的股票
  
  iz = 1
  #每檔開始做紀錄
  for(iz in 1:nrow(indData)){
    cat(paste0("目前正在讀取第 ",i," 個股票內容，進度: ",iz," / ",nrow(indData),"\n"))  # 計算現在做到第i支股票及第iz個row
    
    # 若目前未持有該檔股票(holdNow == 0)，且今日為進場訊號(longInsite == 1)，且不是最後一天，於明日開盤價進場
    # 進場條件1.未持有部位(holdNow == 0) & 多頭進場訊號(longInsite == 1) 
    if((holdNow == 0 & indData$longInsite[iz] == 1 & iz!=nrow(indData))){
      
      inDate <- indData$date[iz+1]  # 進場日期
      inPrice <- indData$open[iz+1] # 進場價格
      holdNow <- 1                  # 紀錄該股已持有
      position <- 1                 # 紀錄手上部位數量
    }    
    
    # 若目前持有部位，且今日為出場訊號，則於明日開盤價出場
    # 出場條件 1. 持有部位且多頭出場訊號(holdNow == 1 & longOutsite ==1)
    #          2. 最後一天出場
    
    #多頭時
    if(is.na(shortPrice)){
      
      if((holdNow == 1 & indData$longOutsite[iz]==1)| (holdNow == 1 & iz == nrow(indData))){
        
        outDate <- ifelse(iz==nrow(indData),indData$date[iz],indData$date[iz+1])   # 出場日期，如果為最後一天，則出場日期為最後一天
        
        outPrice <- ifelse(iz==nrow(indData),indData$close[iz],indData$open[iz+1]) # 出場價格，如果為最後一天，則出場價格為當天收盤價
        
        # 建立交易表
        tradeDetailTable <- rbind(tradeDetailTable, 
                                  data.frame(code = stockCode[i],
                                             position = position,
                                             inDate = inDate, 
                                             inPrice = inPrice,
                                             outDate = outDate, 
                                             outPrice = outPrice))
        
        holdNow <- 0                    # 紀錄目前未持有
        position <- 0                   # 清掉目前手中部位
        
      } 
    }
  }
}


tradePerformance <- tradeDetailTable %>%
  mutate(ret = (outPrice/inPrice)-1,                # 報酬率
         startDate = inDate,                        # 持有起始日期
         endDate = outDate,                         # 持有結束日期
         time_d = ymd(endDate)- ymd(startDate))      # 持有時間


# 交易表績效指標
# 平均報酬率
meanRet <- mean(tradePerformance$ret)
meanRet
# 報酬率標準差
sdRet <- sd(tradePerformance$ret)
sdRet
# 交易次數
transactionNumbers <- nrow(tradePerformance)
transactionNumbers
# 勝率
winRate <- sum(as.numeric(tradePerformance$ret>0))/nrow(tradePerformance)
winRate
# 最大報酬率
maxRet <- max(tradePerformance$ret)
maxRet
# 最小報酬率
minRet <- min(tradePerformance$ret)
minRet
# 平均交易時間 
avgTradePeriod <- mean(tradePerformance$time_d)
avgTradePeriod

# 將各項績效表現合併成表
performanceTable <- data.frame(meanRet = meanRet, 
                               sdRet = sdRet,
                               transactionNumbers = transactionNumbers,
                               winRate = winRate,
                               maxRet = maxRet,
                               minRet = minRet,
                               avgTradePeriod = avgTradePeriod)

# 將績效表現的數字做整理
performanceTable <- data.frame(meanRet = round(meanRet,4)*100,                 # 使用round 整理數字
                               sdRet = round(sdRet,4)*100,
                               transactionNumbers = transactionNumbers,
                               winRate = round(winRate,4)*100,
                               maxRet = round(maxRet,4)*100,
                               minRet = round(minRet,4)*100,
                               avgTradePeriod = round(avgTradePeriod))



# ---chart_Series---
# 繪製的交易樣本(取報酬率最低值畫圖)
plotSample <- which.min(tradePerformance$ret)

inDate <- tradePerformance$inDate[plotSample]
outDate <- tradePerformance$outDate[plotSample]
plotCode <- tradePerformance$code[plotSample]
position <-  tradePerformance$position[plotSample]

indData <- tradeSignalTable %>% filter(code == plotCode)
 
matchSite <- which(indData$date == inDate)-20                                 # 可改動scale 的日期
plotStartDate <- indData$date[ifelse(matchSite < 1, 1, matchSite)]                           

# 繪圖結束日
matchSite <- which(indData$date == outDate)+20                               # 可改動scale 的日期
plotEndDate <- indData$date[ifelse(matchSite > nrow(indData), nrow(indData), matchSite)]

# 整理繪製的股價資料期間範圍及欄位資料
plotData <- indData %>% filter(date >= plotStartDate , date <= plotEndDate)
plotData <- plotData %>% group_by() %>% select(date:volume, MA5:MA20)        # group_by() = ungroup()

#加入進出場位置
plotData <- plotData %>% mutate(inSite = ifelse(date == inDate,open,NA),
                                outSite = ifelse(date == outDate,open,NA))
plotData <- xts(plotData[,-1], order.by= ymd(plotData$date))

# 設定K棒顏色
myTheme <- chart_theme()
myTheme$col$dn.col <- c("green")  # 跌的K棒顏色
myTheme$col$up.col <- c("red")    # 漲的K棒顏色

# 畫出K線圖
chart_Series(x = plotData, name = plotCode, theme = myTheme)
add_Vo()
add_TA(x=plotData$MA5, on=1,  col="blue", lwd=3)
add_TA(x=plotData$MA10, on=1, col="black", lwd=2)
add_TA(x=plotData$MA20, on=1, col="orange", lwd=2)

# 畫出進出場點
add_TA(x=plotData$inSite, on=1, type="p", col="red", pch=5, cex=3, lwd=5)
add_TA(x=plotData$outSite, on=1, type="p", col="green", pch=6, cex=3, lwd=5)


################# PerformanceAnalytics ##################
rm(list = ls());gc()
#PerformanceAnalytics套件
library(PerformanceAnalytics)
library(tidyquant)

AAPL <- c("AAPL") %>%
  tq_get(get = "stock.price", from = "2019-01-01")

AAPL <- AAPL %>% mutate(ret= close/lag(close,1)-1) %>% filter(!is.na(ret))

# 計算累積報酬率
# 對基金日報酬率轉為xts格式
fundRetXts <- xts(AAPL %>% select(ret), order.by = ymd(AAPL$date))

# 年化報酬率
annualRet <- Return.annualized(fundRetXts) %>% as.vector()

# 夏普比率
sharpeRatio <- SharpeRatio(fundRetXts, FUN = c("StdDev"), annualize = T) %>% as.vector()

# 最大回撤率
mdd <- maxDrawdown(fundRetXts) %>% as.vector()

K <- charts.PerformanceSummary(fundRetXts, 
                               main = "策略基金累積報酬率走勢圖")



