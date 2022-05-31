rm(list = ls());gc()
library(PerformanceAnalytics)
library(quantmod)
library(xts)
library(lubridate)
library(xgboost)
library(RMySQL)    
library(data.table)
library(fastDummies)
library(ggplot2)
library(foreach)
library(doParallel)
library(zoo)
library(rvest)
library(ggpubr)
library(RMySQL)
library(tidyverse)

# 讀取自製函數
source("CustomFunction.R", encoding = "big5")

# 是否啟用回溯
refreshAllData <- T

# 資料庫連線設定
dbHost <- "140.117.76.124"
dbUser <- "nsysu_quant"
dbPassword <- "5l jp6wj062jo4"

# 設定資料起始日
dataStartDate <- ifelse(refreshAllData == T, 20000000, as.numeric(gsub("-", "", Sys.Date()-years(2))))

#################################### 下載資料 ####################################
# 連線至資料庫
channel <- dbConnect(dbDriver("MySQL"), host = dbHost, user = dbUser, password = dbPassword)
dbSendQuery(channel, "set names big5;")

########### 整理當沖交易資料 ###########
# 當沖交易資料
queryString <- paste0("select code, date, day_trade_volume, day_trade_buy_value, day_trade_sell_value ",
                      "from stock_market.day_trade_data where date >= ", dataStartDate, " order by code, date;")
res <- dbSendQuery(channel, queryString)
stock_daytrade_list_table <- fetch(res, n = -1) %>% as_tibble() %>% arrange(code, date)

# 今日當沖交易資料
queryString <- paste0("select code, date, day_trade_volume/1000 as day_trade_volume, day_trade_buy_value/1000 as day_trade_buy_value, ",
                      "day_trade_sell_value/1000 as day_trade_sell_value from stock_market.latest_day_trade_data order by code, date;")
res <- dbSendQuery(channel, queryString)
todayDayTradeData <- fetch(res, n = -1) %>% as_tibble() %>% arrange(code, date)

# 判斷是否需要併入今日當沖交易資料
if(max(todayDayTradeData$date) > max(stock_daytrade_list_table$date)){
  stock_daytrade_list_table <- stock_daytrade_list_table %>% bind_rows(todayDayTradeData) %>% arrange(code, date)
}

########### 整理股票資料 ###########
# 股票資料
queryString <- paste0("select code, name, date, open, high, low, close, trade_value, trade_volume ",
                      "from stock_market.stock_price_data where date >= ", dataStartDate, " order by code, date;")
res <- dbSendQuery(channel, queryString)
stock_table <- fetch(res, n = -1) %>% as_tibble() %>% arrange(code, date)

# 當日股票資料
queryString <- paste0("select code, name, date, open, high, low, close, trade_value, trade_volume ",
                      "from stock_market.today_stock_price order by code, date;")
res <- dbSendQuery(channel, queryString)
todayStockPrice <- fetch(res, n = -1) %>% as_tibble() %>% arrange(code, date)

# 判斷是否需要併入今日股價資料
if(max(todayStockPrice$date) > max(stock_table$date)){
  
  stock_table <- stock_table %>% bind_rows(todayStockPrice) %>% arrange(code, date)
}

########### 整理國外指數資料 ###########
# 國外指數
foreIndex_list <- c("SB01", "SB03", "SB05", "SB66", "SB04")  # 篩選需要的國外大盤指數
queryString <- paste0("select code, date, `index` from stock_market.foreign_index_data where date >= ", dataStartDate, 
                      " and code in (", paste0("'", paste0(foreIndex_list, collapse = "','"), "'"),") order by code, date;")
res <- dbSendQuery(channel, queryString)
foreIndex_table <- fetch(res, n = -1) %>% as_tibble() %>% arrange(code, date)

# SB01   台灣發行量加權股價指
# SB03   美國紐約道瓊工業平均
# SB04   日本東京日經225指   
# SB05   香港恆生指數        
# SB66   中國上海綜合股價指數

# 最新國外指數資料
queryString <- paste0("select code, date, `index` from stock_market.latest_foreign_index_data order by code, date;")
res <- dbSendQuery(channel, queryString)
todayForeIndexData <- fetch(res, n = -1) %>% as_tibble() %>% arrange(code, date)

# 判斷是否要更新資料
ix <- 1
for(ix in c(1:length(foreIndex_list))){
  
  # tej資料日期
  databaseDataDate <- foreIndex_table %>% filter(code == foreIndex_list[ix]) %>% pull(date) %>% max()
  
  # 爬蟲資料日期
  newDataDate <- todayForeIndexData %>% filter(code == foreIndex_list[ix]) %>% pull(date) %>% max()
  
  if(newDataDate > databaseDataDate){
    
    foreIndex_table <- foreIndex_table %>% 
      bind_rows(todayForeIndexData %>% 
                  filter(code == foreIndex_list[ix], date > databaseDataDate)) %>%
      arrange(code, date)
  }
}

########### 國內類股指數 ###########
index_List <- c("M1100","M1200", "M1300", "M1400", "M1500", "M1600" ,"M1700", "M1721" ,"M1722", "M1800", 
                "M1900", "M2000" ,"M2100", "M2200", "M2300", "M2324", "M2325", "M2326", "M2327", "M2328", 
                "M2329" , "M2330", "M2331", "M2500", "M2600", "M2700", "M2800", "M9700", "M9900")

index_name <- c("水泥類指數", "食品類指數", "塑膠類指數", "紡織纖維類指數", "電機機械類指數",      
                "電器電纜類指數", "化學生技醫療類指數", "化學類指數", "生技醫療類指數",       
                "玻璃陶瓷類指數", "造紙類指數", "鋼鐵類指數", "橡膠類指數", "汽車類指數", "電子類指數", "半導體類指數",       
                "電腦及週邊設備類指數", "光電類指數", "通信網路類指數", "電子零組件類指數", "電子通路類指數", "資訊服務類指數", 
                "其他電子類指數", "建材營造類指數", "航運類指數", "觀光類指數", "金融保險類指數","油電燃氣類指數", "其他類指數")

# 國內類股指數
queryString <- paste0("select code, date, close, trade_volume from stock_market.domestic_index_data where date >= ", dataStartDate, 
                      " and  code in (", paste0("'", paste0(index_List, collapse = "','"), "'"),") order by code, date;")
res <- dbSendQuery(channel, queryString)
index_table <- fetch(res, n = -1) %>% as_tibble() %>% arrange(code, date)

# 最新國內類股指數資料
queryString <- paste0("select code, date, close, trade_volume from stock_market.latest_domestic_index_data order by code, date;")
res <- dbSendQuery(channel, queryString)
indexData <- fetch(res, n = -1) %>% as_tibble() %>% arrange(code, date)

# 判斷是否需要併入今日國內類股指數資料
if(max(indexData$date) > max(index_table$date)){
  index_table <- index_table %>% bind_rows(indexData) %>% arrange(code, date)
}

############ 三大法人特徵資料 ###########
queryString <- paste0("select code, name, date, foreign_net_buy_share, trust_net_buy_share, dealer_net_buy_share", 
                      "from stock_market.counter_all_data where date >= ", dataStartDate, " order by code, date")
res <- dbSendQuery(channel, queryString)
stock_counter_feature_day <- fetch(res, n = -1) %>% as_tibble() %>% arrange(code, date)

queryString <- paste0("select code, name, date, foreign_BnS as foreign_net_buy_share, invtrust_BnS as trust_net_buy_share, ",
                      " dealers_BnS as dealer_net_buy_share ",
                      " from stock_market.counter_data_latest where date >= ", dataStartDate, " order by code, date")
res <- dbSendQuery(channel, queryString)
stock_counter_feature_latest <- fetch(res, n = -1) %>% as_tibble() %>% arrange(code, date)

# 整理今日資料以符合TEJ格式
stock_counter_feature_latest <- stock_counter_feature_latest %>%
  mutate(foreign_net_buy_share = round(foreign_net_buy_share/1000),
         trust_net_buy_share = round(trust_net_buy_share/1000),
         dealer_net_buy_share = round(dealer_net_buy_share/1000))

# 判斷是否需要併入今日三大法人資料
if(max(stock_counter_feature_latest$date) > max(stock_counter_feature_day$date)){
  stock_counter_feature_day <- stock_counter_feature_day %>% bind_rows(stock_counter_feature_latest)
}

########### 股票動能特徵資料 ###########
queryString <- paste0("select * from machine_learning_feature.stock_mom_feature_day where date >= ", dataStartDate, " order by code, date")
res <- dbSendQuery(channel, queryString)
stock_mom_feature_day <- fetch(res, n = -1) %>% as_tibble() %>% arrange(code, date)

# 不需併入最新資料 此資料表本身已有自動更新功能

########### 股票日頻技術分析指標資料 ###########
queryString <- paste0("select * from machine_learning_feature.stock_tech_indicator_feature_day where date >= ", dataStartDate, " order by code, date")
res <- dbSendQuery(channel, queryString)
stock_tech_indicator_feature_day <- fetch(res, n = -1) %>% as_tibble() %>% arrange(code, date)

# 不需併入最新資料 此資料表本身已有自動更新功能

########### 股票週頻技術分析指標資料 ###########
queryString <- paste0("select * from machine_learning_feature.stock_tech_indicator_feature_day_week where date >= ", dataStartDate, " order by code, date")
res <- dbSendQuery(channel, queryString)
stock_tech_indicator_feature_day_week <- fetch(res, n = -1) %>% as_tibble() %>% arrange(code, date)

# 不需併入最新資料 此資料表本身已有自動更新功能

# 結束資料庫連線
dbDisconnect(channel)

# 儲存資料
save(stock_table, stock_daytrade_list_table, foreIndex_table,
     index_table, stock_counter_feature_day, stock_mom_feature_day, stock_tech_indicator_feature_day,
     stock_tech_indicator_feature_day_week, file = "BacktestData0429.Rdata")

#################################### 變數資料處理 ####################################

# 讀取資料
load("BacktestData0429.Rdata")

min(stock_counter_feature_day$date)
min(stock_mom_feature_day$date)
min(stock_table$date)
min(stock_tech_indicator_feature_day$date)
min(stock_tech_indicator_feature_day_week$date)

#篩出50檔
stock_table <- stock_table %>% 
  filter(name %in% stockName_50)
# 交易日期
tradeDateData <- stock_table %>% 
  filter(date >=20140101,date<=20191231) %>% 
  distinct(date) %>% 
  mutate(nextDate = lead(date, 1),
         nextDate5 = lead(date,5),
         dayDif = ymd(nextDate5)-ymd(nextDate))

# 篩選出上市上櫃股票
stockList <- stock_table %>%
  select(code) %>%
  filter(nchar(code) == 4) %>%
  distinct(code) %>%
  pull(code)


# 計算考慮交易成本
buyCostR <- 0.001425 
sellCostR <- 0.004425

# 整理資料
stock_table <- stock_table %>%
  filter(code %in% stockList) %>%
  arrange(code, date) %>%
  group_by(code) %>%
  filter(n() > 5) %>%
  mutate(leadOpen = lead(open,1),
         leadClose = lead(close,1),
         leadclose5 = lead(close, 5),
         leadclose10 = lead(close, 10),
         leadMinLow5 = rollapplyr(low, 5, FUN = "min",partial = TRUE),
         leadMinLow5 = lead(leadMinLow5, 5),
         nextDate = lead(date,1),
         nextDate5 = lead(date,5),
         ret = (leadClose*(1-sellCostR)/leadOpen*(1+buyCostR))-1,
         ret5 = (leadclose5*(1-sellCostR)/leadOpen*(1+buyCostR))-1,
         ret10 = (leadclose10*(1-sellCostR)/leadOpen*(1+buyCostR))-1,
         checkRetOpen = rollapplyr(abs((lead(open)/open)-1), 5, FUN = "max",partial = T),
         checkRetOpen = lead(checkRetOpen, 5),
         checkRetLow = rollapplyr(abs((lead(low)/low)-1), 5, FUN = "max",partial = T),
         checkRetLow = lead(checkRetLow, 5),
         volumeMa5 = SMA(trade_volume, 5),
         abnormal_volume_weekmonth = SMA(trade_volume, 5)/SMA(trade_volume, 20),
         abnormal_volume_weekyear = SMA(trade_volume, 5)/SMA(trade_volume, 252),
         abnormal_volume_monthyear = SMA(trade_volume, 20)/SMA(trade_volume, 252)) %>%          
  ungroup()

stock_table_filter <- stock_table

# 交易日期最小值、股票總數
tradeDateMin <- min(stock_table_filter$date)
tradeStockNum <- unique(stock_table_filter$code) 

# # 整理國內指數資料
# # 計算指數報酬率相關特徵
# indexRet <- index_table %>%
#   select(date, group = code, value = close) 
# 
# indexRet <- indexRet %>%
#   group_by(group) %>%
#   arrange(group,date) %>%
#   mutate(ret = log(value/lag(value,1))) %>%
#   select(-value) %>%
#   spread(key = group, value = ret) %>%
#   left_join(Rolling_fun(indexRet, days_num = 3), by = c("date" = "date")) %>%
#   left_join(Rolling_fun(indexRet, days_num = 5), by = c("date" = "date")) %>%
#   left_join(MA_fun(indexRet), by = c("date" = "date"))
# 
# # 計算指數成交量相關特徵
# indexVol <- index_table %>%
#   select(date, group = code, value = trade_volume) %>%
#   mutate(group = paste0(group,"_vol"))
# 
# indexVol <- indexVol %>%
#   group_by(group) %>%
#   arrange(group,date) %>%
#   mutate(ret = log(value/lag(value,1))) %>%
#   select(-value) %>%
#   spread(key = group, value = ret) %>%
#   left_join(Rolling_fun(indexVol, days_num = 3), by = c("date" = "date")) %>%
#   left_join(Rolling_fun(indexVol, days_num = 5), by = c("date" = "date")) %>%
#   left_join(MA_fun(indexVol), by = c("date"="date"))
# 
# # 整併指數報酬率與指數成交量變動率
# index_table <- indexRet %>% 
#   left_join(indexVol, by = c("date" = "date")) %>% 
#   arrange(date)
# 
# # 整理國外指數資料
# foreIndex_table <- foreIndex_table %>%
#   spread(key = code, value = index) %>% 
#   gather(group, value, SB01, SB03, SB04, SB05, SB66)
# 
# foreIndex_table <- foreIndex_table %>%
  # group_by(group) %>%
  # arrange(date) %>%
  # fill(value, .direction = "down")  # 若有缺失值則往前補足
# 
# foreIndex_table <- foreIndex_table %>%
#   group_by(group) %>%
#   arrange(group, date) %>%
#   mutate(ret = log(value/lag(value,1))) %>%
#   select(-value) %>%
#   spread(key = group, value = ret) %>%
#   left_join(Rolling_fun(foreIndex_table, days_num = 5), by = c("date" = "date")) %>%
#   left_join(Rolling_fun(foreIndex_table, days_num = 20), by = c("date" = "date")) %>%
#   left_join(MA_fun(foreIndex_table), by = c("date" = "date"))

# 整理籌碼面資料特徵

stock_volume <- stock_table %>% select(code,date,trade_volume)
stock_counter_feature_day <- stock_counter_feature_day %>% 
  # select(-name) %>%
  left_join(stock_volume, by = c("code" = "code", "date" = "date"))
colna <- colnames(stock_counter_feature_day %>% select(-code,-date,-name))
stock_counter_feature_day <-  stock_counter_feature_day %>%
  group_by(code) %>%
  arrange(date) %>%
  fill(colna, .direction = "down")

stock_counter_feature_day2 <- stock_counter_feature_day %>%
  group_by(code) %>%
  arrange(code,date) %>%
  mutate(foreign_share_volume = foreign_net_buy_share/trade_volume,
         trust_share_volume = trust_net_buy_share/trade_volume,
         dealer_share_volume = dealer_net_buy_share/trade_volume,
         volume_MA3 = rollapplyr(trade_volume, 3, FUN = "sum", fill = NA),
         volume_MA5 = rollapplyr(trade_volume, 5, FUN = "sum", fill = NA),
         volume_MA20 = rollapplyr(trade_volume, 20, FUN = "sum", fill = NA),
         foreign_volume_MA3 = rollapplyr(foreign_net_buy_share, 3, FUN = "sum", fill = NA),
         foreign_volume_MA5 = rollapplyr(foreign_net_buy_share, 5, FUN = "sum", fill = NA),
         foreign_volume_MA20 = rollapplyr(foreign_net_buy_share, 20, FUN = "sum", fill = NA),
         trust_volume_MA3 = rollapplyr(trust_net_buy_share, 3, FUN = "sum", fill = NA),
         trust_volume_MA5 = rollapplyr(trust_net_buy_share, 5, FUN = "sum", fill = NA),
         trust_volume_MA20 = rollapplyr(trust_net_buy_share, 20, FUN = "sum", fill = NA),
         dealer_volume_MA3 = rollapplyr(dealer_net_buy_share, 3, FUN = "sum", fill = NA),
         dealer_volume_MA5 = rollapplyr(dealer_net_buy_share, 5, FUN = "sum", fill = NA),
         dealer_volume_MA20 = rollapplyr(dealer_net_buy_share, 20, FUN = "sum", fill = NA),
         foreign_share_volume_3 = foreign_volume_MA3/volume_MA3,
         foreign_share_volume_5 = foreign_volume_MA5/volume_MA5,
         foreign_share_volume_20 = foreign_volume_MA20/volume_MA20,
         trust_share_volume_3 = trust_volume_MA3/volume_MA3,
         trust_share_volume_5 = trust_volume_MA5/volume_MA5,
         trust_share_volume_20 = trust_volume_MA20/volume_MA20,
         dealer_share_volume_3 = dealer_volume_MA3/volume_MA3,
         dealer_share_volume_5 = dealer_volume_MA5/volume_MA5,
         dealer_share_volume_20 = dealer_volume_MA20/volume_MA20,
         foreign_ma3_ma5 = foreign_share_volume_3-foreign_share_volume_5,
         foreign_ma3_ma20 = foreign_share_volume_3-foreign_share_volume_20,
         foreign_ma5_ma20 = foreign_share_volume_5-foreign_share_volume_20,
         trust_ma3_ma5 = trust_share_volume_3-trust_share_volume_5,
         trust_ma3_ma20 = trust_share_volume_3-trust_share_volume_20,
         trust_ma5_ma20 = trust_share_volume_5-trust_share_volume_20,
         dealer_ma3_ma5 = dealer_share_volume_3-dealer_share_volume_5,
         dealer_ma3_ma20 = dealer_share_volume_3-dealer_share_volume_20,
         dealer_ma5_ma20 = dealer_share_volume_5-dealer_share_volume_20) %>%
  select(-(foreign_net_buy_share:trade_volume)) %>%
  select(-(foreign_volume_MA3:dealer_volume_MA20))

# save(stock_counter_feature_day,file = "三大法人0519.RDATA")
# # 新增股價資料的特徵
# stockTableFeature <- stock_table %>%
#   select(code:close) %>%
#   arrange(code, date) %>%
#   group_by(code) %>%
#   filter(n() > 60) %>%
#   mutate(lowMin5 = lag(rollapplyr(low, 5, FUN = "min", fill = NA),1),        # 前5天的最低價
#          lowMin20 = lag(rollapplyr(low, 20, FUN = "min", fill = NA),1),      # 前20天的最低價
#          lowMin60 = lag(rollapplyr(low, 60, FUN = "min", fill = NA),1),      # 前60天的最低價
#          lowBreak5 = low/lowMin5,                                            # 今日最低價與前5天最低價比率
#          lowBreak20 = low/lowMin20,                                          # 今日最低價與前20天最低價比率
#          lowBreak60 = low/lowMin60,                                          # 今日最低價與前60天最低價比率
#          closeBreak5 = close/lowMin5,                                        # 今日收盤價與前5天最低價比率
#          closeBreak20 = close/lowMin20,                                      # 今日收盤價與前20天最低價比率
#          closeBreak60 = close/lowMin60                                       # 今日收盤價與前60天最低價比率
#   )


# 篩選進行訓練的標的
# 排除因股價減半或股票回購的不合理樣本
stock_table_filter <- stock_table %>%
  filter(volumeMa5 > 100) %>%                                                # 流動性篩選五日均量大於100張
  select(code,name,date,nextDate,nextDate5,abnormal_volume_weekmonth:abnormal_volume_monthyear,Yret = ret5)


# 整併特徵資料
data <- stock_table_filter %>% filter(date >=20140101,date<=20191231)
data
# 由於記憶體限制 以下採用逐一併表 並刪除變數以釋放記憶體
# data <- data %>% left_join(index_table, by = c("date" = "date"))
rm(index_table)

# data <- data %>% left_join(foreIndex_table, by = c("date" = "date"))
rm(foreIndex_table)

# data <- data %>% left_join(stock_mom_feature_day, by = c("date" = "date", "code" = "code"))
rm(stock_mom_feature_day)

data <- data %>% left_join(stock_tech_indicator_feature_day %>% select(code,date,rsi,rsi_slope), by = c("date" = "date", "code" = "code"))
# rm(stock_tech_indicator_feature_day)

# data <- data %>% left_join(stock_tech_indicator_feature_day_week, by = c("date" = "date", "code" = "code"))
rm(stock_tech_indicator_feature_day_week)

data <- data %>% left_join(stock_counter_feature_day, by = c("date" = "date","code"="code","name"="name"))
# rm(stock_counter_feature_day)

# data <- data %>% left_join(stockTableFeature, by = c("date" = "date","code" = "code", "name" = "name"))
# rm(stockTableFeature)
#加籌碼
data <- data %>% left_join(stock_counter_day, by = c("date" = "date","code"="code"))
#加大盤
data <- data %>% left_join(Y9999 %>% select(-c("證券代碼","簡稱","收盤價(元)")), by = c("date" = "年月日"))
#加情緒
data <- data %>% left_join(stock50, by = c("date" = "date","name"="name"))


# 刪除特徵值為NA樣本
featureData <- data %>% ungroup() %>% select(-c(code:Yret))
naSite <- unique(which(is.na(featureData), arr.ind = T)[, 1])
if(length(naSite) > 0){
  data <- data[-naSite, ]
}

# 刪除預測目標為NA樣本(此處有保留最新資料)
data <- data %>% filter(!(date != max(tradeDateData$date) & is.na(Yret)))

# 刪除預測目標為NA樣本的資料
save(data, file = "data_filter.Rdata")

#################################### XGBoost機器學習模型 ####################################

load("data_filter.Rdata")

# 設定訓練的變數
retLimit <- c(0.05)  # 刪除雜訊的報酬率臨界值

# 本次程式模型預測期間
modelTestDateList <- tibble(testStartDate = lapply(c(2015:(year(ymd(20191231))+1)), 
                                                   function(x) x*10000+c(0101)) %>% unlist() %>% ymd() %>% sort()) %>%
  mutate(testEndDate = lead(testStartDate, 1),
         testStartDate = gsub("-", "", testStartDate) %>% as.numeric(),
         testEndDate = gsub("-", "", testEndDate) %>% as.numeric()) %>%
  na.omit() %>%
  filter(testStartDate <= gsub("-", "", 20191231))

test_start_day <- modelTestDateList$testStartDate    # 預測期起始日
test_end_day  <- modelTestDateList$testEndDate       # 預測期結束日

refreshAllData <- T
# 依更新需求整理相關資料
if(refreshAllData == T){
  
  # 建立儲存表
  trade_table_train <- NULL
  trade_table_test  <- NULL
  trade_table_pred_ret_train <- NULL
  trade_table_pred_ret_test <- NULL
  trade_table_train_importance <- NULL
  trade_table_pred_ret_train_importance <- NULL 
  
  # 需要更新
  needRefresh <- T
  
  # 迴圈起始位置
  startSite <- 1
  
}else{
  
  # 讀取儲存表
  load("modelPredictResult.Rdata")
  
  # 迴圈起始位置
  if(max(data$date) > max(trade_table_test$date)){
    
    # 需要更新
    needRefresh <- T
    
    # 迴圈起始位置
    startSite <- last(which(test_start_day <= max(data$date)))
    
  }else{
    
    # 不需要更新
    needRefresh <- F
  }
}
needRefresh <- T
ptm <- Sys.time()
if(needRefresh == F){
  
  cat(paste0("今日已為最新資料 不需要更新!\n"))
  
}else{

  # 開始跑XGBoost模型
  i <- startSite
  for(i in startSite:length(test_end_day)){
    
    trainStartDate <- 20140101               # 訓練期資料起始日
    # trainStartDate <- test_start_day[i]-10000  # 訓練期資料起始日
    testStartDate <- test_start_day[i]         # 預測期資料起始日
    testEndDate <- test_end_day[i]             # 預測期資料結束日
    
    cat(paste0("本次迴圈預測期間: ", testStartDate, " 至 ", testEndDate," \n"))
    
    # 建立預測模型數量
    predictModelNums <- 11
    
    # 本期模型資料
    modelData <- data %>% ungroup() %>% filter(date >= trainStartDate, date < testEndDate)
    
    # 處理特徵為無限值及缺值問題 => 直接刪除樣本
    modelDataMatrix <- modelData %>% select(-c(code:Yret)) %>% as.matrix()
    infSampleSite <- which(is.infinite(modelDataMatrix), arr.ind = T)[, 1]
    naSampleSite <- which(is.na(modelDataMatrix), arr.ind = T)[, 1]
    removeSite <- c(infSampleSite, naSampleSite) %>% unique() %>% sort()
    if(length(removeSite) > 0){
      modelData <- modelData[-removeSite, ]
    }
    cat(paste0("本次訓練期共移除", length(removeSite),"個樣本\n"))
    
    # 模型訓練期與預測期資料
    modelTrainData <- modelData %>% filter(date >= trainStartDate, date < testStartDate)
    modelTestData <- modelData %>% filter(date >= testStartDate, date < testEndDate)
    
    # 判斷當前路徑是否有本期特徵值標準化過度參數值
    if(any(dir() %in% paste0("featureStandardizeParaTable_", testStartDate, "_testEndDate_", testEndDate, ".Rdata"))){
      
      load(paste0("featureStandardizeParaTable_", testStartDate, "_testEndDate_", testEndDate, ".Rdata"))
      
    }else{
      
      # 進行特徵標準化 首先整理各特徵標準化過度參數值
      featureStandardizeParaTable <- NULL  # 儲存表
      
      startColumn <- which(colnames(modelTrainData) == "Yret")+1
      iy <- startColumn
      for(iy in c(startColumn:ncol(modelTrainData))){
        
        trainFeature <- modelTrainData %>% pull(iy)  # 取出訓練期特徵資料
        trainFeatureMean <- mean(trainFeature)       # 計算特徵平均值(儲存)
        trainFeatureSd <- sd(trainFeature)           # 計算特徵標準差(儲存)
        trainFeature <- (trainFeature-trainFeatureMean)/trainFeatureSd  # 訓練期特徵Z分數
        
        # 處理極值問題
        q1Value <- quantile(trainFeature, probs = 0.25, na.rm = T)
        q3Value <- quantile(trainFeature, probs = 0.75, na.rm = T)
        upperLimitValue <- q3Value + (q3Value-q1Value)*2            # 特徵上限值(儲存)
        lowerLimitValue <- q1Value - (q3Value-q1Value)*2            # 特徵下限值(儲存)
        trainFeature[which(trainFeature > upperLimitValue)] <- upperLimitValue
        trainFeature[which(trainFeature < lowerLimitValue)] <- lowerLimitValue
        
        # 標準化至[0,1]區間
        trainFeatureMax <- max(trainFeature)   # 特徵最大值(儲存)
        trainFeatureMin <- min(trainFeature)   # 特徵最小值(儲存)
        
        # 儲存過度值
        featureStandardizeParaTable <- featureStandardizeParaTable %>%
          bind_rows(tibble(featureName = colnames(modelTrainData)[iy], trainFeatureMean = trainFeatureMean,
                           trainFeatureSd = trainFeatureSd, upperLimitValue = upperLimitValue, 
                           lowerLimitValue = lowerLimitValue, trainFeatureMax = trainFeatureMax, trainFeatureMin = trainFeatureMin))
      }
      
      # 儲存特徵資料標準化過度參數值
      save(featureStandardizeParaTable, file = paste0("featureStandardizeParaTable_", testStartDate, "_testEndDate_", testEndDate, ".Rdata"))
    }
    
    # 進行特徵標準化
    startColumn <- which(colnames(modelTrainData) == "Yret")+1
    iy <- startColumn
    for(iy in c(startColumn:ncol(modelTrainData))){
      
      # 取出訓練期及測試期特徵資料
      trainFeature <- modelTrainData %>% pull(iy)  
      testFeature <- modelTestData %>% pull(iy)
      
      # 取出特徵資料標準化過度參數值
      featurePara <- featureStandardizeParaTable %>% filter(featureName == colnames(modelTrainData)[iy])
      trainFeatureMean <- featurePara %>% pull(trainFeatureMean)
      trainFeatureSd <- featurePara %>% pull(trainFeatureSd)
      upperLimitValue <- featurePara %>% pull(upperLimitValue)
      lowerLimitValue <- featurePara %>% pull(lowerLimitValue)
      trainFeatureMax <- featurePara %>% pull(trainFeatureMax)
      trainFeatureMin <- featurePara %>% pull(trainFeatureMin)
      
      # Z分數
      trainFeature <- (trainFeature-trainFeatureMean)/trainFeatureSd
      testFeature <- (testFeature-trainFeatureMean)/trainFeatureSd
      
      # 處理離群值
      trainFeature[which(trainFeature > upperLimitValue)] <- upperLimitValue
      trainFeature[which(trainFeature < lowerLimitValue)] <- lowerLimitValue
      testFeature[which(testFeature > upperLimitValue)] <- upperLimitValue
      testFeature[which(testFeature < lowerLimitValue)] <- lowerLimitValue
      
      # 標準化至[0,1]之間
      trainFeature <- (trainFeature - min(trainFeatureMin))/(trainFeatureMax - trainFeatureMin)
      testFeature <- (testFeature - min(trainFeatureMin))/(trainFeatureMax - trainFeatureMin)
      testFeature[which(testFeature > 1)] <- 1
      testFeature[which(testFeature < 0)] <- 0
      
      # 將整理好特徵取代回原資料
      modelTrainData[, iy] <- trainFeature
      modelTestData[, iy] <- testFeature
    }
    
    # # 繪圖觀察特徵資料分配
    # iy <- startColumn
    # plotData <- modelTrainData
    # plotData <- modelTestData
    # for(iy in c(startColumn:ncol(plotData))){
    #   hist(plotData %>% pull(iy), main = colnames(plotData)[iy])
    # }
    
    ########################################### 建立預測分類漲跌之XGBoost模型 ###########################################
    
    # # 訓練期資料
    # train_all <- modelTrainData %>% 
    #   # filter((Yret < (-retLimit))|(Yret > retLimit)) %>% 
    #   mutate(up_down_label = ifelse(Yret < (-retLimit), 1, 0)) %>%
    #   select(code:Yret, up_down_label, everything())
    # # 資料篩選: 清除介於 -retLimit~retLimit值間的雜訊 並將當沖報酬依據上漲為1 下跌為0做up_down_label的標籤
    # 
    # # 測試集資料
    # test_all <- modelTestData %>% 
    #   mutate(up_down_label = ifelse(Yret < (-retLimit), 1, 0)) %>%
    #   select(code:Yret, up_down_label, everything())
    # 
    # ############ XGBoost模型訓練 ############
    # trainPredictTable <- testPredictTable <- NULL
    # model_num <- 1
    # 
    # for(model_num in c(1:predictModelNums)){
    #   
    #   # 將訓練及資料打亂並取8成出來
    #   train_random <- train_all[sample(nrow(train_all)),]
    #   train_random <- train_random[sample(nrow(train_random)*0.8),]
    #   
    #   # 訓練期與測試期特徵
    #   train_data <- train_random %>% select(-c(code:up_down_label)) %>% data.matrix()
    #   test_data  <- test_all %>% select(-c(code:up_down_label)) %>% data.matrix()
    #   
    #   # 整理XGBoost資料
    #   train_label <- train_random %>% select(up_down_label) %>% pull()
    #   train_matrix <- xgb.DMatrix(data = train_data, label = train_label)
    #   train_list <- train_random %>% select(code:up_down_label)
    #   
    #   test_label <- test_all %>% select(up_down_label) %>% pull()
    #   test_matrix <- xgb.DMatrix(data = test_data, label = test_label)
    #   test_list <- test_all %>% select(code:up_down_label)
    #   
    #   save(train_list,test_list,file = paste0("train_test_",testEndDate,"_model_num",model_num,".Rdata"))
    #   
    #   # 判斷當前路徑是否已有訓練好的模型 若有則直接讀取 若沒有則建立模型
    #   if(!any(dir() %in% paste0("xgboost_class_testStartDate_", testStartDate, "_testEndDate_", testEndDate, "_modelNums_", model_num, ".model"))){
    #     
    #     ############ XGBoost交叉驗證 ############
    #     # 判斷是否有要做交叉驗證
    #     if(!any(dir() %in% paste0("cvOutput_class_testStartDate_", testStartDate, "_testEndDate_", testEndDate, ".Rdata"))){
    #       
    #       # 交叉驗證參數表
    #       paramTable <- expand.grid(eta = c(0.1), max_depth = c(7, 9, 11, 13, 15), subsample = c(0.9), colsample_bytree = c(0.9))
    #       
    #       # 進行交叉驗證
    #       iy <- 1
    #       cvOutput <- NULL
    #       for(iy in c(1:nrow(paramTable))){
    #         
    #         cat(paste0("正在交叉驗證預測期間: ", testStartDate, " 至 ", testEndDate," 第", iy, "個參數組合，進度:", iy, " / ", nrow(paramTable),"\n"))
    #         
    #         params <- list(booster = "gbtree",
    #                        eta = paramTable$eta[iy],
    #                        max_depth = paramTable$max_depth[iy],
    #                        subsample = paramTable$subsample[iy],
    #                        colsample_bytree = paramTable$colsample_bytree[iy],
    #                        tree_method = "gpu_hist",
    #                        eval_metric = "error",
    #                        objective = "binary:logistic",
    #                        scale_pos_weight = round((1-mean(train_random$up_down_label))/(mean(train_random$up_down_label)),2))
    #         
    #         cvResult <- xgb.cv(params = params, data = train_matrix, nrounds = 10000, 
    #                            nfold = 5, early_stopping_rounds = 20, verbose = F)
    #         
    #         cvOutput <- cvOutput %>%
    #           bind_rows(tibble(paramsNum = iy,
    #                            eta = paramTable$eta[iy],
    #                            max_depth = paramTable$max_depth[iy],
    #                            subsample = paramTable$subsample[iy],
    #                            colsample_bytree = paramTable$colsample_bytree[iy],
    #                            bestIteration = cvResult$best_iteration,
    #                            bestCvlogloss = cvResult$evaluation_log$test_error_mean[bestIteration]))
    #         
    #         gc()
    #       }
    #       
    #       # 儲存最佳參數
    #       save(cvOutput, file = paste0("cvOutput_class_testStartDate_", testStartDate, "_testEndDate_", testEndDate, ".Rdata"))
    #     }
    #     
    #     # 讀取最佳參數
    #     load(paste0("cvOutput_class_testStartDate_", testStartDate, "_testEndDate_", testEndDate, ".Rdata"))
    #     bestCvSite <- which(cvOutput$bestCvlogloss == min(cvOutput$bestCvlogloss))
    #     bestParamsNum <- cvOutput$paramsNum[bestCvSite]
    #     bestIteration <- cvOutput$bestIteration[bestCvSite]
    #     
    #     # 最佳參數
    #     params <- list(booster = "gbtree",
    #                    eta = cvOutput$eta[bestParamsNum], 
    #                    max_depth = cvOutput$max_depth[bestParamsNum], 
    #                    subsample = cvOutput$subsample[bestParamsNum], 
    #                    colsample_bytree = cvOutput$colsample_bytree[bestParamsNum],
    #                    tree_method = "gpu_hist",
    #                    eval_metric = "error",
    #                    objective = "binary:logistic",
    #                    scale_pos_weight = round((1-mean(train_random$up_down_label))/(mean(train_random$up_down_label)),2))
    #     
    #     # 訓練XGBoost模型
    #     xgbModel <- xgb.train(data = train_matrix, params = params, nrounds = bestIteration, verbose = T,maximize = F)
    #     
    #     # 儲存模型
    #     xgb.save(xgbModel, paste0("xgboost_class_testStartDate_", testStartDate, "_testEndDate_", testEndDate, "_modelNums_", model_num, ".model"))
    #     
    #     gc()
    #   }
    #   
    #   # 讀取模型
    #   xgbModel <- xgb.load(paste0("xgboost_class_testStartDate_", testStartDate, "_testEndDate_", testEndDate, "_modelNums_", model_num, ".model"))
    #   
    #   # 進行模型預測並儲存
    #   # trainPredictTable <- predict(xgbModel, train_data) %>% as_tibble() %>% bind_cols(trainPredictTable)
    #   # 以防測試集沒資料會報錯
    #   if(nrow(test_data)!=0){
    #     
    #     testPredictTable <- predict(xgbModel, test_data) %>% as_tibble() %>% bind_cols(testPredictTable)
    #   }
      
      # # 儲存重要特徵資訊
      # featureNames <- test_data %>% colnames()
      # importance_matrix <- xgb.importance(feature_names = featureNames, model = xgbModel)
      # importance_matrix <- importance_matrix[1:20,] %>% as.data.frame() # 將重要特徵函數取前20個Gain值最大的變數
      # trade_table_train_importance <- trade_table_train_importance %>% bind_rows(importance_matrix)
    # }
    
    
    # if(nrow(test_data)!=0){
    #   
    #   # 合併11次模型結果 並刪去最大最小值後取平均
    #   # testPredictTable <- testPredictTable %>% as.matrix() %>% apply(1, MeanPredict)
    #   testPredictTable <- bind_cols(test_list, testPredictTable)
    #   
    #   # 若為部份更新 刪除舊資料放入新資料
    #   if(refreshAllData == F){
    #     
    #     trade_table_test <- trade_table_test %>% filter(date < min(testPredictTable$date))
    #   }
    #   
    #   trade_table_test <- bind_rows(testPredictTable, trade_table_test)
    # }
    # 
    ########################################### 建立預測報酬率之XGBoost模型 ###########################################
    # 訓練期資料
    train_all <- modelTrainData %>% 
      # filter((Yret < (-retLimit))|(Yret > retLimit)) %>%
      select(code:Yret, everything())
    
    # 整理測試集資料
    test_all <- modelTestData %>% select(code:Yret, everything())
    
    ############ XGBoost模型訓練 ############
    trainPredictTable <- testPredictTable <- NULL
    model_num <- 1
    for(model_num in c(1:predictModelNums)){
      
      # 將訓練期資料打亂再取8成資料
      train_random <- train_all[sample(nrow(train_all)),]
      train_random <- train_random[sample(nrow(train_random)*0.8),]
      
      # 訓練期與測試期特徵
      train_data <- train_random %>% select(-c(code:Yret)) %>% data.matrix()
      test_data  <- test_all %>% select(-c(code:Yret)) %>% data.matrix()
      
      # 整理XGBoost資料
      train_label <- train_random %>% select(Yret) %>% pull()
      train_matrix <- xgb.DMatrix(data = train_data, label = train_label)
      train_list <- train_random %>% select(code:Yret)
      
      test_label <- test_all %>% select(Yret) %>% pull()
      test_matrix <- xgb.DMatrix(data = test_data, label = test_label)
      test_list <- test_all %>% select(code:Yret)
      
      # 判斷當前路徑是否已有訓練好的模型 若有則直接讀取 若沒有則建立模型
      if(!any(dir() %in% paste0("xgboost_ret_testStartDate_", testStartDate, "_testEndDate_", testEndDate, "_modelNums_", model_num, ".model"))){
        
        ############ XGBoost交叉驗證 ############
        # 判斷是否有要做交叉驗證
        if(!any(dir() %in% paste0("cvOutput_ret_testStartDate_", testStartDate, "_testEndDate_", testEndDate, ".Rdata"))){
          
          # 交叉驗證參數表
          paramTable <- expand.grid(eta = c(0.1), max_depth = c(7, 9, 11, 13, 15), subsample = c(0.9), colsample_bytree = c(0.9))
          
          # 進行交叉驗證
          iy <- 1
          cvOutput <- NULL
          gc()
          for(iy in c(1:nrow(paramTable))){
            
            cat(paste0("正在交叉驗證預測期間: ", testStartDate, " 至 ", testEndDate," 第", iy, "個參數組合，進度:", iy, " / ", nrow(paramTable),"\n"))
            
            params <- list(booster = "gbtree",
                           eta = paramTable$eta[iy],
                           max_depth = paramTable$max_depth[iy],
                           subsample = paramTable$subsample[iy],
                           colsample_bytree = paramTable$colsample_bytree[iy],
                           tree_method = "gpu_hist",
                           eval_metric = "rmse",
                           objective = "reg:linear")
            
            cvResult <- xgb.cv(params = params, data = train_matrix, nrounds = 10000, 
                               nfold = 5, early_stopping_rounds = 20, verbose = T)
            
            cvOutput <- cvOutput %>%
              bind_rows(tibble(paramsNum = iy,
                               eta = paramTable$eta[iy],
                               max_depth = paramTable$max_depth[iy],
                               subsample = paramTable$subsample[iy],
                               colsample_bytree = paramTable$colsample_bytree[iy],
                               bestIteration = cvResult$best_iteration,
                               bestCvlogloss = cvResult$evaluation_log$test_rmse_mean[bestIteration]))
            
            gc()
          }
          
          # 儲存最佳參數
          save(cvOutput, file = paste0("cvOutput_ret_testStartDate_", testStartDate, "_testEndDate_", testEndDate, ".Rdata"))
        }
        
        # 讀取最佳參數
        load(paste0("cvOutput_ret_testStartDate_", testStartDate, "_testEndDate_", testEndDate, ".Rdata"))
        bestCvSite <- which(cvOutput$bestCvlogloss == min(cvOutput$bestCvlogloss))
        bestParamsNum <- cvOutput$paramsNum[bestCvSite]
        bestIteration <- cvOutput$bestIteration[bestCvSite]
        
        
        # 最佳參數
        params <- list(booster = "gbtree",
                       eta = cvOutput$eta[bestParamsNum], 
                       max_depth = cvOutput$max_depth[bestParamsNum], 
                       subsample = cvOutput$subsample[bestParamsNum], 
                       colsample_bytree = cvOutput$colsample_bytree[bestParamsNum],
                       tree_method = "gpu_hist",
                       eval_metric = "rmse",
                       objective = "reg:linear")
        
        # 訓練XGBoost模型
        xgbModel <- xgb.train(data = train_matrix, params = params, nrounds = bestIteration)
        
        # 儲存模型
        xgb.save(xgbModel, paste0("xgboost_ret_testStartDate_", testStartDate, "_testEndDate_", testEndDate, "_modelNums_", model_num, ".model"))
        
        gc()
      }
      
      # 讀取模型
      xgbModel <- xgb.load(paste0("xgboost_ret_testStartDate_", testStartDate, "_testEndDate_", testEndDate, "_modelNums_", model_num, ".model"))
      
      # 進行模型預測並儲存
      # trainPredictTable <- predict(xgbModel, train_data) %>% as_tibble() %>% bind_cols(trainPredictTable)
      
      if(nrow(test_data)!=0){
        testPredictTable <- predict(xgbModel, test_data) %>% as_tibble() %>% bind_cols(testPredictTable)
      }
      
      # # 儲存重要特徵資訊
      featureNames <- test_data %>% colnames()
      importance_matrix <- xgb.importance(feature_names = featureNames, model = xgbModel)
      importance_matrix <- importance_matrix[1:20,] %>% as.data.frame() # 將重要特徵函數取前20個Gain值最大的變數
      trade_table_pred_ret_train_importance <- trade_table_pred_ret_train_importance %>% bind_rows(importance_matrix)
    }
    
    if(nrow(test_data)!=0){
      
      # 合併11次模型結果 並刪去最大最小值後取平均
      testPredictTable <- testPredictTable %>% as.matrix() %>% apply(1, mean)
      testPredictTable <- bind_cols(test_list, tibble(predict = testPredictTable))
      
      # 若為部份更新 刪除舊資料放入新資料
      if(refreshAllData == F){
        
        trade_table_pred_ret_test <- trade_table_pred_ret_test %>% filter(date < min(testPredictTable$date))
      }
      
      trade_table_pred_ret_test <- bind_rows(testPredictTable, trade_table_pred_ret_test)
    }
  }  
  
  # 儲存預測結果
  save(trade_table_pred_ret_test,trade_table_pred_ret_train_importance,featureData,
       file = "modelPredictResult.Rdata")
  
  # # ############################################ 整理資料 ###########################################
  load("modelPredictResult.Rdata")
  # # 交易成本
  tradeCostR <- 0
  # ########################################### 績效分析 ###########################################
  # 
  # # 讀取預測結果
  # load("modelPredictResult.Rdata")
  # 
  # # 交易成本
  # tradeCostR <- 0
  # 
  # # 是否要預測先賣後買
  # predictNegative <- F
  # 
  # # 加入預測放空(先賣後買)資訊
  # if(predictNegative == T){
  #   
  #   trade_table_test <- trade_table_test %>% mutate(Yret = -Yret, predict = 1-predict)
  #   trade_table_pred_ret_test <- trade_table_pred_ret_test %>% mutate(Yret = -Yret, predict = -predict)
  # }
  # 
  # # 觀看模型學習狀況
  cutClassPredict <- seq(0.5, 0.9, 0.05)    # 預測分類機率值
  cutRetPredict <- seq(0, 0.03, 0.005)      # 預測報酬率值
  # 
  # ############ 觀察預測分類值學習狀況 ############
  # cutClassPerformanceTable <- NULL
  # # cutClassValue <- 0.9
  # for(cutClassValue in cutClassPredict){
  #   
  #   # 交易資料
  #   iPerformanceTable <- trade_table_test %>%
  #     filter(!is.na(nextDate)) %>%
  #     filter(predict > cutClassValue) %>%
  #     mutate(Yret = Yret-tradeCostR) %>%
  #     summarise(meanRet = mean(Yret),
  #               sdRet = sd(Yret),
  #               tradeNums = n(),
  #               winRatio = sum(as.numeric(Yret>0))/n()) %>%
  #     mutate(cutValue = cutClassValue)
  #   
  #   # 紀錄績效
  #   cutClassPerformanceTable <- cutClassPerformanceTable %>% bind_rows(iPerformanceTable)
  # }
  # 
  # 
  # # 繪製預測分類值學習圖形
  # PlotModelPerformance(cutClassPerformanceTable)
  # 
  # 
  # ############ 觀察預測報酬率值學習狀況 ############
  cutRetPerformanceTable <- NULL
  cutRetPerformanceTable_code <- NULL
  
  # cutRetValue <- 0.03
  for(cutRetValue in cutRetPredict){

    # 交易資料
    iPerformanceTable <- trade_table_pred_ret_test %>%
      # filter(!is.na(nextDate)) %>%
      filter(predict > cutRetValue) %>%
      mutate(Yret = Yret) %>%
      summarise(meanRet = mean(Yret),
                sdRet = sd(Yret),
                tradeNums = n(),
                winRatio = sum(as.numeric(Yret>0))/n()) %>%
      mutate(cutValue = cutRetValue)
    PerformanceTable_code <- trade_table_pred_ret_test %>% 
      filter(predict  > cutRetValue) %>% 
      # filter(!is.na(nextDate))  %>% 
      group_by(code,name) %>%
      mutate(Yret = Yret) %>%
      summarise(meanRet = mean(Yret),
                tradeNums = n(),
                winRatio = sum(as.numeric(Yret>0))/n())  %>% 
    mutate(cutValue = cutRetValue)

    # 紀錄績效
    cutRetPerformanceTable <- cutRetPerformanceTable %>% bind_rows(iPerformanceTable)
    cutRetPerformanceTable_code <- cutRetPerformanceTable_code %>% bind_rows(PerformanceTable_code)
  }

  # # 繪製預測分類值學習圖形
  PlotModelPerformance(cutRetPerformanceTable)
  # ############ 觀察特定參數組合下的各年月資訊 ############
  # cutClassValue <- 0.7
  cutRetValue <- 0.02
  # 
  # tradeDetailTable <- combineModelPredictTable %>%
  #   filter(predict > cutRetValue)

  strategyPerformanceTable <- trade_table_pred_ret_test  %>%
    filter(predict > cutRetValue) %>% 
    filter(!is.na(nextDate))  %>% 
    mutate(yearMonth = as.numeric(substring(date, 1, 6))) %>%
    group_by(yearMonth) %>%
    summarise(meanRet = mean(Yret),
              sdRet = sd(Yret,na.rm = T),
              tradeNums = n(),
              winRatio = sum(as.numeric(Yret>0))/n())
  
  Sys.time()-ptm
}


###新投資組合##########################

# trade_table_pred_ret_test <- trade_table_pred_ret_test %>%
#   select(everything(),ret =Yret ) %>%
#   left_join(data %>% select(code:date,Yret),by = c("code"="code","name"="name","date"="date"))
minDate<-ymd(min(trade_table_pred_ret_test$date))
maxDate<-ymd(max(trade_table_pred_ret_test$date))
weekDate<-tibble(date=seq(minDate,maxDate,1)) %>% 
  mutate(week=isoweek(date),
         wDay=lubridate::wday(date,
                   week_start = getOption("lubridate.week.start",1)),
         lagweek =lag(week),
         newWeek =ifelse(week != lagweek,1,0),
         newWeek =ifelse(is.na(newWeek),0,newWeek),
         trueWeek =cumsum(newWeek)+1,
         date=as.numeric(gsub("-","",date)))
trade_Data <- trade_table_pred_ret_test %>% 
  left_join(weekDate %>% select(date,week=trueWeek,wDay),
            by =c("date"="date"))# 

test_week_day <- trade_Data %>% 
  # mutate(year = as.numeric(substring(nextDate, 1, 4))) %>%
  group_by(code,name,week) %>% 
  dplyr::slice(nextDate,1) %>% arrange(code,date)

test_week_day %>% filter(code == 2330) %>% pull(date) -> indate
windowsFonts(A=windowsFont("微軟正黑體"))
# 最大回撤率
mdd <- function(netValue){
  mdd <- min(rev((cummin(rev(netValue))/rev(netValue)-1)))
  return(mdd)
}
portflioPerformanceTable_week_stocknum <- NULL
portflioPerformanceTable_week_all <- NULL

i <- 5
for (i in c(seq(5,50,5))) {
  portflioPerformanceTable_week <- trade_Data  %>%
    filter(nextDate %in% indate) %>% 
    # filter(predict > 0.02) %>%
    # filter(!is.na(nextDate))  %>% 
    mutate(year = as.numeric(substring(nextDate, 1, 4)),yearMonth =substring(date, 1, 6)) %>%
    group_by(nextDate,year) %>%
    top_n(n=i,wt = predict)%>% #挑10檔
    summarise(meanRet = mean(Yret),
              sdRet = sd(Yret,na.rm = T),
              tradeNums = n(),
              winRatio = sum(as.numeric(Yret>0))/n() )%>% 
    arrange(year) %>% 
    ungroup() %>% 
    mutate(cumRet = round(cumprod(meanRet+1)-1,4),
           money = 10000000*(1+cumRet))
  meanRet <- mean(portflioPerformanceTable_week$meanRet)
  cumRet <- round(cumprod(portflioPerformanceTable_week$meanRet+1)-1,4) %>% tail(1)
  annualRet <- (1+cumRet)^(52/nrow(portflioPerformanceTable_week))-1
  winRatio <- mean(portflioPerformanceTable_week$winRatio)
  maxDrawdown <- mdd(portflioPerformanceTable_week$money)
  sharpeRatio <- SharpeRatio.annualized(xts(portflioPerformanceTable_week %>% select(meanRet),order.by = ymd(portflioPerformanceTable_week$nextDate) )) %>% as.numeric()
  portflioPerformanceTable_week_num <- data.frame(stknum = i , meanRet = meanRet, cumRet = cumRet , annualRet = annualRet , sharpeRatio = sharpeRatio, winRatio =winRatio , maxDrawdown = maxDrawdown)
  portflioPerformanceTable_week_stocknum <- rbind(portflioPerformanceTable_week_stocknum,portflioPerformanceTable_week_num)
  portflioPerformanceTable_week <- portflioPerformanceTable_week %>% mutate(stocknum = i)
  portflioPerformanceTable_week_all <- rbind(portflioPerformanceTable_week_all,portflioPerformanceTable_week)
  
}
portflioPerformanceTable_week_all$stocknum <- as.factor(portflioPerformanceTable_week_all$stocknum)

ggplot(portflioPerformanceTable_week_all, aes(x=week, y=cumRet,group=stocknum,color=stocknum)) +
  geom_line()+
  theme_minimal()+
  theme(axis.text.y = element_text(angle = 0))+
  scale_x_continuous(breaks=seq(0,258,10))+
  labs(x ="週",y = "累積報酬",color="標的數")+
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 00,size=10))+
  ggtitle("投資人情緒原始策略累積報酬")


#IC圖
trade_Data_week <-  trade_Data  %>%
  filter(nextDate %in% indate)
w <- trade_Data_week %>% group_by(nextDate) %>% summarise(cor = cor(predict,Yret)) %>% ungroup() %>% mutate(cum =cumsum(cor))
ggplot(w, aes(x=1:nrow(w), y=cum,group=1)) +
  geom_line()+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0))+
  scale_x_continuous(breaks=seq(0,nrow(w),10))+
  xlab("week")+
  ggtitle("IC")

#######
portflioPerformanceTable_week_allS <- portflioPerformanceTable_week_all %>% mutate(type = "原始") 
portflioPerformanceTable_week_allN <- portflioPerformanceTable_week_all %>% mutate(type = '情緒')

portflioPerformanceTable_allA <- bind_rows(portflioPerformanceTable_week_allS,portflioPerformanceTable_week_allN)

#5
portflioPerformanceTable_allA %>% 
  filter(stocknum == 5) %>% 
  ggplot(aes(x=week, y=cumRet,group=type,color=type)) +
  geom_line()+
  theme_minimal()+
  scale_x_continuous(breaks=seq(0,258,10))+
  labs(x ="週",y = "累積報酬",color="策略")+
  theme(text = element_text(size=20,family = "A"),axis.text.x = element_text(angle = 00,size=10))+
  ggtitle("5檔累積報酬")

#10
portflioPerformanceTable_all %>% 
  filter(stocknum == 10) %>% 
  ggplot(aes(x=week, y=cumRet,group=type,color=type)) +
  geom_line()+
  theme_minimal()+
  scale_x_continuous(breaks=seq(0,258,10))+
  labs(x ="週",y = "累積報酬",color="策略")+
  theme(text = element_text(size=20,family = "A"),axis.text.x = element_text(angle = 00,size=10))+
  ggtitle("10檔累積報酬")


#15
portflioPerformanceTable_all %>% 
  filter(stocknum == 15) %>% 
  ggplot(aes(x=week, y=cumRet,group=type,color=type)) +
  geom_line()+
  theme_minimal()+
  scale_x_continuous(breaks=seq(0,258,10))+
  labs(x ="週",y = "累積報酬",color="策略")+
  theme(text = element_text(size=20,family = "A"),axis.text.x = element_text(angle = 00,size=10))+
  ggtitle("15檔累積報酬")
#20
portflioPerformanceTable_all %>% 
  filter(stocknum == 20) %>% 
  ggplot(aes(x=week, y=cumRet,group=type,color=type)) +
  geom_line()+
  theme_minimal()+
  scale_x_continuous(breaks=seq(0,258,10))+
  labs(x ="週",y = "累積報酬",color="策略")+
  theme(text = element_text(size=20,family = "A"),axis.text.x = element_text(angle = 00,size=10))+
  ggtitle("20檔累積報酬")


########年的部分

portflioPerformanceTable_year <- portflioPerformanceTable_week_all  %>%
  filter(stocknum == 5) %>% 
  group_by(year) %>%
  summarise(meanRet = mean(meanRet),
            cumRet = tail(cumRet,1)-head(cumRet,1),
            tradeNums = n(),
            annualRet = (1+cumRet)^(52/tradeNums)-1,
            winRatio = sum(as.numeric(meanRet>0))/n(),
            mdd = mdd(money))

portflioPerformanceTable_year2 <- trade_Data  %>%
  filter(nextDate %in% indate) %>% 
  # filter(predict > 0.02) %>%
  # filter(!is.na(nextDate))  %>% 
  mutate(year = as.numeric(substring(nextDate, 1, 4)),yearMonth =substring(date, 1, 6)) %>%
  group_by(week,year) %>%
  top_n(n=5,wt = predict)%>% #挑10檔
  ungroup() %>%
  group_by(year) %>% 
  summarise(meanRet = mean(Yret),
            sdRet = sd(Yret,na.rm = T)*sqrt(252/5),
            tradeNums = n(),
            winRatio = sum(as.numeric(Yret>0))/n() )


portflioPerformanceTable_year <- portflioPerformanceTable_year %>% 
  select(year,cumRet,annualRet) %>% 
  left_join(portflioPerformanceTable_year2 %>% select(year,sdRet,tradeNums,winRatio))

portflioPerformanceTable_year <- portflioPerformanceTable_year %>% mutate(sharpeRatio = annualRet/sdRet)
meanRet <- mean(portflioPerformanceTable_week$meanRet)
cumRet <- round(cumprod(portflioPerformanceTable_week$meanRet+1)-1,4) %>% tail(1)
annualRet <- (1+cumRet)^(52/nrow(portflioPerformanceTable_week))-1
sdRet <- sd(portflioPerformanceTable_week$meanRet)*sqrt(252/5)
sr <- annualRet/sdRet

winRatio <- mean(portflioPerformanceTable_week$winRatio)
maxDrawdown <- mdd(portflioPerformanceTable_week$money)
portflioPerformanceTable_week_num <- data.frame(stknum = i , meanRet = meanRet, cumRet = cumRet , annualRet = annualRet , winRatio =winRatio , maxDrawdown = maxDrawdown)
portflioPerformanceTable_week_stocknum <- rbind(portflioPerformanceTable_week_stocknum,portflioPerformanceTable_week_num)
portflioPerformanceTable_week <- portflioPerformanceTable_week %>% mutate(stocknum = i)
portflioPerformanceTable_week_all <- rbind(portflioPerformanceTable_week_all,portflioPerformanceTable_week)

