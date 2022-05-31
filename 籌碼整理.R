### 籌碼面整理####
rm(list = ls());gc()
library(dplyr)
library(data.table)

counter <-fread(file="counterother.txt",header = T,stringsAsFactors = F,data.table = F)
finance_short <- fread(file="financeshort.txt",header = T,stringsAsFactors = F ,data.table = F)
turnover <- fread(file="turnover.txt",header = T,stringsAsFactors = F ,data.table = F)

counter <- counter %>% filter(簡稱 %in% stockName_50)
finance_short <- finance_short %>% filter(簡稱 %in% stockName_50)
turnover <- turnover %>% filter(簡稱 %in% stockName_50)

counter$年月日 %>% unique() %>% length()
finance_short$年月日 %>% unique() %>% length()

stock_counter <- counter %>% 
  left_join(finance_short %>% select(-公司代碼),by = c("簡稱"="簡稱","年月日"="年月日")) %>% 
  left_join(turnover %>% select(-證券代碼),by = c("簡稱"="簡稱","年月日"="年月日"))

# save(stock_counter,file = "stock_counterNEW.RDATA")
# write.csv(stock_counter,"stock_counter.csv")
stock_counter <-fread(file="stock_counterNEW.csv",header = T,stringsAsFactors = F,data.table = F)
stock_counter_col <- colnames(stock_counter)
colnames(stock_counter) <- c("code","name","date","foreign_net_buy_day","trust_net_buy_day",
                             "dealer_net_buy_day","finance_short_net_buy_day","stock_net_buy_day",
                             "foreign_net_buy_Ret","trust_net_buy_Ret","dealer_net_buy_Ret",
                             "finance_short_net_buy_Ret","stock_net_buy_Ret","foreign_cum_net_buy_share",
                             "trust_cum_net_buy_share","dealer_cum_net_buy_share","finance_short_cum_net_buy_share",
                             "stock_cum_net_buy_share","finance_balance","finance_up","finance_down","finance_cash",
                             "finance_change","short_balance","short_up","short_down","short_cash","short_change",
                             "borrow_balance","borrow_short","borrow_treasury_change","real_turnover","short_margin",
                             "finance_usage","short_usage")
stock_counter_col <- colnames(stock_counter)
col_na <- c("foreign_net_buy_day","trust_net_buy_day",
            "dealer_net_buy_day","finance_short_net_buy_day","stock_net_buy_day",
            "foreign_net_buy_Ret","trust_net_buy_Ret","dealer_net_buy_Ret",
            "finance_short_net_buy_Ret","stock_net_buy_Ret")
stock_volume <- stock_table %>% select(code,date,trade_volume)
stock_counter <- stock_counter %>%
  mutate_at(stock_counter_col,as.numeric) %>% 
  select(-name)
stock_counter$code <- as.character(stock_counter$code)  
stock_counter_day <- stock_counter %>% 
  left_join(stock_volume, by = c("code" = "code", "date" = "date"))

stock_counter_day <-  stock_counter_day %>% 
  group_by(code) %>%
  arrange(date) %>%
  fill(col_na, .direction = "down")  # 若有缺失值則往前補足



stock_counter_day <- stock_counter_day %>%
  group_by(code) %>%
  arrange(code,date) %>%
  mutate(foreign_cum_net_buy_share_volume = foreign_cum_net_buy_share/trade_volume,
         trust_cum_net_buy_share_volume = trust_cum_net_buy_share/trade_volume,
         dealer_cum_net_buy_share_volume = dealer_cum_net_buy_share/trade_volume,
         finance_short_cum_net_buy_share_volume = finance_short_cum_net_buy_share/trade_volume,
         stock_cum_net_buy_share_volume = stock_cum_net_buy_share/trade_volume,
         finance_balance_volume = finance_balance/trade_volume,
         finance_up_volume = finance_up/trade_volume,
         finance_down_volume = finance_down/trade_volume,
         finance_cash_volume = finance_cash/trade_volume,
         finance_change_volume = finance_change/trade_volume,
         short_balance_volume = short_balance/trade_volume,
         short_up_volume = short_up/trade_volume,
         short_down_volume = short_down/trade_volume,
         short_cash_volume = short_cash/trade_volume,
         short_change_volume = short_change/trade_volume,
         borrow_balance_volume = borrow_balance/trade_volume,
         borrow_short_volume = borrow_short/trade_volume,
         borrow_treasury_change_volume = borrow_treasury_change/trade_volume,
         ) %>%
  select(-(finance_balance:borrow_treasury_change)) %>%
  select(-(foreign_cum_net_buy_share:stock_cum_net_buy_share)) %>% 
  select(-trade_volume)
# save(stock_counter_day,file = "其他籌碼.RDATA")


Y9999 <-fread(file="Y9999.txt",header = T,stringsAsFactors = F,data.table = F)

Y9999 <- Y9999[,c(1:3,7,9)] %>% 
  mutate(Y9999_turnover = `週轉率％`) %>% 
  select(-`週轉率％`) %>% 
  mutate(Y999ret_1 = log(`收盤價(元)`/lag(`收盤價(元)`,1))) %>% 
  mutate(Y999ret_5 = log(`收盤價(元)`/lag(`收盤價(元)`,5))) %>%
  mutate(Y999ret_10 = log(`收盤價(元)`/lag(`收盤價(元)`,10))) %>%
  mutate(Y999ret_22 = log(`收盤價(元)`/lag(`收盤價(元)`,22))) %>%
  mutate(Y999ret_60 = log(`收盤價(元)`/lag(`收盤價(元)`,60))) 
Y9999_in <- Y9999 %>% filter(年月日 %in% indate) %>% mutate(cumRet = round(cumprod(Y999ret_5+1)-1,4), money = 10000000*(1+cumRet) )
cumRet <- round(cumprod(Y9999_in$Y999ret_5+1)-1,4) %>% tail(1)
annualRet <- (1+cumRet)^(52/nrow(Y9999_in))-1
maxDrawdown <- mdd(Y9999_in$money)
Y9999_in_xts <- xts(Y9999_in$Y999ret_5,order.by = ymd(Y9999_in$年月日))
sharpe_ratio <- SharpeRatio.annualized(Y9999_in_xts)
Y9999 <- Y9999 %>% na.omit()
cumRet <- round(cumprod(Y9999$Y999ret_1+1)-1,4) %>% tail(1)
annualRet <- (1+cumRet)^(252/nrow(Y9999))-1
maxDrawdown <- mdd(Y9999_in$money)
winRatio = sum(as.numeric(Y9999$Y999ret_1>0))/nrow(Y9999)

Y9999_xts <- xts(Y9999$Y999ret_1,order.by = ymd(Y9999$年月日))
sharpe_ratio2 <- SharpeRatio.annualized(Y9999_xts)

E0050 <-fread(file="0050.txt",header = T,stringsAsFactors = F,data.table = F)
E0050_in <- E0050 %>%mutate(E0050ret_5ret_1 = log(`收盤價(元)`/lag(`收盤價(元)`,1))) %>% 
  mutate(E0050ret_5 = log(`收盤價(元)`/lag(`收盤價(元)`,5))) %>%
  mutate(E0050ret_10 = log(`收盤價(元)`/lag(`收盤價(元)`,10))) %>%
  mutate(E0050ret_22 = log(`收盤價(元)`/lag(`收盤價(元)`,22))) %>%
  mutate(E0050ret_60 = log(`收盤價(元)`/lag(`收盤價(元)`,60))) %>%  filter(年月日 %in% indate) %>%
  mutate(cumRet = round(cumprod(E0050ret_5+1)-1,4), money = 10000000*(1+cumRet) )

cumRet <- round(cumprod(E0050_in$E0050ret_5+1)-1,4) %>% tail(1)
annualRet <- (1+cumRet)^(52/nrow(E0050_in))-1
maxDrawdown <- mdd(E0050_in$money)

