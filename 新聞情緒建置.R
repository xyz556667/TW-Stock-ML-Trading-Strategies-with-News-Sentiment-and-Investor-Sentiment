rm(list=ls());gc()
load("fasttextpredictData50.RDATA")
pacman::p_load(data.table,readr,stringr,tidytext,
               tidyverse,lubridate,scales,widyr)

#data為fasttextpredict2元根3元的預結果合併，已經儲存成fasttextpredictData50
data <- data %>% mutate(hhmm=as.numeric(substring(Newstime,9,12)),
                        TradeTime=ifelse((hhmm>=0)&(hhmm<900),"交易前",
                                         ifelse((hhmm>=900)&(hhmm<1300),"交易中","交易後")),
                        yyyymmdd=as.numeric(substring(as.character(Newstime),1,8)),
                        date=as.Date(ISOdate(year=substring(yyyymmdd,1,4),
                                             month=substring(yyyymmdd,5,6),
                                             day=substring(yyyymmdd,7,8)),format="%Y%m%d"),
                        weekday=weekdays(as.Date(as.character(yyyymmdd),format="%Y%m%d"))
) %>% arrange(Newstime)


#股價(未調整)中的標的
stockprice_read <- fread(file="stockprice20132019.txt",header = T,stringsAsFactors = F,sep="\t")
colnames(stockprice_read) <- c("code","name","industry","type","date","open","high","low","close","volume","mkt_value","PE","PB")
stockprice_read$company_name <- gsub("\\s+", "", stockprice_read$company_name)
stockprice_read$code <- as.numeric(stockprice_read$code)
stockprice_read$open <- as.numeric(stockprice_read$open)

#股票資料
ETF50 <- fread(file="0050成分.csv",header = T,stringsAsFactors = F,)
colnames(ETF50) <- c("code","company_name","weight","shares")
ETF50 <- ETF50 %>% arrange(desc(weight))
stockName_50 <- ETF50 %>% pull(company_name)
stockName_15 <- ETF50 %>%dplyr::slice(1:15) %>% pull(company_name)

stockdata <- stockprice_read %>% filter(name %in% stockName_50,date >=20140101)

# ix <- 1
# # iy <- 2
# for (ix in 1:15) {
#   stockdata_one <- stockdata %>% filter(name %in% stockName_50[ix])
#   indate <- unique(stockdata_one$date)
#   date <- unique(data$date)
#   newsData <- data %>%
#     filter(str_detect(company_name,stockName_50[ix]))
#   for (iy in 1:nrow(newsData)) {
#     if (newsData$hhmm[iy]<830) {
#       newsData$indate[iy] <- newsData$yyyymmdd[iy]
#       }else if(newsData$yyyymmdd == 20191231) {
#         break
#       }else{
#         newsData$indate[iy] <- indate[which(indate==newsData$yyyymmdd[iy])+1]
#       }
# 
#     }
#   }
# 
# }


newsData <- data
iy <- 1

#將8點前的新聞往前移，要跑很久可以直接load Rdata
for (iy in 1:nrow(newsData)) {
  if(newsData$weekday[iy] %in% c("星期二","星期三","星期四","星期五")){
    
    if(newsData$hhmm[iy]<0800){
      newsData$new_date[iy] <- (ymd(newsData$yyyymmdd[iy])-1) %>% as.character()
    }else{
      newsData$new_date[iy] <- ymd(newsData$yyyymmdd[iy]) %>% as.character()
    }
  }else if(newsData$weekday[iy] %in% c("星期一")){
    if(newsData$hhmm[iy]<0800){
      newsData$new_date[iy] <- (ymd(newsData$yyyymmdd[iy])-3) %>% as.character()
    }else{
      newsData$new_date[iy] <- ymd(newsData$yyyymmdd[iy]) %>% as.character()
    }
    
  }else if(newsData$weekday[iy] %in% "星期六"){
    newsData$new_date[iy] <- (ymd(newsData$yyyymmdd[iy])-1) %>% as.character()
  }else{
    newsData$new_date[iy] <- (ymd(newsData$yyyymmdd[iy])-2) %>% as.character()
  }
}

# save(newsData,file = "newsData50.RDATA")
load("newsData50.RDATA")
newsData <- newsData %>% 
  mutate(
    predict2 = if_else(predict2 == "正面",1,-1),
    predict3 = if_else(predict3 == "正面",1,
                       if_else(predict3 == "負面",-1,0))
  )

#算每一間每天的分數
stock50 <- NULL
ix <- 1
for (ix in 1:50) {
  cat(paste0("目前正在讀取第 ",ix," 家公司，進度: ",ix," / ",length(stockName_50),"\n"))
  stockdata_one <- stockdata %>% filter(name %in% stockName_50[ix])
  indate <- unique(stockdata_one$date)
  newsData_one <- newsData %>%
    filter(str_detect(company_name,stockName_50[ix]))
  newsData_one$new_date <- ymd(newsData_one$new_date) 
  stockdata_one$date <- ymd(stockdata_one$date) 
  # news15 <- newsData_one %>% full_join(stockdata_one %>% select(name,date),by = c("new_date" = "date"))
  news15 <- stockdata_one%>% select(name,date) %>% left_join(newsData_one ,by = c("date" = "new_date"))
  news_count <- news15 %>% group_by(name) %>% summarise(num=n())
  news_stock <- news15 %>% 
    group_by(name,date) %>% 
    summarise(
      n = n(),
      sentiment2 = sum(predict2),
      sentiment3 = sum(predict3),
      sentiment2_com = sum(predict2/company_num),
      sentiment3_com = sum(predict3/company_num),
      sentiment2_avg = mean(predict2),
      sentiment3_avg = mean(predict3)

    )
  stock50 <- rbind(stock50,news_stock)
}

# stock50 <- stock50 %>% mutate(weekday = weekdays(date))
stock50[is.na(stock50)] <- 0
stock50$date <- gsub("-","",stock50$date)
stock50$date <- as.numeric(stock50$date)

stock50 <- stock50 %>% 
  group_by(name) %>% 
  mutate(shock21=(sentiment2_avg-rollapply(lag(sentiment2_avg),width = 5,FUN = mean, fill = NA,align = "right")),
         shock22=rollapply(lag(sentiment2_avg),width = 5,FUN = sd, fill = NA,align = "right"),
         shock_score2=shock21/shock22,
         trend2=lag(sentiment2_avg)-lag(sentiment2_avg,2),
         trend_score2=rollapply(trend2,width = 4,FUN = sum, fill = NA,align = "right"),
         shock31=(sentiment3_avg-rollapply(lag(sentiment3_avg),width = 5,FUN = mean, fill = NA,align = "right")),
         shock32=rollapply(lag(sentiment3_avg),width = 5,FUN = sd, fill = NA,align = "right"),
         shock_score3=shock31/shock32,
         trend3=lag(sentiment3_avg)-lag(sentiment3_avg,2),
         trend_score3=rollapply(trend3,width = 4,FUN = sum, fill = NA,align = "right")
) %>% 
  select(-c(shock21,shock22,trend2,shock31,shock32,trend3))

colna <- colnames(stock50 %>% select(-date,-name))
stock50 <-  stock50 %>%
  group_by(name) %>%
  arrange(date) %>%
  fill(colna, .direction = "down")

#確認星期六
table(stock50$weekday)
stock50 %>% filter(weekday == "星期六") %>% pull(date) %>% unique() ->saturday
stockprice_read %>% mutate(Date = ymd(date)) %>% filter(Date %in% saturday)



