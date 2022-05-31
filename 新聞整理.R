rm(list=ls());gc()
library(pacman)
p_load(data.table,readr,stringr,tidytext,tidyverse,lubridate,fastrtext)
options(scipen = 99)
#######讀取全部新聞##########
load("D:/Text/傳銘/論文/bert與報酬/news20200408.RDATA")
# csvname <- news5 %>% select(ID:text)
# write.csv(csvname,"news20200408_big5.csv",row.names = FALSE)
# csvname <- fread(file="news20200408_big5.csv",header = T,stringsAsFactors = F )
# csvname <- iconv(csvname,"big5", "UTF-8" )
newNews <- news5[!duplicated(news5$Header),]
newNews <- newNews[!duplicated(newNews$text),]
newNews <- newNews[!duplicated(newNews$Content),]
newNews <- newNews %>% mutate(wordcount = str_count(text))
newNews <- newNews %>% filter(company_name != "長")
mean(newNews$wordcount)
#######製作丟入BERT前資料#####

#2330標籤資料倂上
news_2330 <- fread(file="code2330_20200411_utf8.csv",header = T,stringsAsFactors = F,encoding = "UTF-8" )
news_2330 <- news_2330 %>% select(ID,sentiment)
newNews_2330 <- newNews %>% left_join(news_2330,by = c("ID"="ID"))
newNews_2330 <- newNews_2330 %>% mutate(wordsenti = ifelse(word_polarity>0,1,0))


#取出貼標資料
newNews_2330_labeled <- newNews_2330 %>% filter(sentiment != "") %>% mutate(Newstime=ymd_hm(Newstime),yyyy=year(Newstime)) 
#每年新聞數
newNews_2330_labeled %>%
  group_by(yyyy) %>% 
  summarise(count = n())

#2013
newNews_2330_labeled_2013 <- newNews_2330_labeled %>% filter(yyyy == 2013) %>% select(ID,comment=Content,sentiment)
# write.csv(newNews_2330_labeled_2013,"newNews_2330_labeled_2013_utf8.csv",row.names = FALSE,fileEncoding = "UTF-8")
#2014
newNews_2330_labeled_2014 <- newNews_2330_labeled %>% filter(yyyy == 2014) %>% select(ID,comment=Content,sentiment)
# write.csv(newNews_2330_labeled_2014,"newNews_2330_labeled_2014_utf8.csv",row.names = FALSE,fileEncoding = "UTF-8")
#2015
newNews_2330_labeled_2015 <- newNews_2330_labeled %>% filter(yyyy == 2015) %>% select(ID,comment=Content,sentiment)
# write.csv(newNews_2330_labeled_2015,"newNews_2330_labeled_2015_utf8.csv",row.names = FALSE,fileEncoding = "UTF-8")
#2016
newNews_2330_labeled_2016 <- newNews_2330_labeled %>% filter(yyyy == 2016) %>% select(ID,comment=Content,sentiment)
# write.csv(newNews_2330_labeled_2016,"newNews_2330_labeled_2016_utf8.csv",row.names = FALSE,fileEncoding = "UTF-8")
#2017
newNews_2330_labeled_2017 <- newNews_2330_labeled %>% filter(yyyy == 2017) %>% select(ID,comment=Content,sentiment)
# write.csv(newNews_2330_labeled_2017,"newNews_2330_labeled_2017_utf8.csv",row.names = FALSE,fileEncoding = "UTF-8")

######試試不要全部文章######
#2330標籤資料倂上
news_2330 <- fread(file="code2330_20200411_utf8.csv",header = T,stringsAsFactors = F,encoding = "UTF-8" )
news_2330 <- news_2330 %>% select(ID,comment,sentiment)
newNews_2330 <- newNews %>% left_join(news_2330,by = c("ID"="ID"))
#取出貼標資料
newNews_2330_labeled <- newNews_2330 %>% filter(sentiment != "") %>% mutate(Newstime=ymd_hm(Newstime),yyyy=year(Newstime)) 

#2016
newNews_2330_labeled_2016 <- newNews_2330_labeled %>% filter(yyyy == 2016) %>% select(ID,comment,sentiment)
write.csv(newNews_2330_labeled_2016,"newNews_2330_labeled_2016_utf802.csv",row.names = FALSE,fileEncoding = "UTF-8")
#2017
newNews_2330_labeled_2017 <- newNews_2330_labeled %>% filter(yyyy == 2017) %>% select(ID,comment,sentiment)
write.csv(newNews_2330_labeled_2017,"newNews_2330_labeled_2017_utf802.csv",row.names = FALSE,fileEncoding = "UTF-8")



########準備fasttext######
# 
newNews_JB <- fread(file="label_20200408_JB_big5.csv",header = T,stringsAsFactors = F )
newNews_JB <- newNews_JB %>% select(-Newstime) %>% left_join(news5 %>% select(ID,Newstime),by=c("ID"="ID"))
newNews_JB <- newNews_JB %>% mutate(yyyy = substring(Newstime,1,4) %>% as.numeric()) %>%  select(ID,Newstime,yyyy,everything())
# save(newNews_JB,file = "newNews_JB.RDATA")
# newNews_JB2 <- newNews_JB[!duplicated(newNews_JB$Header),]
# newNews <- newNews[!duplicated(newNews$text),]
# newNews <- newNews[!duplicated(newNews$Content),]

# newNews_JB_2330 <- newNews_JB %>% left_join(news_2330,by = c("ID"="ID"))
# 
# newNews_JB_2330_labeled <- newNews_JB_2330 %>% filter(sentiment != "") %>% mutate(Newstime=ymd_hm(Newstime),yyyy=year(Newstime),label=sentiment) 

# write.csv(newNews_JB_2330_labeled,"newNews_JB_2330_labeled_utf8.csv",row.names = FALSE,fileEncoding = "UTF-8")
newNews_JB_2330_labeled <- fread(file="label_2330_20200412_JBRANKGRAM_big5.csv",header = T,stringsAsFactors = F )

newNews_JB_2330_labeled <- newNews_JB_2330_labeled %>% 
  mutate(wordsenti = ifelse(word_polarity>0,1,0),
         label = ifelse(word_polarity>0 & sentiment >0,"正面",
                        ifelse(word_polarity>0 & sentiment == 0 ,"中立","負面")))

#####挑出15新聞
newNews_JB_15 <- newNews_JB %>% 
  filter(ID%in%news_15ID) %>% 
  mutate(yyyy=year(ymd_hm(Newstime)))
  
newNews_JB_15 %>%
  group_by(yyyy) %>% 
  summarise(count = n()/2)->yearNews
# save(yearNews,file = "每年標簽數.RData")  
newNews_JB_now <- newNews_JB %>% 
  filter(ID%in%newslabel_all2$ID) %>% 
  mutate(yyyy=year(ymd_hm(Newstime)))

newNews_JB_now %>%
  group_by(yyyy) %>% 
  summarise(count = n())->b

c <- cbind(yearNews,b %>% select(countNow=count))
c <- c %>% mutate(diff = count-countNow)

i <- 2013
new_news_ID <- NULL
for (i in 2013:2015) {
  year_news <- c %>% filter(yyyy == i) %>% pull(diff) %>% round()
  news_ID <- newNews_JB_15 %>% filter(yyyy == i) %>% pull(ID)
  news_ID <- sample(news_ID,year_news)
  new_news_ID <-c(new_news_ID,news_ID)
}

new_news_ID[-intersect(new_news_ID,newslabel_all2$ID)]->d

intersect(news_15ID,newslabel_all2$ID) %>% length()
news_need <- newNews_JB_15 %>% filter(ID%in%new_news_ID )

# write.csv(news_need,"news_need.csv")
#新的貼好
news_need <- fread(file="news_need0422.csv",header = T,stringsAsFactors = F )

news_need <- news_need %>% 
  mutate(label = ifelse(word_polarity>0 & label >0,"正面",
                        ifelse(word_polarity<=0 & label == 0 ,"負面","中立")))
table(news_need$label) %>% prop.table()

news_need <- news_need %>% select(ID,label)

news_label_now <- rbind(newslabel_all2,news_need)
news_label_now <- news_label_now[!duplicated(news_label_now$ID),]

news_ID <- sample(unique(news_label_now$ID),43557)

unique(news_ID) %>% length()

newNews_JB_now <- newNews_JB %>% 
  filter(ID%in%news_ID) %>% 
  mutate(yyyy=year(ymd_hm(Newstime)))

newNews_JB_now <- newNews_JB_now %>% left_join(news_label_now,by = c("ID"="ID"))
newNews_JB_now %>%
  group_by(yyyy) %>% 
  summarise(count = n())
table(newNews_JB_now$label) %>% prop.table()

# save(newNews_JB_now,file = "目前label43557.RDATA")

########前五十檔##########
#股價(未調整)中的標的
stockprice_read <- fread(file="stockprice20122019.txt",header = T,stringsAsFactors = F,sep="\t")
colnames(stockprice_read) <- c("code","company_name","industry","type","date","open","high","low","close","volume","mkt_value","PE","PB")
stockprice_read$company_name <- gsub("\\s+", "", stockprice_read$company_name)
stockprice_read$code <- as.numeric(stockprice_read$code)
stockprice_read$open <- as.numeric(stockprice_read$open)
company_list <- stockprice_read %>% select(code,company_name) %>%
  distinct(code,company_name) %>%  mutate(company_name=as.character(company_name)) %>% arrange(desc(company_name))
company_list <- company_list %>% filter(company_name!="長科*")

#########################
stockName <- ETF50$company_name
time00 <- Sys.time()
allnews_combine <- NULL
news_num <- NULL
ix <- 1
for (ix in 1:length(stockName)) {
  cat(paste0("目前正在讀取第 ",ix," 家公司，進度: ",ix," / ",length(stockName),"\n"))
  news15 <- newNews_JB_all %>% filter(str_detect(company_name,stockName[ix]))
  news_count <- news15 %>% group_by(company_name) %>% summarise(num=n()) 
  allnews_combine  <- rbind(allnews_combine,news15)
  news_num <- rbind(news_num,news_count)
  time01 <- Sys.time()
  
}
time01 - time00
allnews_combine$ID %>% unique() %>% length()
news50ID <- allnews_combine$ID %>% unique() 
# save(news50ID,ETF50,file = "news50ID.RDATA")

###找出50全部#####
newNews_JB_50 <- newNews_JB %>% 
  filter(ID%in%news50ID) %>% 
  mutate(yyyy=year(ymd_hm(Newstime)))



########新聞敘述################

#全90745
newNews_JB_year <- newNews_JB %>% 
  group_by(yyyy) %>% 
  summarise(count = n())

newNews_JB_day <- newNews_JB %>% 
  mutate(date=date(ymd_hm(Newstime))) %>% 
  group_by(date) %>% 
  summarise(count = n())
summary(newNews_JB_day$count)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00   13.00   89.00   88.02  139.00  514.00


newNews_JB_hour <- newNews_JB %>% mutate(hour=as.numeric(substring(Newstime,9,10)))
table(newNews_JB_hour$hour) %>% data.frame() %>% spread(Var1,Freq)
table(newNews_JB_hour$TradeTime)
newNews_JB_hour %>% 
  ggplot(aes(x=hour))+
  geom_bar(fill ="#c6d9f1") + 
  xlab(label = "幾點")+
  ylab(label = "新聞篇數")+
  ggtitle("新聞發布時點")+
  theme_minimal()+
  scale_fill_discrete(guide = FALSE)+
  scale_x_continuous(breaks=seq(0, 23, 1))
#8點最多

newNews_JB_hour %>% filter(hhmm>=1300,hhmm<1330) %>% count()




#50
newNews_JB_year_50 <- newNews_JB_50 %>% 
  group_by(yyyy) %>% 
  summarise(count = n())

newNews_JB_day_50 <- newNews_JB_50 %>% 
  mutate(date=date(ymd_hm(Newstime))) %>% 
  group_by(date) %>% 
  summarise(count = n())
summary(newNews_JB_day_50$count)


#標籤
#總數45373
newNews_JB_year_now <- newNews_JB_50 %>% 
  group_by(yyyy) %>% 
  summarise(count = n()/2)

newNews_JB_now$label %>% table %>% prop.table() 
# 中立      正面      負面 
# 0.1626145 0.6488050 0.1885805 

