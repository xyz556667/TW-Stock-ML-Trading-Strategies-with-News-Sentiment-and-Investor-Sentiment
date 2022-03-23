## 當沖策略更新程式自製函數
## 程式撰寫: 中山財管 研究助理 蘇彥庭

####################################### 平均多個模型預測值 #######################################
MeanPredict <- function(predictValue){
  predictValue <- sort(predictValue)
  predictValue <- predictValue[2:(length(predictValue)-1)]   # 去掉最低值與最高值
  return(mean(predictValue))
}


####################################### 繪製模型預測能力圖形函數 #######################################
PlotModelPerformance <- function(performanceTable){
  windowsFonts(A=windowsFont("微軟正黑體"))
  # 繪製各門檻值預測圖形
  p1 <- ggplot(data = performanceTable, aes(x = cutValue, y = meanRet, group = 1)) +
    geom_line() +
    geom_point() +
    theme_minimal()+
    theme(text = element_text(size=16,family = "A"),axis.text.x = element_text(angle = 0,size=10))+
    labs(title = "預測報酬率投資門檻值與平均報酬率關係", x = "預測報酬率投資門檻值", y = "平均報酬率")
  
  p2 <- ggplot(data = performanceTable, aes(x = cutValue, y = winRatio, group = 1)) +
    geom_line() +
    geom_point() +
    theme_minimal()+
    theme(text = element_text(size=16,family = "A"),axis.text.x = element_text(angle = 00,size=10))+
    labs(title = "預測報酬率投資門檻值與贏率關係",x = " 預測報酬率投資門檻值", y = "贏率")
  
  p3 <- ggplot(data = performanceTable, aes(x = cutValue, y = tradeNums, group = 1)) +
    geom_line() +
    geom_point() + 
    theme_minimal()+
    theme(text = element_text(size=16,family = "A"),axis.text.x = element_text(angle = 00,size=10))+
    labs(title = "預測報酬率投資門檻值與交易次數關係", x = "預測報酬率投資門檻值", y = "交易次數")
  
  ggarrange(p1, p2, p3,  ncol = 1, nrow = 3)
}


####################################### 特徵整理函數 #######################################
Rolling_fun <- function(data,days_num){
  
  data1 <- data %>%
    group_by(group) %>%
    arrange(group,date) %>%
    mutate(value = ifelse(value == 0, lag(value, 1), value))  # 有些資料可能會有問題，若當天沒有資料或是為0則以前一天代替
  
  data1 <- data1 %>%
    arrange(group,date) %>%
    group_by(group) %>%
    mutate(ret = log(value/lag(value, 1))) # 以log來算報酬率在算不同長度的報酬率時只要相加就是累積報酬比較方便計算
  
  data1 <- data1 %>%
    group_by(group) %>%
    mutate(ret_days = rollapplyr(ret, days_num, FUN = "sum", fill = NA)) %>%  # days_num 是累積長度
    select(-ret, -value) %>%
    spread(key = group, value = ret_days)
  
  group_name <- data %>% arrange(group) %>% select(group) %>% distinct(group) %>% pull(group)
  
  factor_name <- paste0(group_name, "_", days_num)
  
  colnames(data1) <- c("date", factor_name)
  
  return(data1)
}

MA_fun <- function(data){
  
  data1 <- data %>%
    group_by(group) %>%
    arrange(group, date) %>%
    mutate(ma5 = rollapplyr(value, 5, FUN = "mean", fill = NA),
           ma20 = rollapplyr(value, 20, FUN = "mean", fill = NA),
           ma20_ma5 = ma20/ma5,
           ma5_p = ma5/value,
           ma20_p = ma20/value) %>%
    select(-ma5,-ma20) %>%
    ungroup()
  
  group_name <- data %>% arrange(group) %>% select(group) %>% distinct(group) %>% pull(group)
  
  factor_colnames <- colnames(data1)[4:length(data1)]
  
  factor_table_store <- data1 %>% select(date) %>% distinct(date) %>% arrange(date)
  
  for (ixx in 1:(length(data1)-3)) {
    
    factor_table <- data1[,c(1,2,ixx+3)]
    factor_table <- factor_table %>% spread(key = group, value = factor_colnames[ixx])
    colnames(factor_table) <- c("date", paste0(group_name,"_", factor_colnames[ixx]))
    factor_table_store <- factor_table_store %>% cbind(factor_table %>% select(-date))  
  }
  
  return(factor_table_store)
}

####################################### 繪製模型預測能力圖形函數 #######################################
PlotModelPerformance_compare <- function(performanceTable){
  
  # 繪製各門檻值預測圖形
  p1 <- ggplot(data = performanceTable, aes(x = cutValue, y = meanRet, color = type)) +
    geom_line() +
    geom_point() +
    labs(title = "測試期預測報酬率投資門檻值與平均報酬率關係", x = "預測報酬率投資門檻值", y = "平均報酬率")
  
  p2 <- ggplot(data = performanceTable, aes(x = cutValue, y = winRatio, color = type)) +
    geom_line() +
    geom_point() +
    labs(title = "測試期預測報酬率投資門檻值與勝率關係",x = " 預測報酬率投資門檻值", y = "勝率")
  
  p3 <- ggplot(data = performanceTable, aes(x = cutValue, y = tradeNums, color = type)) +
    geom_line() +
    geom_point() + 
    labs(title = "測試期預測報酬率投資門檻值與交易次數關係", x = "預測報酬率投資門檻值", y = "交易次數")
  
  ggarrange(p1, p2, p3,  ncol = 1, nrow = 3)
}
