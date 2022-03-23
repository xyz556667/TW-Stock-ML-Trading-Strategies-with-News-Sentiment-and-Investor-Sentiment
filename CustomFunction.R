## ���R������s�{���ۻs���
## �{�����g: ���s�]�� ��s�U�z Ĭ�ۮx

####################################### �����h�Ӽҫ��w���� #######################################
MeanPredict <- function(predictValue){
  predictValue <- sort(predictValue)
  predictValue <- predictValue[2:(length(predictValue)-1)]   # �h���̧C�ȻP�̰���
  return(mean(predictValue))
}


####################################### ø�s�ҫ��w����O�ϧΨ�� #######################################
PlotModelPerformance <- function(performanceTable){
  windowsFonts(A=windowsFont("�L�n������"))
  # ø�s�U���e�ȹw���ϧ�
  p1 <- ggplot(data = performanceTable, aes(x = cutValue, y = meanRet, group = 1)) +
    geom_line() +
    geom_point() +
    theme_minimal()+
    theme(text = element_text(size=16,family = "A"),axis.text.x = element_text(angle = 0,size=10))+
    labs(title = "�w�����S�v�����e�ȻP�������S�v���Y", x = "�w�����S�v�����e��", y = "�������S�v")
  
  p2 <- ggplot(data = performanceTable, aes(x = cutValue, y = winRatio, group = 1)) +
    geom_line() +
    geom_point() +
    theme_minimal()+
    theme(text = element_text(size=16,family = "A"),axis.text.x = element_text(angle = 00,size=10))+
    labs(title = "�w�����S�v�����e�ȻPĹ�v���Y",x = " �w�����S�v�����e��", y = "Ĺ�v")
  
  p3 <- ggplot(data = performanceTable, aes(x = cutValue, y = tradeNums, group = 1)) +
    geom_line() +
    geom_point() + 
    theme_minimal()+
    theme(text = element_text(size=16,family = "A"),axis.text.x = element_text(angle = 00,size=10))+
    labs(title = "�w�����S�v�����e�ȻP����������Y", x = "�w�����S�v�����e��", y = "�������")
  
  ggarrange(p1, p2, p3,  ncol = 1, nrow = 3)
}


####################################### �S�x��z��� #######################################
Rolling_fun <- function(data,days_num){
  
  data1 <- data %>%
    group_by(group) %>%
    arrange(group,date) %>%
    mutate(value = ifelse(value == 0, lag(value, 1), value))  # ���Ǹ�ƥi��|�����D�A�Y���ѨS����ƩάO��0�h�H�e�@�ѥN��
  
  data1 <- data1 %>%
    arrange(group,date) %>%
    group_by(group) %>%
    mutate(ret = log(value/lag(value, 1))) # �Hlog�Ӻ���S�v�b�⤣�P���ת����S�v�ɥu�n�ۥ[�N�O�ֿn���S�����K�p��
  
  data1 <- data1 %>%
    group_by(group) %>%
    mutate(ret_days = rollapplyr(ret, days_num, FUN = "sum", fill = NA)) %>%  # days_num �O�ֿn����
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

####################################### ø�s�ҫ��w����O�ϧΨ�� #######################################
PlotModelPerformance_compare <- function(performanceTable){
  
  # ø�s�U���e�ȹw���ϧ�
  p1 <- ggplot(data = performanceTable, aes(x = cutValue, y = meanRet, color = type)) +
    geom_line() +
    geom_point() +
    labs(title = "���մ��w�����S�v�����e�ȻP�������S�v���Y", x = "�w�����S�v�����e��", y = "�������S�v")
  
  p2 <- ggplot(data = performanceTable, aes(x = cutValue, y = winRatio, color = type)) +
    geom_line() +
    geom_point() +
    labs(title = "���մ��w�����S�v�����e�ȻP�Ӳv���Y",x = " �w�����S�v�����e��", y = "�Ӳv")
  
  p3 <- ggplot(data = performanceTable, aes(x = cutValue, y = tradeNums, color = type)) +
    geom_line() +
    geom_point() + 
    labs(title = "���մ��w�����S�v�����e�ȻP����������Y", x = "�w�����S�v�����e��", y = "�������")
  
  ggarrange(p1, p2, p3,  ncol = 1, nrow = 3)
}