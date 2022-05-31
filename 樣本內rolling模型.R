rm(list=ls());gc()
library(dplyr)
library(fastrtext)
library(data.table)
library(MLmetrics)
#斷詞後所有新聞
load("D:/Text/傳銘/論文/論文實證/現在所有新聞.RDATA")
#目前標籤新聞
load("D:/Text/傳銘/論文/論文實證/目前label43557.RDATA")

news_label <- newNews_JB_now %>% filter(label!="無")
news_unlabel <- newNews_JB_now %>% filter(label=="無")
news_label <- newNews_JB_now %>% mutate(k_fold=row_number()%%5,
                                        k_fold=ifelse(k_fold==0,5,k_fold))
news_label <- news_label %>% 
  mutate(label2 = ifelse(label=="正面","正面",
                        ifelse(label=="負面","負面",
                               ifelse(label=="中立" & word_polarity>0 ,"正面","負面"))))


# fasttext GRAM
RANK_table <- NULL
#rolling
i=2013
gc()
for( class_num in c(2,3)){
  for(g in c(3)){
    for( i in 2013:2018){
      
      if (class_num==2) {
        Data <- news_label %>% filter(yyyy <= i) %>% select(-label) %>% mutate(label=label2)
        index <- 1:(nrow(Data)*0.8)
        trainData <- Data[index,]
        testData  <- Data[-index,]          
      }else{
        Data <- news_label %>% filter(yyyy <= i)
        index <- 1:(nrow(Data)*0.8)
        trainData <- Data[index,]
        testData  <- Data[-index,]   
      }
      table(news_label$label)
      table(trainData$label)
      table(testData$label)
      
      col <- paste0("textJB_PY_")
      traincol <- trainData %>% select(col)
      testcol <- testData %>% select(col)
      
      start<- proc.time()
      
      # prepare data
      train_labels <- paste0("__label__", as.factor(trainData$label))
      train_texts <- tolower(as.character(traincol[,1]))
      train_to_write <- paste(train_labels, train_texts)
      
      train_tmp_file_txt <- tempfile()
      writeLines(text = train_to_write, con = train_tmp_file_txt)
      tmp_file_model <- tempfile()
      
      test_labels <- paste0("__label__", as.factor(testData$label))
      test_texts <- tolower(as.character(testcol[,1]))
      test_to_write <- paste(test_labels, test_texts)
      
      Encoding(train_tmp_file_txt)
      
      # learn model
      execute(commands = c("supervised",
                           "-input", train_tmp_file_txt,
                           "-output", tmp_file_model,
                           "-dim", 100,
                           "-lr", 1,
                           "-epoch", 20,
                           "-wordNgrams", g,
                           "-verbose", 1))
      
      
      # load model
      model <- load_model(tmp_file_model)
      predictions <- predict(model, sentences = test_to_write)
      testData$predict <- sapply(predictions, names)
      
      end <- proc.time()
      elapsed <- as.numeric((end-start)[3])
      
      accurRate <- mean(testData$predict == testData$label)
      precision <- Precision(testData$label,testData$predict)
      # confusionMatrix <- table(testData$predic, testData$label)
      # confusionMatrix
      # accurRate
      tmp <- data.frame(class=class_num,
                        gram=g,
                        yyyy=i,
                        accuracy=accurRate,
                        precision=precision,
                        time=elapsed)
      RANK_table <- rbind(RANK_table,tmp)
      gc()
    }
  }
}

# write.csv(RANK_table,"fast_gram_20190521.csv",row.names = FALSE)




# fasttext tfidf
RANK_table <- NULL


for( class_num in c(2)){
  for(top in c(10,20,50,100)){
    for( k in c(1,2,3,4,5)){
      
      if (class_num==2) {
        trainData <- news_label %>% filter(k_fold!=k,label!="中立")
        testData  <- news_label %>% filter(k_fold==k,label!="中立")
      }else{
        trainData <- news_label %>% filter(k_fold!=k)
        testData  <- news_label %>% filter(k_fold==k)
      }
      table(news_label$label)
      table(trainData$label)
      table(testData$label)
      
      col <- paste0('1gram_',as.character(top),"top")
      traincol <- trainData %>% select(col)
      testcol <- testData %>% select(col)
      
      start<- proc.time()
      
      # prepare data
      train_labels <- paste0("__label__", as.factor(trainData$label))
      train_texts <- tolower(as.character(traincol[,1]))
      train_to_write <- paste(train_labels, train_texts)
      
      train_tmp_file_txt <- tempfile()
      writeLines(text = train_to_write, con = train_tmp_file_txt)
      tmp_file_model <- tempfile()
      
      test_labels <- paste0("__label__", as.factor(testData$label))
      test_texts <- tolower(as.character(testcol[,1]))
      test_to_write <- paste(test_labels, test_texts)
      
      Encoding(train_tmp_file_txt)
      
      # learn model
      execute(commands = c("supervised",
                           "-input", train_tmp_file_txt,
                           "-output", tmp_file_model,
                           "-dim", 100,
                           "-lr", 1,
                           "-epoch", 20,
                           "-wordNgrams", 1,
                           "-verbose", 1))
      
      
      # load model
      model <- load_model(tmp_file_model)
      predictions <- predict(model, sentences = test_to_write)
      testData$predict <- sapply(predictions, names)
      
      end <- proc.time()
      elapsed <- as.numeric((end-start)[3])
      
      accurRate <- mean(testData$predict == testData$label)
      # confusionMatrix <- table(testData$predic, testData$label)
      # confusionMatrix
      # accurRate
      tmp <- data.frame(class=class_num,
                        top=top,
                        k=k,
                        accuracy=accurRate,
                        time=elapsed)
      RANK_table <- rbind(RANK_table,tmp)
    }
  }
}

# write.csv(RANK_table,"fast_tfidf_20190521.csv",row.names = FALSE)

# fasttext CH
RANK_table <- NULL


for( class_num in c(2)){
  for(ch in c(0.01,0.02,0.05,0.1,0.2,0.5,0.8,1)){
    for( k in c(1,2,3,4,5)){
      
      if (class_num==2) {
        trainData <- news_label %>% filter(k_fold!=k,label!="中立")
        testData  <- news_label %>% filter(k_fold==k,label!="中立")
      }else{
        trainData <- news_label %>% filter(k_fold!=k)
        testData  <- news_label %>% filter(k_fold==k)
      }
      table(news_label$label)
      table(trainData$label)
      table(testData$label)
      
      col <- paste0('CH',as.character(ch))
      traincol <- trainData %>% select(col)
      testcol <- testData %>% select(col)
      
      start<- proc.time()
      
      # prepare data
      train_labels <- paste0("__label__", as.factor(trainData$label))
      train_texts <- tolower(as.character(traincol[,1]))
      train_to_write <- paste(train_labels, train_texts)
      
      train_tmp_file_txt <- tempfile()
      writeLines(text = train_to_write, con = train_tmp_file_txt)
      tmp_file_model <- tempfile()
      
      test_labels <- paste0("__label__", as.factor(testData$label))
      test_texts <- tolower(as.character(testcol[,1]))
      test_to_write <- paste(test_labels, test_texts)
      
      Encoding(train_tmp_file_txt)
      
      # learn model
      execute(commands = c("supervised",
                           "-input", train_tmp_file_txt,
                           "-output", tmp_file_model,
                           "-dim", 100,
                           "-lr", 1,
                           "-epoch", 20,
                           "-wordNgrams", 1,
                           "-verbose", 1))
      
      
      # load model
      model <- load_model(tmp_file_model)
      predictions <- predict(model, sentences = test_to_write)
      testData$predict <- sapply(predictions, names)
      
      end <- proc.time()
      elapsed <- as.numeric((end-start)[3])
      
      accurRate <- mean(testData$predic == testData$label)
      # confusionMatrix <- table(testData$predic, testData$label)
      # confusionMatrix
      # accurRate
      tmp <- data.frame(class=class_num,
                        ch=ch,
                        k=k,
                        accuracy=accurRate,
                        time=elapsed)
      RANK_table <- rbind(RANK_table,tmp)
    }
  }
}

# write.csv(RANK_table,"fast_ch_20190521.csv",row.names = FALSE)



# fasttext MI
RANK_table <- NULL


for( class_num in c(2)){
  for(mi in c(0.01,0.02,0.05,0.1,0.2,0.5,0.8,1)){
    for( k in 2013:2018){
      
      if (class_num==2) {
        trainData <- news_label %>% filter(yyyy <= k) 
        testData  <- news_label %>% filter(yyyy == k+1) 
      }else{
        trainData <- news_label %>% filter(k_fold!=k)
        testData  <- news_label %>% filter(k_fold==k)
      }
      table(news_label$label)
      table(trainData$label)
      table(testData$label)
      
      col <- paste0('MI',as.character(mi))
      traincol <- trainData %>% select(col)
      testcol <- testData %>% select(col)
      
      start<- proc.time()
      
      # prepare data
      train_labels <- paste0("__label__", as.factor(trainData$label))
      train_texts <- tolower(as.character(traincol[,1]))
      train_to_write <- paste(train_labels, train_texts)
      
      train_tmp_file_txt <- tempfile()
      writeLines(text = train_to_write, con = train_tmp_file_txt)
      tmp_file_model <- tempfile()
      
      test_labels <- paste0("__label__", as.factor(testData$label))
      test_texts <- tolower(as.character(testcol[,1]))
      test_to_write <- paste(test_labels, test_texts)
      
      Encoding(train_tmp_file_txt)
      
      # learn model
      execute(commands = c("supervised",
                           "-input", train_tmp_file_txt,
                           "-output", tmp_file_model,
                           "-dim", 100,
                           "-lr", 1,
                           "-epoch", 20,
                           "-wordNgrams", 1,
                           "-verbose", 1))
      
      
      # load model
      model <- load_model(tmp_file_model)
      predictions <- predict(model, sentences = test_to_write)
      testData$predict <- sapply(predictions, names)
      
      end <- proc.time()
      elapsed <- as.numeric((end-start)[3])
      
      accurRate <- mean(testData$predic == testData$label)
      # confusionMatrix <- table(testData$predic, testData$label)
      # confusionMatrix
      # accurRate
      tmp <- data.frame(class=class_num,
                        mi=mi,
                        k=k,
                        accuracy=accurRate,
                        time=elapsed)
      RANK_table <- rbind(RANK_table,tmp)
    }
  }
}

write.csv(RANK_table,"fast_mi_20190521.csv",row.names = FALSE)


# # fasttext TFIDF GRAM
# RANK_table <- NULL
# 
# 
# for( class_num in c(2,3)){
#   for(g in c(1,2,3)){
#   for(top in c(10,20,50,100,200,300,400)){
#     for( k in c(1,2,3,4,5)){
#       
#       if (class_num==2) {
#         trainData <- news_label %>% filter(k_fold!=k,label!="中立")                        
#         testData  <- news_label %>% filter(k_fold==k,label!="中立")
#       }else{
#         trainData <- news_label %>% filter(k_fold!=k)                        
#         testData  <- news_label %>% filter(k_fold==k)
#       }
#       table(news_label$label)
#       table(trainData$label)
#       table(testData$label)
#       
#       col <- paste0(as.character(g),'gram_',as.character(top),"top")
#       traincol <- trainData %>% select(col)
#       testcol <- testData %>% select(col)
#       
#       start<- proc.time()
#       
#       # prepare data
#       train_labels <- paste0("__label__", as.factor(trainData$label))
#       train_texts <- tolower(as.character(traincol[,1]))
#       train_to_write <- paste(train_labels, train_texts)
#       
#       train_tmp_file_txt <- tempfile()
#       writeLines(text = train_to_write, con = train_tmp_file_txt)
#       tmp_file_model <- tempfile()
#       
#       test_labels <- paste0("__label__", as.factor(testData$label))
#       test_texts <- tolower(as.character(testcol[,1]))
#       test_to_write <- paste(test_labels, test_texts)
#       
#       Encoding(train_tmp_file_txt)
#       
#       # learn model
#       execute(commands = c("supervised",
#                            "-input", train_tmp_file_txt,
#                            "-output", tmp_file_model,
#                            "-dim", 100,
#                            "-lr", 1,
#                            "-epoch", 20,
#                            "-wordNgrams", 1,
#                            "-verbose", 1))
#       
#       
#       # load model
#       model <- load_model(tmp_file_model)
#       predictions <- predict(model, sentences = test_to_write)
#       testData$predict <- sapply(predictions, names)
#       
#       end <- proc.time()
#       elapsed <- as.numeric((end-start)[3])
#       
#       accurRate <- mean(testData$predic == testData$label)
#       # confusionMatrix <- table(testData$predic, testData$label)
#       # confusionMatrix
#       # accurRate
#       tmp <- data.frame(class=class_num,
#                         gram=g,
#                         top=top,
#                         k=k,
#                         accuracy=accurRate,
#                         time=elapsed)
#       RANK_table <- rbind(RANK_table,tmp)
#     }
#   }
# }
# 
# }
# 
# 
# 
# write.csv(RANK_table,"fast_tfidfgram_20190521.csv",row.names = FALSE)














