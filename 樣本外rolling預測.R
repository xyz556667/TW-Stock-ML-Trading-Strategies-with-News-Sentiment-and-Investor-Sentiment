rm(list=ls());gc()
library(dplyr)
library(fastrtext)
library(data.table)

#斷詞後所有新聞
load("D:/Text/傳銘/論文/論文實證/現在所有新聞.RDATA")
#標籤新聞
load("D:/Text/傳銘/論文/論文實證/目前label43557.RDATA")
#50檔新聞
load("D:/Text/傳銘/論文/論文實證/50檔所有新聞.RDATA")

news_label <- newNews_JB_now %>% filter(label!="無")


news_label <- news_label %>% 
  mutate(label2 = ifelse(label=="正面","正面",
                         ifelse(label=="負面","負面",
                                ifelse(label=="中立" & word_polarity>0 ,"正面","負面"))))

#以下做完了
# newNews_JB_50 <- newNews_JB_50 %>% left_join(news_label %>% select(ID,label,label2),by = c("ID"="ID"))
# newNews_JB_50$label[is.na(newNews_JB_50$label)] <- "無"
# newNews_JB_50$label2[is.na(newNews_JB_50$label2)] <- "無"



#以下要手動更改class_num，2是二元，3是三元
# fasttext GRAM
RANK_table <- NULL
fasttext_prediction <- NULL
#rolling
i=2013
gc()
for( class_num in c(2)){
  for(g in c(3)){
    for( i in 2013:2018){
      
      if (class_num==2) {
        trainData <- news_label %>% filter(yyyy <= i) %>% select(-label) %>% mutate(label=label2)
        testData  <- newNews_JB_50 %>% filter(yyyy == i+1)           
      }else{
        trainData <- news_label %>% filter(yyyy <= i)
        testData  <- newNews_JB_50 %>% filter(yyyy == i+1) 
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
      testData$predict2 <- sapply(predictions, names)
      fasttext_prediction <- rbind(fasttext_prediction,testData)
      end <- proc.time()
      elapsed <- as.numeric((end-start)[3])
      
      accurRate <- mean(testData$predict == testData$label)
      # confusionMatrix <- table(testData$predic, testData$label)
      # confusionMatrix
      # accurRate
      tmp <- data.frame(class=class_num,
                        gram=g,
                        yyyy=i,
                        accuracy=accurRate,
                        time=elapsed)
      RANK_table <- rbind(RANK_table,tmp)
      gc()
    }
  }
}
#以下要手動更改class_num，2是二元，3是三元
# write.csv(fasttext_prediction,"fasttext_prediction3.csv",row.names = FALSE)
write.csv(fasttext_prediction,"fasttext_prediction2.csv",row.names = FALSE)
