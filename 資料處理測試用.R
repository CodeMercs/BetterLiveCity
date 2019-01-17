library("data.table")
require(ggplot2)
#取得資料夾內的檔案名稱集，命名：theFiles
theFiles <- dir("data//WRDataUTF8", pattern = "\\.csv")
#建list名explot，用來存放.csv檔
explot <- list()
#迴圈次數1~theFiles的長度
for (n in c(1:length(theFiles))) {
  #存放格式explot[[檔名]]內存放對應(檔名.csv)內容的data.frame
  explot[[theFiles[n]]] <-
    read.csv(paste0("data//WRDataUTF8//", theFiles[n]))
}
#建list名ophren，用來存放更改結構後的內容
ophren<-list()
#迴圈次數為1~explot[[第109個檔案檔名]]的地區值數量(29)
for ( area in c(1:length(explot[[theFiles[109]]][,1]))) {
  type  <-c()
  warm  <-c()
  y     <-c()
  #迴圈次數為1~theFiles的長度
  for(n in c(1:length(theFiles))){
    #此迴圈用來取得109個檔案內特定地區的溫濕度資料
    explot[[theFiles[n]]]<-read.csv(paste0("data//WRDataUTF8//", theFiles[n]), stringsAsFactors = FALSE )
    #假如資料不為空，type<-溫度,warm<-濕度,y<-年份
    ifelse(is.null(explot[[theFiles[n]]][area,2]),type[n] <-c(0),type[n] <-c(explot[[theFiles[n]]][area,2]))
    ifelse(is.null(explot[[theFiles[n]]][area,8]),warm[n] <-c(0),warm[n] <-c(explot[[theFiles[n]]][area,8]))
    ifelse(is.null(substring(theFiles[n],4,9)),y[n] <-c(0),y[n] <-substring(theFiles[n],4,9))
  }
  #迴圈結束後，type,warm,y三筆各自數量皆為109
  #st是迴圈次數對應的地區，第1次就是第一個地區，第29次就是地29個地區，類型是字串
  st<- explot[[theFiles[109]]][,1][area]
  
  #將type,warm,y三筆特定地區的溫濕度存進ophren[[地區名]]內
  ophren[[st]] <- data.table(
    TIME = y, 
    TAVE = type,
    CHA  = warm
  )
}

#datalist用來存放修改後的數據
datalist <- list()
#迴圈次數10次，dateyear的值依序為
#"2008" "2009" "2010" "2011" "2012" "2013" "2014" "2015" "2016" "2017"
for(dateyear in unique(substring(theFiles,4,7))){
  #aera_vector存地區
  #cha_abs_vector存cha-最佳的絕對值，排名用
  #cha_mean_vector存cha的年度平均
  #tave_abs_vector存tave-最佳的絕對值，排名用
  #tave_mean_vector存tave的年度平均
  aera_vector <- c()
  cha_abs_vector<-c()
  cha_mean_vector <-c()
  tave_abs_vector<-c()
  tave_mean_vector <-c()
  #迴圈次數為1~explot[[第109個檔案檔名]]的地區值數量(29)
  for (a in  c(1:length(explot[[theFiles[109]]][,1]))) {
    #透過data.table的篩選特性
    #取出該地區的年平均-最佳溫濕度（溫度27.5及濕度50）
    tave_abs_vector[a]    <- abs(ophren[[a]][grepl(dateyear,TIME),mean(TAVE)]- 27.5)
    cha_abs_vector[a]    <- abs(ophren[[a]][grepl(dateyear,TIME),mean(CHA)]- 50)
    #取出該地區的年平均
    tave_mean_vector[a]<-ophren[[a]][grepl(dateyear,TIME),mean(TAVE)]
    cha_mean_vector[a]<-ophren[[a]][grepl(dateyear,TIME),mean(CHA)]
    #取出地區
    aera_vector[a] <- explot[[theFiles[109]]][,1][a]
  }
  #重組為新data.table，暫存名datavector
  datavector<-data.table(
    TAVE_ABS = tave_abs_vector,
    TAVE_MEAN = tave_mean_vector,
    CHA_ABS = cha_abs_vector,
    CHA_MEAN =cha_mean_vector,
    AERA = aera_vector
  )
  #datavector存進datalist[[年份]]
  datalist[[dateyear]] <-datavector
  #匯出成.csv檔，匯出到testcsv//年份.csv
  write.table(datavector, file = paste0("testcsv//",dateyear,".csv"), sep = ",")
}

#以下為讀出上面匯出的.csv檔------------------------------------------
testcsvs <- dir("testcsv", pattern = "\\.csv")
testcsvlist1 <- list()
for (n in c(1:length(testcsvs))) {
  testcsvlist1[[substring(testcsvs[n],1,4)]] <-
    data.table(read.table(paste0("testcsv//", testcsvs[n]), header = TRUE, sep = ","))
}
#------------------------------------------


theFiles <- dir("data//WRDataUTF8", pattern = "\\.csv")
explot <- list()
#迴圈次數為1~theFiles的長度
#此迴圈用來將109筆檔案全部併表
for (n in c(1:length(theFiles))) {
  addnum<-c()
  li<-data.table(read.csv(paste0("data//WRDataUTF8//", theFiles[n])))
  #---為各地區資料增加對應年份---
  for(num in c(1:length(li$AREA))){
    addnum[num]<-substring(theFiles[n],4,9)
  }
  li$TIME<-addnum
  #---------------------------
  #加完年份暫存
  explot[[substring(theFiles[n],4,9)]]<-li
  
  if(n==2){
    #迴圈執行第二次時，將第一筆與現在第二筆併表成alldata
    alldata <- rbind(explot[[substring(theFiles[1],4,9)]], li)
  }
  if(n>2){
    #第三次開始直接併到alldata後
    alldata <- rbind(alldata, li)
  }
}
#全部併表完得到alldata，再匯出成.csv成all_data_table.csv
write.table(alldata, file = "testcsv//all_data_table.csv", sep = ",")

#以下為alldata的使用測試----------------------
ophren<-list()

alltable<-data.table(read.table("testcsv//all_data_table.csv", header = TRUE, sep = ","))

alltable[grepl('2014',TIME)&grepl('嘉義',AREA),max(TAVE)]
View(alltable[grepl('2014',TIME)&grepl('嘉義',AREA)][order(TAVE)])
alltable[grepl('2014',TIME)&grepl('嘉義',AREA)]$TIME
#取特定地區時間的排名
#datalist[[1]][order(ABS),.(c(1:length(explot[[theFiles[109]]][,1])),AERA,CHA)][grepl("阿里山",AERA)]


#ggplot(ophren[['竹子湖']][grepl('2014',TIME)], aes(x = ophren[['竹子湖']][grepl('2014',TIME)]$TIME, y = ophren[['竹子湖']][grepl('2014',TIME)]$TAVE)) + geom_bar(stat ="identity",width=0.5, fill = "#33CCFF")+geom_text(mapping = aes(label= ophren[['竹子湖']][grepl('2014',TIME)]$TAVE),color ="orange",vjust = -0.5, size = 5)+geom_line()+labs( x="溫度", y="月份")+theme(axis.title.y = element_text(angle = 0, family = "宋體-繁 細體"),
#                                                                                                                                                                                                                                                                                                                                                                                                 axis.text.x = element_text(angle = 60, family = "宋體-繁 細體", hjust = 1),
#                                                                                                                                                                                                                                                                                                                                                                                                 axis.title.x = element_text(family = "宋體-繁 細體"))

