library("data.table")
require(ggplot2)
theFiles <- dir("data//WRDataUTF8", pattern = "\\.csv")
explot <- list()
for (n in c(1:length(theFiles))) {
  explot[[theFiles[n]]] <-
    read.csv(paste0("data//WRDataUTF8//", theFiles[n]))
}
ophren<-list()

for ( area in c(1:length(explot[[theFiles[109]]][,1]))) {
  type  <-c()
  warm  <-c()
  y     <-c()
  for(n in c(1:length(theFiles))){
    explot[[theFiles[n]]]<-read.csv(paste0("data//WRDataUTF8//", theFiles[n]), stringsAsFactors = FALSE )
    ifelse(is.null(explot[[theFiles[n]]][area,2]),type[n] <-c(0),type[n] <-c(explot[[theFiles[n]]][area,2]))
    ifelse(is.null(explot[[theFiles[n]]][area,8]),warm[n] <-c(0),warm[n] <-c(explot[[theFiles[n]]][area,8]))
    ifelse(is.null(substring(theFiles[n],4,9)),y[n] <-c(0),y[n] <-substring(theFiles[n],4,9))
  }
  st<- explot[[theFiles[109]]][,1][area]

  ophren[[st]] <- data.table(
    TIME = y, 
    TAVE = type,
    CHA  = warm
  )
  print(ophren[[st]])
}

ggplot(ophren[['竹子湖']][grepl('2014',TIME)], aes(x = ophren[['竹子湖']][grepl('2014',TIME)]$TIME, y = ophren[['竹子湖']][grepl('2014',TIME)]$TAVE)) + geom_bar(stat ="identity",width=0.5, fill = "#33CCFF")+geom_text(mapping = aes(label= ophren[['竹子湖']][grepl('2014',TIME)]$TAVE),color ="orange",vjust = -0.5, size = 5)+geom_line()+labs( x="溫度", y="月份")+theme(axis.title.y = element_text(angle = 0, family = "宋體-繁 細體"),
                                                                                                                                                                                                                                                                                                                                                                                                 axis.text.x = element_text(angle = 60, family = "宋體-繁 細體", hjust = 1),
                                                                                                                                                                                                                                                                                                                                                                                                 axis.title.x = element_text(family = "宋體-繁 細體"))

