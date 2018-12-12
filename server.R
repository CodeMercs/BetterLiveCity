library(shiny)
library(data.table)
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
  
}
minlist2017 <-c()
date2017 <- c()
abs2017<-c()
for(a in  c(1:length(explot[[theFiles[109]]][,1]))){
  abs2017[a]    <- abs(ophren[[a]][grepl("2017",TIME),min(CHA)]- 50)
  minlist2017[a]<-ophren[[a]][grepl("2017",TIME),min(CHA)]
  date2017[a] <- explot[[theFiles[109]]][,1][a]
}
data2017 <- data.table(
  ABS = abs2017,
  CHA =minlist2017,
  AERA = date2017
)
shinyServer(function(input, output, session){
  
 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#  
  
#  output$exPlot<-renderPlot({
#    ggplot(dt, aes(x = dt$AREA, y = dt$TAVE)) + geom_bar(stat ="identity")
#  })
#  output$exPlot2<-renderPlot({
#    ggplot(ot, aes(x = ot$AREA, y = ot$TAVE)) + geom_bar(stat ="identity")
#  })
#  output$exPlot3<-renderPlot({
#    ggplot(ft, aes(x = ft$AREA, y = ft$TAVE)) + geom_bar(stat ="identity")
#  })
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#  
  output$dateText1  <- renderText({
    paste0("地區：", input$select)
  })
  output$dateText2  <- renderText({
    paste0("年份：", input$selectdate)
  })
  output$max  <- renderText({
    paste0("年最高溫度：", ophren[[input$select]][grepl(input$selectdate,TIME),max(TAVE)])
  })
  output$min  <- renderText({
    paste0("年最低溫度：", ophren[[input$select]][grepl(input$selectdate,TIME),min(TAVE)])
  })
  output$mean  <- renderText({
    paste0("年平均溫度：", ophren[[input$select]][grepl(input$selectdate,TIME),mean(TAVE)])
  })
  output$dateText21  <- renderText({
    paste0("地區：", input$select)
  })
  output$dateText22  <- renderText({
    paste0("年份：", input$selectdate)
  })
  output$max2  <- renderText({
    paste0("年最高濕度：", ophren[[input$select2]][grepl(input$selectdate2,TIME),max(CHA)])
  })
  output$min2 <- renderText({
    paste0("年最低濕度：", ophren[[input$select2]][grepl(input$selectdate2,TIME),min(CHA)])
  })
  output$mean2  <- renderText({
    paste0("年平均濕度：", ophren[[input$select2]][grepl(input$selectdate2,TIME),mean(CHA)])
  })
  output$viewtext <-renderText({
    "最佳濕度：40~60"
  })
  output$view  <- renderTable({
    data.table(
      排名= c(1:29),
      地區= data2017[order(ABS)]$AERA,
      平均濕度 = data2017[order(ABS)]$CHA
    )
      })
  output$exPlot<-renderPlot({
    ggplot(ophren[[input$select]][grepl(input$selectdate,TIME)], aes(x = ophren[[input$select]][grepl(input$selectdate,TIME)]$TIME, y = ophren[[input$select]][grepl(input$selectdate,TIME)]$TAVE)) + geom_bar(stat ="identity",width=0.5, fill = "#33CCFF")+geom_text(mapping = aes(label= ophren[[input$select]][grepl(input$selectdate,TIME)]$TAVE),color ="orange",vjust = -0.5, size = 5)+geom_line()+labs( x="溫度", y="月份")+theme(axis.title.y = element_text(angle = 0,size = 17, family = "宋體-繁 細體"),
                                                                                                                                                                                                                                                                                                                                                       axis.text.x = element_text(angle = 0, size = 17,family = "宋體-繁 細體", hjust = 1),
                                                                                                                                                                                                                                                                                                                                                       axis.title.x = element_text(size = 17,family = "宋體-繁 細體"))
    
  })
  output$exPlot2<-renderPlot({
    ggplot(ophren[[input$select2]][grepl(input$selectdate2,TIME)], aes(x = ophren[[input$select2]][grepl(input$selectdate2,TIME)]$TIME, y = ophren[[input$select2]][grepl(input$selectdate2,TIME)]$CHA)) + geom_bar(stat ="identity",width=0.5, fill = "orange")+geom_text(mapping = aes(label= ophren[[input$select2]][grepl(input$selectdate2,TIME)]$CHA),color ="#33CCFF",vjust = -0.5, size = 5)+geom_line()+labs( x="濕度", y="月份")+theme(axis.title.y = element_text(angle = 0,size = 17, family = "宋體-繁 細體"),
                                                                                                                                                                                                                                                                                                                                                                                                                                       axis.text.x = element_text(angle = 0, size = 17,family = "宋體-繁 細體", hjust = 1),
                                                                                                                                                                                                                                                                                                                                                                                                                                       axis.title.x = element_text(size = 17,family = "宋體-繁 細體"))
    
  })
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  

    })