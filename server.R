library(shiny)
library("data.table")
require(ggplot2)


testcsvs <- dir("testcsv", pattern = "\\.csv")
testcsvlist <- list()
for (n in c(1:length(testcsvs))) {
  testcsvlist[[substring(testcsvs[n],1,4)]] <-
    data.table(read.table(paste0("testcsv//", testcsvs[n]), header = TRUE, sep = ","))
}


alltable<-data.table(read.table("testcsv//all_data_table.csv", header = TRUE, sep = ","))


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
    paste0("年最高溫度：", alltable[grepl(input$selectdate,TIME)&grepl(input$select,AREA),max(TAVE)])
  })
  output$min  <- renderText({
    paste0("年最低溫度：", alltable[grepl(input$selectdate,TIME)&grepl(input$select,AREA),min(TAVE)])
  })
  output$mean  <- renderText({
    paste0("年平均溫度：", alltable[grepl(input$selectdate,TIME)&grepl(input$select,AREA),mean(TAVE)])
  })
  output$rank  <- renderText({
    paste0("該年平均溫度地區排名：",testcsvlist[[input$selectdate]][order(TAVE_ABS),.(c(1:29),AERA)][grepl(input$select,AERA),V1]
)
  })
  output$dateText21  <- renderText({
    paste0("地區：", input$select2)
  })
  output$dateText22  <- renderText({
    paste0("年份：", input$selectdate2)
  })
  output$max2  <- renderText({
    paste0("年最高濕度：", alltable[grepl(input$selectdate2,TIME)&grepl(input$select2,AREA),max(CHA)])
  })
  output$min2 <- renderText({
    paste0("年最低濕度：",alltable[grepl(input$selectdate2,TIME)&grepl(input$select2,AREA),min(CHA)])
  })
  output$mean2  <- renderText({
    paste0("年平均濕度：",alltable[grepl(input$selectdate2,TIME)&grepl(input$select2,AREA),mean(CHA)])
  })
  output$rank2  <- renderText({
    paste0("該年平均濕度地區排名：",testcsvlist[[input$selectdate2]][order(CHA_ABS),.(c(1:29),AERA)][grepl(input$select2,AERA),V1])
    })
  output$viewtext <-renderText({
    "最佳濕度：40~60"
  })
  output$view  <- renderTable({
    data.table(
      排名= c(1:29),
      地區= testcsvlist[[input$tvSelectDATE]][order(CHA_ABS),]$AERA,
      平均濕度 = testcsvlist[[input$tvSelectDATE]][order(CHA_ABS),]$CHA_MEAN
    )
      })
  #output$exPlot<-renderPlot({
   # ggplot(ophren[[input$select]][grepl(input$selectdate,TIME)], aes(x = ophren[[input$select]][grepl(input$selectdate,TIME)]$TIME, y = ophren[[input$select]][grepl(input$selectdate,TIME)]$TAVE)) + geom_bar(stat ="identity",width=0.5, fill = "#33CCFF")+geom_text(mapping = aes(label= ophren[[input$select]][grepl(input$selectdate,TIME)]$TAVE),color ="orange",vjust = -0.5, size = 5)+geom_line()+labs( x="溫度", y="月份")+theme(axis.title.y = element_text(angle = 0,size = 17, family = "宋體-繁 細體"),
  #                                                                                                                                                                                                                                                                                                                                                     axis.text.x = element_text(angle = 0, size = 17,family = "宋體-繁 細體", hjust = 1),
  #                                                                                                                                                                                                                                                                                                                                                     axis.title.x = element_text(size = 17,family = "宋體-繁 細體"))
  #  
  #})
  #output$exPlot2<-renderPlot({
   # ggplot(ophren[[input$select2]][grepl(input$selectdate2,TIME)], aes(x = ophren[[input$select2]][grepl(input$selectdate2,TIME)]$TIME, y = ophren[[input$select2]][grepl(input$selectdate2,TIME)]$CHA)) + geom_bar(stat ="identity",width=0.5, fill = "orange")+geom_text(mapping = aes(label= ophren[[input$select2]][grepl(input$selectdate2,TIME)]$CHA),color ="#33CCFF",vjust = -0.5, size = 5)+geom_line()+labs( x="濕度", y="月份")+theme(axis.title.y = element_text(angle = 0,size = 17, family = "宋體-繁 細體"),
    #                                                                                                                                                                                                                                                                                                                                                                                                                                   axis.text.x = element_text(angle = 0, size = 17,family = "宋體-繁 細體", hjust = 1),
    #                                                                                                                                                                                                                                                                                                                                                                                                                                   axis.title.x = element_text(size = 17,family = "宋體-繁 細體"))
    #
  #})
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  

    })