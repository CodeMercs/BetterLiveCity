
theFiles <- dir("data//WRDataUTF8", pattern = "\\.csv")
explot<-list()
for (n in c(1:length(theFiles))) {
  explot[[theFiles[n]]] <-read.csv(paste0("data//WRDataUTF8//", theFiles[n]))
}
shinyUI(
  fluidPage(
    HTML("<!-- Scripts -->
			      <script src=\"assets/js/jquery.min.js\"></script>
            <script src=\"assets/js/skel.min.js\"></script>
            <script src=\"assets/js/util.js\"></script>
            <script src=\"assets/js/main.js\"></script>"),
    HTML("<head>
		        <title>回介紹頁</title>
            <meta charset=\"utf-8\" />
            <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />
            <link rel=\"stylesheet\" href=\"assets/css/main.css\" />
          </head>"),
    
    HTML("<!-- Header -->
		    	  <header id=\"header\">
              <div class=\"inner\">
                <a href=\"index.html\" class=\"logo\"><strong>宜居城市</strong></a>
                <a href=\"#navPanel\" class=\"navPanelToggle\"><span class=\"fa fa-bars\"></span></a>
              </div>
           </header>
          "),
    
    tabsetPanel(id = "tabs",
      tabPanel("首頁",HTML(" <!-- Banner -->
                  <section id=\"banner\">
                         
                         </section>"),
         value = "#panel1"),
      tabPanel("年度地區平均溫度",
               fluidPage(
                 sidebarLayout(
                   sidebarPanel(
                                helpText("資料依年份分組，資料來源：中央氣象局"),           
                                selectInput("select", "選擇地區:", choices=explot[[theFiles[109]]][,1]),
                                selectInput("selectdate", "選擇月份:", choices=unique(substring(theFiles,4,7))),
                                hr()),
                   
                   mainPanel(column(6,
                                    verbatimTextOutput("dateText1"),
                                    verbatimTextOutput("dateText2"),
                                    verbatimTextOutput("max"),
                                    verbatimTextOutput("min"),
                                    verbatimTextOutput("mean")
                   ),plotOutput("exPlot")))), value = "#panel2"),
             
      tabPanel("年度地區平均溼度",
               fluidPage(
                 sidebarLayout( 
                   sidebarPanel(helpText("資料依年份分組，資料來源：中央氣象局"),           
                                selectInput("select2", "選擇地區:", choices=explot[[theFiles[109]]][,1]),
                                selectInput("selectdate2", "選擇月份:", choices=unique(substring(theFiles,4,7))),
                                hr()),
                   mainPanel(column(6,
                                    verbatimTextOutput("dateText21"),
                                    verbatimTextOutput("dateText22"),
                                    verbatimTextOutput("max2"),
                                    verbatimTextOutput("min2"),
                                    verbatimTextOutput("mean2")
                   ),plotOutput("exPlot2"))), value = "#panel3")),
             
      tabPanel("年平均排名",fluidPage(
        verbatimTextOutput("viewtext")
        ,tableOutput("view")
        ), value = "#panel5"),
             
      tabPanel("", value = "#panel6"),
             
      tabPanel("", value = "#panel7"))
    ))