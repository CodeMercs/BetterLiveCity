
theFiles <- dir("data//WRDataUTF8", pattern = "\\.csv")
explot<-list()
for (n in c(1:length(theFiles))) {
  explot[[theFiles[n]]] <-read.csv(paste0("data//WRDataUTF8//", theFiles[n]))
}
shinyUI(
  fluidPage(
    HTML("<head>
		<title>宜居城市</title>
         <meta charset=\"utf-8\" />
         <meta name=\"viewport\" content=\"width=device-width, initial-scale=1, user-scalable=no\" />
         <link rel=\"stylesheet\" href=\"assets/css/main.css\" />
         </head>
         <body class=\"is-preload\">
         
         <!-- Wrapper -->
         <div id=\"wrapper\">
         
         <!-- Main -->
         <div id=\"main\">
         <div class=\"inner\">
         
         <!-- Header -->
         <header id=\"header\">
         <a href=\"index.html\" class=\"logo\"><strong>Editorial</strong> by HTML5 UP</a>
         <ul class=\"icons\">
         <li><a href=\"#\" class=\"icon fa-twitter\"><span class=\"label\">Twitter</span></a></li>
         <li><a href=\"#\" class=\"icon fa-facebook\"><span class=\"label\">Facebook</span></a></li>
         <li><a href=\"#\" class=\"icon fa-snapchat-ghost\"><span class=\"label\">Snapchat</span></a></li>
         <li><a href=\"#\" class=\"icon fa-instagram\"><span class=\"label\">Instagram</span></a></li>
         <li><a href=\"#\" class=\"icon fa-medium\"><span class=\"label\">Medium</span></a></li>
         </ul>
         </header>
         
         <!-- Banner -->
								<section id=\"banner\">
									<div class=\"content\">
										<header>
											<h1>溫濕度分析</h1>
											<p>使用者可以依據這個系統了解各地區的舒適程度</p>
                      <p>資料來源：中央氣象局</p>
                    </header>
									</div>
								</section>

         <section>
<header class=\"major\">
         </header>
         "),
    sidebarLayout(
      sidebarPanel(
        helpText("資料依年份分組"),           
        selectInput("select", "選擇地區:", choices=explot[[theFiles[109]]][,1]),
        selectInput("selectdate", "選擇年份:", choices=unique(substring(theFiles,4,7))),
        hr()),
      
      mainPanel(verbatimTextOutput("dateText1"),
                verbatimTextOutput("dateText2")
                ,column(6,
                       verbatimTextOutput("max"),
                       verbatimTextOutput("min"),
                       verbatimTextOutput("mean")
                       ,verbatimTextOutput("rank")
      ),column(6,
               verbatimTextOutput("max2"),
               verbatimTextOutput("min2"),
               verbatimTextOutput("mean2"),
               verbatimTextOutput("rank2")
      ))),
    plotOutput("exPlot"),plotOutput("exPlot2"),
      HTML("
         <!-- Section -->
         <section>
         <header class=\"major\">
         <h2>各地區溫濕度排名</h2>
         </header>
         <div class=\"posts\">
         <article>
         "),sidebarLayout(sidebarPanel(helpText("資料依年份分組"),           
                                       selectInput("tvSelectDATE", "選擇年份:", choices=unique(substring(theFiles,4,7))),
                                       hr(),width = 10),
                          mainPanel(tableOutput("view"),width = 18)
                          ),
        HTML("
         </article>
         </div>
         </section>
         
         </div>
         </div>
         
         <!-- Sidebar -->
         <div id=\"sidebar\">
         <div class=\"inner\">
         
         
         
         <!-- Menu -->
         <nav id=\"menu\">
         <header class=\"major\">
         <h2>Menu</h2>
         </header>
         <ul>
         <li><a href=\"index.html\">首頁</a></li>
         <li><a href=\"index.html#section-about\">專案介紹</a></li>
         <li><a href=\"index.html#section-offer\">研究背景</a></li>
         <li><a href=\"index.html#section-menu\">團隊介紹</a></li>
         </ul>
          </nav>
		<!-- Footer -->
								<footer id=\"footer\">
									<p class=\"copyright\">&copy; Untitled. All rights reserved. Demo Images: <a href=\"https://unsplash.com\">Unsplash</a>. Design: <a href=\"https://html5up.net\">HTML5 UP</a>.</p>
								</footer>

						</div>
					</div>

			</div>

		<!-- Scripts -->
			<script src=\"assets/js/jquery.min.js\"></script>
			<script src=\"assets/js/browser.min.js\"></script>
			<script src=\"assets/js/breakpoints.min.js\"></script>
			<script src=\"assets/js/util.js\"></script>
			<script src=\"assets/js/main.js\"></script>

	</body>"),
    HTML(""),
    
    HTML("
          ")
    ))