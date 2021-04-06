############Whatsapp app Raymond###########
Sys.setenv(TZ='GMT')
suppressMessages(library(rwhatsapp))
suppressMessages(library(shiny))
suppressMessages(library(shinydashboard))
suppressMessages(library(shinydashboardPlus))
suppressMessages(library(dplyr))
suppressMessages(library(NLP))
suppressMessages(library(ggplot2))
suppressMessages(library(tm)) # text mining
suppressMessages(library(stringr))
suppressMessages(library(SnowballC)) # text stemming
suppressMessages(library(RColorBrewer)) # Color Palettes
suppressMessages(library(wordcloud2))
suppressMessages(library(viridis))
suppressMessages(library(tidytext))
suppressMessages(library(tidylo))
suppressMessages(library(dplyr))
suppressMessages(library(slam))
suppressMessages(library(tidyr))
suppressMessages(library(igraph))
suppressMessages(library(ggraph))
suppressMessages(library(ggiraphExtra))
suppressMessages(library(ggiraph))
suppressMessages(library(widyr))
suppressMessages(library(urltools))
suppressMessages(library(scales))
suppressMessages(library(plotly))
suppressMessages(library(shinycssloaders))
suppressMessages(library(ggplot2))
suppressMessages(library(gganimate))
suppressMessages(library(hrbrthemes))
suppressMessages(library(gifski))
suppressMessages(library(extrafont))
suppressMessages(library(lubridate))
suppressMessages(library(topicmodels))
suppressMessages(library(Rmpfr))
suppressMessages(library(RColorBrewer))
suppressMessages(library(rvest))
suppressMessages(library(purrr))
suppressMessages(library(forcats))
suppressMessages(library(forecast))
suppressMessages(library(visNetwork))
suppressMessages(library(DT))
suppressMessages(library(data.table))  
  


#Loading available fonts
loadfonts()

#Max size of the imported file
options(shiny.maxRequestSize = 1000 * 1024^2)


ui <- dashboardPagePlus(
    dashboardHeader(title = "What-Stat v2.0",titleWidth = 350),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Statistics of messages", tabName = "stats", icon = icon("bar-chart")),
            menuItem("Sentiment Analysis", icon = icon("meh-o"), tabName = "emojis"),
            style = "font-family:Cambria;color:white",
            menuItem("User Network Analysis",icon = icon("cogs"),tabName = "user_net"),
            menuItem("Correlation Analysis", icon = icon("line-chart"), tabName = "corr"),
            menuItem("Topic Modeling", icon = icon("list-ul"), tabName = "topic_mod"),
            menuItem("Message Forecasting", icon = icon("envelope"), tabName = "forc")),

        fileInput("fichier", strong("Please, upload the data file!",  style = "font-family:Cambria;color:white;text-align:center"),
                  accept = c(".zip", ".txt")
        ),
        hr(),
        hr(),
        helpText(strong("By Lucainson RAYMOND"),style = "font-family:Cambria;color:white;text-align:center"),
        hr()
    ),
    dashboardBody(tags$head(tags$style(HTML('.box {margin: 5px;}',
                                            '.main-header .logo {
        font-family: "Georgia", Times, "agency fb", serif;
        font-weight: bold;
        font-size: 20px;
      }
    '))),
                  tags$style(HTML('#my-textInput-id::placeholder {color:black}')),
                  
                  tags$script(HTML("$('body').addClass('fixed');")),
                  
                  #Customizing appearance of the app
                  
                  tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #11533E;
                                }

                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: purple;
                                }

                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #11533E;
                                }

                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #green;
                                }

                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color:#85c769;
                                }

                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color:	#6C5C34;
                                }
                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #FF0000;
                                }
                                
                                 /* body */
                                .content-wrapper, .right-side {
                                background-color: white;
                                }

                                '))),
                  
                  
                  
                  
                  # Set some of the boxes
                  tags$head(tags$style(HTML(".box-header {text-align: center;} "))),
                  tags$head(tags$style(HTML("#col_serie {width:100%;} "))),
                  verticalLayout(
                      #Processing message
                      tags$body(tags$style(type="text/css", "
             #loadmessage {
               position: fixed;
               top: 100px;
               left: 0px;
               width: 100%;
               padding: 4px 0px 3px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: white;
               background-color: green;
               z-index: 100;
             }
          ")),
                      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                       tags$div("Loading...",id="loadmessage"))),
                  
                  
                  #Customizing the sliderinput
                  tags$head(
                      tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: purple}")),
                      tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: red}")),
                      tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: green}")),
                      tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: orange}")),
                      tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: purple}")),
                      tags$style(HTML(".js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar {background: red}")),
                      tags$style(HTML(".js-irs-6 .irs-single, .js-irs-6 .irs-bar-edge, .js-irs-6 .irs-bar {background: green}")),
                      tags$style(HTML(".js-irs-7 .irs-single, .js-irs-7 .irs-bar-edge, .js-irs-7 .irs-bar {background: orange}")),
                      tags$style(HTML(".js-irs-8 .irs-single, .js-irs-8 .irs-bar-edge, .js-irs-8 .irs-bar {background: black}")),
                      tags$style(HTML(".js-irs-9 .irs-single, .js-irs-9 .irs-bar-edge, .js-irs-9 .irs-bar {background: blue}")),
                      tags$style(HTML(".js-irs-10 .irs-single, .js-irs-10 .irs-bar-edge, .js-irs-10 .irs-bar {background: black}")),
                      tags$style(HTML(".js-irs-11 .irs-single, .js-irs-11 .irs-bar-edge, .js-irs-11 .irs-bar {background: green}")),
                      tags$style(HTML(".js-irs-12 .irs-single, .js-irs-12 .irs-bar-edge, .js-irs-12 .irs-bar {background: blue}"))),
                  
                  tabItems(
                      tabItem(tabName = "stats",
                              tabsetPanel(type = 'pills',
                                          id = 'data_messages',
                                          tabPanel("Key numbers",
                                                   br(),
                              fluidRow(
                                column(4,
                                     valueBoxOutput("n_messages",width = "100%")), 
                              column(4,
                                     valueBoxOutput("n_sup_mess",width = "100%")),
                              column(4,
                                     valueBoxOutput("n_author",width = "100%"))),
                              
                              fluidRow(
                                column(6,
                                  box(width=12, height=630, solidHeader = T, background ="black",
                                      title = strong("Chronological series of messages", 
                                                                style = "font-family:Cambria;color:white;margin:auto"),
                                      hr(),
                                      sliderInput("limit_year", "Minimal year to display on the graph:", width="100%",
                                                  min=2009, max= 2021, value=2019, step = 1, sep=""),
                                      style = "font-family:Cambria;color:white;margin:auto",
                                      a(),
                                      plotlyOutput("plot_seri_chrono",height = "430px"))),
                              
                              column(6,
                                  box(width=12, height=630, solidHeader = T, title = strong("Web link shared",
                                      style = "font-family:Cambria;color:white;margin:auto"),
                                      background ="black",
                                      hr(),
                                      sliderInput("max_link",
                                                  "Fix the number of web domain name:",
                                                  min = 5,  max = 20, value = 10, step = 2),
                                      a(),
                                      
                              plotOutput("plot_link_shared",height = "430px")))),
                              
                              a(),
                              
                              fluidRow(
                                  box(width=12, height=670, solidHeader = T, title = strong("Plot of multimedia files shared by Category",
                                      style = "font-family:Cambria;color:white;margin:auto"),
                                      background ="black",
                                      hr(),
                                      radioButtons("tel_type",NULL, c("ios","android"), selected = "ios", inline = T),
                                      plotlyOutput("plot_stat",height = "530px")
                                      )
                                  )
                              ),
                          
                          tabPanel("Activity",
                                   br(),
                                   fluidRow(
                                     column(6,
                                       box(width=12, height=680, solidHeader = F, background="black",title = strong("Top most active",
                                           style = "font-family:Cambria;color:white;margin:auto"),
                                           hr(),
                                                    sliderInput("num_users", "Number of users to display:", width="100%",
                                                                min=2, max= 10, value=2, step = 1),
                                           style = "font-family:times new roman;color:white;margin:auto",
                                           plotOutput("plot_top_users",height = "470px"))),
                                     column(6,
                                            box(width=12, height=680, solidHeader = F, background="black",title = strong("Users' activity depending on the moment of day",
                                                                                                                         style = "font-family:Cambria;color:white;margin:auto"),
                                                hr(),
                                                sliderInput("num_ajust", "Number of users to display:", width="100%",
                                                            min=2, max= 10, value=2, step = 1),
                                                style = "font-family:Cambria;color:white;margin:auto",
                                                plotOutput("plot_moment_jour",height = "470px")))),
                                   fluidRow(
                                   column(12,
                                   radioButtons("freq_ch","Choose a level for both day and hour graphs:", c("Cumulative","Median","Average"), selected = "Cumulative", inline = T))),
                                   style = "font-family:Cambria;color:black;margin:auto",
                                   div(),
                                     
                                     fluidRow(
                                       column(6,
                                              box(width=12, height=590, solidHeader = F, background="black",title = strong("Users' activity by time of day",
                                                  style = "font-family:Cambria;color:white;margin:auto"),
                                                  hr(),
                                                  plotOutput("plot_act_heure",height = "480px"))),
                                       column(6,
                                              box(width=12, height=590, solidHeader = F,background="black", title = strong("Users' activity depending on the day of the week",
                                                  style = "font-family:Cambria;color:white;margin:auto"),
                                                  hr(),
                                                  plotOutput("plot_act_jour",height = "480px")))),
                                   
                                     fluidRow(
                                       box(width="100%", height=600, solidHeader = F,background="black", title = strong("Users' activity according to the time and day of the week",
                                                                                                                        style = "font-family:Cambria;color:white;margin:auto"),
                                       hr(),
                                           plotOutput("plot_heure_jour",height = "485px"))
                                     )),

                      
                      
                      tabPanel("Token Frequency",
                               br(),
                              fluidPage(
                                fluidRow(
                                  column(6,
                                  box(width=12, height=750, solidHeader = F, background="black",
                                      title = strong("Frequency Analysis with a wordcloud",
                                      style = "font-family:Cambria;color:white;margin:auto"),
                                      hr(),
                                               sliderInput("w_cloud_freq", "Minimal frequency of words in the Wordcloud:", width="100%",
                                                           min=5, max= 1000, value=20, step = 5),
                                               style = "font-family:Cambria;color:white;margin:auto",
                                               wordcloud2Output("word_cloud_plot",height = "500px"))
                                  ),
                                  
                                  column(6,
                                         box(width=12, height=750, solidHeader = F, background="black",
                                             title = strong("Lexical Diversity of Users",
                                                            style = "font-family:Cambria;color:white;margin:auto"),
                                             hr(),
                                             sliderInput("max_div",
                                                         "Number of Users to fix:", width="100%",
                                                         min = 2,  max = 10, value = 2, step = 1),
                                             style = "font-family:Cambria;color:white;margin:auto",
                                             plotOutput("plot_div_lex",height = "510px")))
                                
                                ),
                      
                                fluidRow(

                                  
                                  column(6,
                                         box(width=12, height=750, solidHeader = F,background="black", 
                                             title = strong("Histogram of highest frequent tokens",
                                                            style = "font-family:Cambria;color:white;margin:auto"),
                                             hr(),
                                             radioButtons("hist_gram",NULL, c("Uni-gram","Bi-gram", "Tri-gram"), 
                                                          selected = "Uni-gram", inline = T),
                                             style = "font-family:times new roman;color:white;margin:auto",
                                             plotOutput("word_freq_plot", height="610px"))),
                                  
                                column(6,
                                  box(width=12, height=750, solidHeader = F,background="black", title = strong("Most frequent Words of top Users",
                                                                                                               style = "font-family:Cambria;color:white;margin:auto"),
                                      hr(),
                                               radioButtons("word_div_lex",NULL, c("Unweighted Frequency","TF-IDF","Weighted log odds ratio"), selected = "Unweighted Frequency", inline = T),
                                      style = "font-family:times new roman;color:white;margin:auto",
                                               plotOutput("plot_predilec",height = "610px")
                                  )
                      )
                      
                  )
                      
              )      
                      
           )
           
        )
                  
    ),
              

                      
                      
                      tabItem(tabName = "user_net",
                              fluidPage(
                                fluidRow(
                                  box(width=12,title = strong("Some notes..."), status = "warning",
                                  helpText("This network analysis is implemented using the visNetwork package in R. It is about taking into account the network formed by the participants of a group in the conversations. The limit of this analysis remains the fact that the “edges” of the said network are built around the tags in the messages. In other words, the limit lies in the fact that community detection is done for users who tag themselves in conversations. Zoom in on the graph for a better experience.",
                                           align="justify",style = "font-family:times new roman;color:black;margin:auto")),
                                  br(),
                                  box(width=12, height=820, solidHeader = F, background="black",title = strong("Network analysis"),
                                  align="center",style = "font-family:times new roman;color:white;margin:auto",
                                  hr(),
                                  visNetworkOutput("plot_net",height="635px",width = "100%"))))),
                      
                      tabItem(tabName = "emojis",
                              fluidPage(
                                  fluidRow(
                                      box(width=12,title = strong("Some notes..."), status = "warning",
                                      helpText("The quantitative sentiment analysis here results from the cross between the emojis contained in the different messages and the
                      lexicon of feelings called Emoji Sentiment Ranking developed by a group
                      of Russian developers (Kralj Novak P, Smailovic J, Sluban B, Mozetic I (2015) Sentiment of Emojis. PLoS ONE 10 (12): e0144296). The Emoji Sentiment Ranking maps the feelings of emojis most commonly used these days.
Regarding the ranking, the classification process and analysis are detailed
by the authors in this article: https://doi.org/10.1371/journal.pone.0144296",
                                               align="justify",style = "font-family:times new roman;color:black;margin:auto")),
                                      align="justify",style = "font-family:times new roman;color:black;margin:auto",
                                      br(),
                                      fluidRow(
                                        br(),
                                          box(width=12, height=760, solidHeader = F, background="black",title = strong("Sentiment Polarity of Users",
                                                                                                                       style = "font-family:Cambria;color:white;margin:auto"),
                                              hr(),
                                          plotlyOutput("plot_pol_sent",height="650px", width = "100%")))
                                      
                                      
                                  )
                            )
                      ),
                
                      
                      tabItem(tabName = "corr",
                              fluidPage(
                                  fluidRow(
                                    box(width=12,title = strong("Some notes..."), status = "warning",
                                      helpText("This module allows you to analyze the correlations relating to the occurrences of words in the
                               different documents (Whatsapp messages). You can generate the words randomly from your data
                               or type them directly into the search bar. And below, you can visualize at a glance networking between words using Markov chain model.",
                                               align="justify",style = "font-family:Cambria;color:black;margin:auto")),
                                      br(),
                                      fluidRow(
                                        column(6,
                                          textInput(inputId = "word",
                                                    label = "Find word associations in the corpus:", 
                                                    width="100%")), 
                                          style = "font-family:times new roman;color:black;margin:auto",
                                          column(6,
                                          sliderInput("corLimit",
                                                      "Threshold limit for correlation in associations:",width="100%",
                                                      min = .01,  max = 1, value = .10, step = 0.01)),
                                          tags$hr(),
                                          actionButton("randWord", "Generate random word from your data",icon("mouse-pointer"),class = "btn-success",
                                                       style="color: white;backgroud-color: green"),
                                          hr(),
                                          tabPanel(width=12, height=600, solidHeader = F, title = strong("Word correlation table")),
                                          textOutput("corrTableText"),
                                          verbatimTextOutput("plot_corr_table"),
                                          br(),
                                          box(width=12, height=990, solidHeader = F, background="black",title = strong("Word network analysis",
                                                                                                                        style ="font-family:Cambria;color:white;margin:auto"),
                                              hr(),
                                              tabsetPanel(type = 'pills',
                                                          id = 'network_panel',
                                                          tabPanel("Bidirectional network", width=12, 
                                                                   style = "font-family:Cambria;color:white;margin:auto",
                                                                   hr(),
                                                                   sliderInput("limit_gram",
                                                                               "Number of words to display in the network:",width="100%",
                                                                               min = 10,  max = 100, value = 20, step = 10),align="justify",
                                                                   style = "font-family:Cambria;color:white;margin:auto",
                                                                   plotOutput("network_plot", height ="697px")),
                                                          
                                                          tabPanel("Correlation in the network",width=12,
                                                                   style = "font-family:Cambria;color:white;margin:auto",
                                                                   hr(),
                                                                   column(6,
                                                                   sliderInput("limit_cor",
                                                                               "Minimum word correlation threshold:",width="100%",
                                                                               min = 0.2,  max = 1, value = 0.3, step = 0.1)),
                                                                   style = "font-family:Cambria;color:white;margin:auto",
                                                                   column(6,
                                                                   sliderInput("limit_freq_word",
                                                                               "Minimum network word frequency:",width="100%",
                                                                               min = 5,  max = 500, value = 20, step = 5)),
                                                                   style = "font-family:Cambria;color:white;margin:auto",
                                                                   plotOutput("corr_plot",height = "700px"))
                                              )
                                          )
                                       )
                                    )
                                  )
                              ),
                      
                      tabItem(tabName = "topic_mod",
                              fluidPage(
                                  fluidRow(
                                    box(width=12,title = strong("Some notes..."), status = "warning",
                                      helpText("The topic modeling here is built with the LDA (Latent Dirichlet Allocation) model.
                      A fundamental aspect of this modeling is the message generation system is based on a Markov chain,
                      itself based on strings of n-grams (or sequences of words of length n) of the corpus.
The LDA model here is based on the idea that each document in the corpus (or WhatsApp message, in this case) belongs to a number of subjects,
the subjects themselves being composed of a small number of words which are used most often in each of them.
The model is trained multiple times to determine the maximum likelihood that each document (WhatsApp message) belongs to each subject, which is based on the probability that each subject/ word appears in the document given its frequency.
",
                                               align="justify",style = "font-family:Cambria;color:black;margin:auto")),
                                      br(),
                                      fluidRow(
                                        column(4,
                                          sliderInput("times_r", "Number of topics to try:", width="100%",
                                                      min=5, max= 30, value=15, step = 1)),
                                          style = "font-family:C;color:black;margin:auto",
                                          column(4,
                                          sliderInput("n_topic", "Number of topics to display:", width="100%",
                                                      min=1, max= 20, value=8, step = 1)),
                                          column(4,
                                          sliderInput("terms", "Number of terms per subject:", width="100%",
                                                      min=2, max= 15, value=10, step = 1)),
                                          actionButton("newTopics", "Go",icon("mouse-pointer"),class = "btn-success",
                                                       style="color: white;backgroud-color: green"))),
                                  hr(),
                                  fluidRow(
                                  column(12,
                                         
                                         fluidRow(
                                           column(12,
                                                  box(width=12, height=800, solidHeader = F, background="black",
                                                      title = strong("Topic Proportions over Time", style = "font-family:Cambria;color:white;margin:auto"),
                                                      style = "font-family:Cambria;color:white;margin:auto",
                                                      hr(),
                                                      plotlyOutput("plot_topic",height = "690px")))),
                                         br(),
                                         
                                  textOutput("tpcs_number"),
                                  DT::dataTableOutput("plot_topic_table"))),
                                  align="justify",style = "font-family:times new roman;color:black;margin:auto",
                                  
                                  br(),
                                  
                                  fluidRow(
                                  column(6,
                                         DT::dataTableOutput("t_table")
                                         ),
                                  
                                  column(6,
                                         DT::dataTableOutput("d_table")
                                  ))
                                  

                              )),
                      
                      
                      tabItem(tabName = "forc",
                              
                              fluidPage(
                                fluidRow(
                                  box(width=12,title = strong("Some notes..."), status = "warning",
                                  helpText("This involves making forecasts on the number of messages that will be exchanged within a ten (10) day horizon between the users. 
                                           The app implements an ARIMA (Auto Regressive Integrated Moving Average) bagged-model described in Bergmeir et al. (2016). 
                                           In this case, considering the original series (number of messages per day), a bootstrapped series is calculated with the Box-Cox and Loess-based decomposition (BLD) bootstrap. 
                                           This is in order to escape the constraint of the probability distribution of said series. It is very difficult to describe the law of probability of Whatsapp message flow.
                                           So, with the bootstrap method, there is no need to test the normality of the series. In short, the optimal model is selected and estimated automatically using the Hyndman-Khandakar (2008) algorithm to select p and q and the Haslett and Raftery (1989) algorithm to estimate the d parameter.
                                           The Hyndman-Khandakar algorithm uses,among others, a combination of (KPSS) Kwiatkowski-Phillips-Schmidt-Shin test (unit root test), AKAIKE criterion minimization (AICc) & Maximum likelihood estimator (MLE) to obtain an optimal ARIMA model among all candidates (models).
                                                  Note that one of the limitations of our model is that it gives certain prediction intervals (100%). But anyway, that's one of the eventual flaws in any boostrap process.",
                                                  align="justify",style = "font-family:Cambria;color:black;text-align:justify")),
                                  align="justify",style = "font-family:Cambria;color:black",
                                  br(),
                                  column(6,id="col_plot_comp",
                                         
                                         box(width=12, height=500, solidHeader = F,background="black",
                                             
                                             plotlyOutput("plot_comp",height = "470px"))),
                                  column(6,id="col_plot_pred",
                                         box(width=12, height=500, solidHeader = F,background="black",
                                             plotOutput("plot_pred",height = "470px")
                                             
                                         ))),
                                div(),
                                
                                fluidRow(
                                  column(4,id="col_table",
                                         textOutput("summary_table"),
                                         verbatimTextOutput("print_table")),
                                  column(4,id="col_prev",
                                         textOutput("summary_comp"),
                                         DT::dataTableOutput("print_table_comp")),
                                  
                                  column(4,id="col_table_pred",
                                         textOutput("prev_ponc_int"),
                                         DT::dataTableOutput("print_table_pred")),
                                  
                                  style = "font-family:cambria;color:margin:auto"
                                  
                                )
                                
                          )
                              
                              
                      )
                      
                      #tabItem(tabName = "doc",
                              #fluidPage(
                                  #fluidRow(
                                      #box(width=12, height=600, solidHeader = F, title = strong("User manual"))
                               #)
                         #)
                  #)
          )
    )
)
