#
# OBSERVATORIO DE LAS METAS DEL MILENIO
# @aelorriaga, Septiembre 2016
# CAPTURA DE DATOS PARA ANALIZAR EL CUMPLIMIENTO DE LAS METAS DEL MILENIO
#

library(shiny)
library(shinydashboard)
library(mongolite)
library(twitteR)
library(tm)

# USER INTERFACE ------------------------------------------------------

ui <- dashboardPage(
  
  dashboardHeader(title = "Observatory of Global Sustainability Goals"),
  
  dashboardSidebar(
      
      sidebarMenu(
        menuItem("Overview" , tabName = "overview", icon = icon("tachometer")),
        menuItem("Goal1" , tabName  = "goal1", icon = icon("dot-circle-o")),
        menuItem("Goal2" , tabName  = "goal2", icon = icon("dot-circle-o")),
        menuItem("Goal3" , tabName  = "goal3", icon = icon("dot-circle-o")),
        menuItem("Goal4" , tabName  = "goal4", icon = icon("dot-circle-o")),
        menuItem("Goal5" , tabName  = "goal5", icon = icon("dot-circle-o")),
        menuItem("Goal6" , tabName  = "goal6", icon = icon("dot-circle-o")),
        menuItem("Goal7" , tabName  = "goal7", icon = icon("dot-circle-o")),
        menuItem("Goal8" , tabName  = "goal8", icon = icon("dot-circle-o")),
        menuItem("Goal9" , tabName  = "goal9", icon = icon("dot-circle-o")),
        menuItem("Goal10" , tabName  = "goal10", icon = icon("dot-circle-o")),
        menuItem("Goal11" , tabName  = "goal11", icon = icon("dot-circle-o")),
        menuItem("Goal12" , tabName  = "goal12", icon = icon("dot-circle-o")),
        menuItem("Goal13" , tabName  = "goal13", icon = icon("dot-circle-o")),
        menuItem("Goal14" , tabName  = "goal14", icon = icon("dot-circle-o")),
        menuItem("Goal15" , tabName  = "goal15", icon = icon("dot-circle-o")),
        menuItem("Goal16" , tabName  = "goal16", icon = icon("dot-circle-o")),
        menuItem("Goal17" , tabName  = "goal17", icon = icon("dot-circle-o"))
      )
    ),
  
  dashboardBody(
    
    tabItems(
      
      # Contenido de la seccion Overview ***************************
      tabItem(tabName = "overview",
              fluidRow(column(2,
                              tags$h4("Goal 1")),
                       column(10,
                              uiOutput("uiOverview1"))
                       ),
              
              fluidRow(column(2,
                              tags$h4("Goal 2")),
                       column(10,
                              uiOutput("uiOverview2"))
                      ),
              
              fluidRow(column(2,
                              tags$h4("Goal 3")),
                       column(10,
                              uiOutput("uiOverview3"))
                      ),
              
              fluidRow(column(2,
                              tags$h4("Goal 4")),
                       column(10,
                              uiOutput("uiOverview4"))
                      ),
              
              fluidRow(column(2,
                              tags$h4("Goal 5")),
                       column(10,
                              uiOutput("uiOverview5"))
                      ),
              
              fluidRow(column(2,
                              tags$h4("Goal 6")),
                       column(10,
                              uiOutput("uiOverview6"))
                      )
              ),
      
      # Contenido de la seccion Goal1 ***************************
      tabItem(tabName = "goal1",
              fluidRow(column(2,
                              uiOutput("imageG1")
                              ),
                       column(8,
                              uiOutput("titleG1")
                              )
                      ),
              
              fluidRow(column(6,
                              tags$div(`class`="well well-large",
                                       uiOutput("boxG1"))),
                       column(6,
                              tags$div(`class`="well well-large",
                                       uiOutput("boxG1_1")))
                      ),
              
              fluidRow(column(2,div(style = "height:10px"))),
              uiOutput("uig1")),
      
      # Contenido de la seccion Goal2 ***************************
      tabItem(tabName = "goal2",
              fluidRow(column(2,
                              uiOutput("imageG2")
              ),
              column(8,
                     uiOutput("titleG2")
              )
              ),
              fluidRow(column(2,div(style = "height:10px"))),
              uiOutput("uig2")),
      
      # Contenido de la seccion Goal3 ***************************      
      tabItem(tabName = "goal3",
              fluidRow(column(2,
                              uiOutput("imageG3")
              ),
              column(8,
                     uiOutput("titleG3")
              )
              ),
              fluidRow(column(2,div(style = "height:10px"))),
              uiOutput("uig3")),
      
      # Contenido de la seccion Goal4 ***************************
      tabItem(tabName = "goal4",
              fluidRow(column(2,
                              uiOutput("imageG4")
              ),
              column(8,
                     uiOutput("titleG4")
              )
              ),
              fluidRow(column(2,div(style = "height:10px"))),
              uiOutput("uig4")),
      
      # Contenido de la seccion Goal5 ***************************
      tabItem(tabName = "goal5",
              fluidRow(column(2,
                              uiOutput("imageG5")
              ),
              column(8,
                     uiOutput("titleG5")
              )
              ),
              fluidRow(column(2,div(style = "height:10px"))),
              uiOutput("uig5")),
      
      # Contenido de la seccion Goal6 ***************************      
      tabItem(tabName = "goal6",
              fluidRow(column(2,
                              uiOutput("imageG6")
              ),
              column(8,
                     uiOutput("titleG6")
              )
              ),
              fluidRow(column(2,div(style = "height:10px"))),
              uiOutput("uig6")),
      
      # Contenido de la seccion Goal7 ***************************
      tabItem(tabName = "goal7",
              fluidRow(column(2,
                              uiOutput("imageG7")
              ),
              column(8,
                     uiOutput("titleG7")
              )
              ),
              fluidRow(column(2,div(style = "height:10px"))),
              uiOutput("uig7")),
      
      # Contenido de la seccion Goal8 ***************************      
      tabItem(tabName = "goal8",
              fluidRow(column(2,
                              uiOutput("imageG8")
              ),
              column(8,
                     uiOutput("titleG8")
              )
              ),
              fluidRow(column(2,div(style = "height:10px"))),
              uiOutput("uig8")),
      
      # Contenido de la seccion Goal9 ***************************      
      tabItem(tabName = "goal9",
              fluidRow(column(2,
                              uiOutput("imageG9")
              ),
              column(8,
                     uiOutput("titleG9")
              )
              ),
              fluidRow(column(2,div(style = "height:10px"))),
              uiOutput("uig9")),
      
      # Contenido de la seccion Goal10 ***************************      
      tabItem(tabName = "goal10",
              fluidRow(column(2,
                              uiOutput("imageG10")
              ),
              column(8,
                     uiOutput("titleG10")
              )
              ),
              fluidRow(column(2,div(style = "height:10px"))),
              uiOutput("uig10")),
      
      # Contenido de la seccion Goal11 ***************************      
      tabItem(tabName = "goal11",
              fluidRow(column(2,
                              uiOutput("imageG11")
              ),
              column(8,
                     uiOutput("titleG11")
              )
              ),
              fluidRow(column(2,div(style = "height:10px"))),
              uiOutput("uig11")),
      
      # Contenido de la seccion Goal12 ***************************      
      tabItem(tabName = "goal12",
              fluidRow(column(2,
                              uiOutput("imageG12")
              ),
              column(8,
                     uiOutput("titleG12")
              )
              ),
              fluidRow(column(2,div(style = "height:10px"))),
              uiOutput("uig12")),
      
      # Contenido de la seccion Goal13 ***************************      
      tabItem(tabName = "goal13",
              fluidRow(column(2,
                              uiOutput("imageG13")
              ),
              column(8,
                     uiOutput("titleG13")
              )
              ),
              fluidRow(column(2,div(style = "height:10px"))),
              uiOutput("uig13")),
      
      # Contenido de la seccion Goal14 ***************************      
      tabItem(tabName = "goal14",
              fluidRow(column(2,
                              uiOutput("imageG14")
              ),
              column(8,
                     uiOutput("titleG14")
              )
              ),
              fluidRow(column(2,div(style = "height:10px"))),
              uiOutput("uig14")),
      
      # Contenido de la seccion Goal15 ***************************      
      tabItem(tabName = "goal15",
              fluidRow(column(2,
                              uiOutput("imageG15")
              ),
              column(8,
                     uiOutput("titleG15")
              )
              ),
              fluidRow(column(2,div(style = "height:10px"))),
              uiOutput("uig15")),
      
      # Contenido de la seccion Goal16 ***************************      
      tabItem(tabName = "goal16",
              fluidRow(column(2,
                              uiOutput("imageG16")
              ),
              column(8,
                     uiOutput("titleG16")
              )
              ),
              fluidRow(column(2,div(style = "height:10px"))),
              uiOutput("uig16")),
      
      # Contenido de la seccion Goal17 ***************************      
      tabItem(tabName = "goal17",
              fluidRow(column(2,
                              uiOutput("imageG17")
              ),
              column(8,
                     uiOutput("titleG17")
              )
              ),
              fluidRow(column(2,div(style = "height:10px"))),
              uiOutput("uig17"))
    )
  )
)

# SERVER -------------------------------------------------------------------

server <- shinyServer(function(input, output, session) {
  
  
  # ************************ Configuracion MONGODB *************************
  
  options(mongodb = list(
    "host" = "ds033996.mlab.com:33996",
    "username" = "admin",
    "password" = "admin"
  ))
  
  collectionName <- "goals"
  databaseName <- "observatory"
  
  # Connect to the database
  db <- mongo(
    collection = collectionName,
    url = sprintf(
      "mongodb://%s:%s@%s/%s",
      options()$mongodb$username,
      options()$mongodb$password,
      options()$mongodb$host,
      databaseName
    )
  )
  
  # ************************ Configuracion OAUTH Twitter + MySQL ********************************
  
  consumer_key <- "#####################################"
  consumer_secret <- "#####################################"
  access_token <- "#####################################"
  access_token_secret <- "#####################################"
  
  setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_token_secret)
  
  
 

  #************** Rendering Overview**************************************************************
  
  output$uiOverview1 <- renderUI(tags$div(`class`="progress",
                                        tags$div(`class`="progress-bar",
                                                 `role`="progressbar",
                                                 `aria-valuenow`="70",
                                                 `aria-valuemin`="0",
                                                 `aria-valuemax`="100",
                                                 `style`="width:70%",
                                                 "20%"
                                                )
                                        )
                                )
  
  output$uiOverview2 <- renderUI(tags$div(`class`="progress",
                                          tags$div(`class`="progress-bar",
                                                   `role`="progressbar",
                                                   `aria-valuenow`="70",
                                                   `aria-valuemin`="0",
                                                   `aria-valuemax`="100",
                                                   `style`="width:30%",
                                                   "30%"
                                                  )
                                          )
                                )
  
  output$uiOverview3 <- renderUI(tags$div(`class`="progress",
                                          tags$div(`class`="progress-bar",
                                                   `role`="progressbar",
                                                   `aria-valuenow`="70",
                                                   `aria-valuemin`="0",
                                                   `aria-valuemax`="100",
                                                   `style`="width:30%",
                                                   "60%"
                                                  )
                                          )
                               )
  
  output$uiOverview4 <- renderUI(tags$div(`class`="progress",
                                          tags$div(`class`="progress-bar",
                                                   `role`="progressbar",
                                                   `aria-valuenow`="70",
                                                   `aria-valuemin`="0",
                                                   `aria-valuemax`="100",
                                                   `style`="width:30%",
                                                   "40%"
                                                  )
                                          )
                              )
  
  output$uiOverview5 <- renderUI(tags$div(`class`="progress",
                                          tags$div(`class`="progress-bar",
                                                   `role`="progressbar",
                                                   `aria-valuenow`="70",
                                                   `aria-valuemin`="0",
                                                   `aria-valuemax`="100",
                                                   `style`="width:30%",
                                                   "30%"
                                                  )
                                          )
                              )
  
  output$uiOverview6 <- renderUI(tags$div(`class`="progress",
                                          tags$div(`class`="progress-bar",
                                                   `role`="progressbar",
                                                   `aria-valuenow`="70",
                                                   `aria-valuemin`="0",
                                                   `aria-valuemax`="100",
                                                   `style`="width:30%",
                                                   "10%"
                                          )
  )
  )
    
  # *********************************** RENDERING GOAL 1 ****************************************************************
  
  # EXTRACCION DATOS MONGO: Sacamos la informacion relativa a cada GOAL: el goal, su título y la imagen
  objTitle <- db$distinct("Title", '{"_id":{"$eq":1}}')
  #subobj<- db$find('{"_id": 1, "Objectives.id_obj":"1.1"}, {"Objectives.$":1, _id : 0}')
  imgObj <- db$distinct("Image", '{"_id":{"$eq":1}}')
  
  # CONSULTA A TWITTER: En el prototipo se hace una busqueda con terminos 'hardcoded'
  # Lo ideal sería tener una ONTOLOGIA asociada a cada objetivo con los terminos por los cuales
  # se llevara a cabo la busqueda
  criteria <- "Poverty"
  
  tweets_result = searchTwitter (as.character(criteria),
                                  n=500, lang = "en",
                                  since = as.character (Sys.Date()-5),
                                  until = as.character (Sys.Date()))
  
  count_positive = 0
  count_negative = 0
  count_neutral = 0
  
  
  # ANALISIS DE SENTIMIENTO: Para cada twit recogido analizamos si el texto contiene terminos +, - o neutros
    for (tweet in tweets_result) {
    

      if (grepl("less", tweet$text, ignore.case = TRUE) == TRUE | grepl("fewer", tweet$text, ignore.case = TRUE) | grepl("good", tweet$text, ignore.case = TRUE))
        
      { # Si hay terminos positivos
        
        count_positive = count_positive + 1

      }
      
      else if (grepl("more", tweet$text, ignore.case = TRUE) | grepl("grow", tweet$text, ignore.case = TRUE) | grepl("worse", tweet$text, ignore.case = TRUE))
        
      { # Si hay terminos negativos
        
        count_negative = count_negative + 1

      } else
        
      { # Si hay terminos neutros
        
        count_neutral = count_neutral + 1

      }
  }
  

  # JPG DEL OBJETIVO: renderizamos la imagen del objetivo
  output$imageG1 <- renderUI (tags$img (src=imgObj,
                                         height="80",
                                         width="80"))
  
  # CAJA SUPERIOR IZQUIERDA: INFO GENERAL DE LOS OBJETIVOS
  output$boxG1 <- renderUI (tags$ul (class="list-group",
                                     tags$li (class="list-group-item",
                                              tags$span(class="badge",
                                                        8),
                                              "Number of objectives"),
                                     tags$li (class="list-group-item",
                                              tags$span(class="badge",
                                                        2),
                                              "Objectives with SGDI"),
                                     tags$li (class="list-group-item",
                                              tags$span(class="badge",
                                                        2),
                                              "Clarity"),
                                     tags$li (class="list-group-item",
                                              tags$span(class="badge",
                                                        2),
                                              "measurability"),
                                     tags$li (class="list-group-item",
                                              tags$span(class="badge",
                                                        2),
                                              "observability")
                                     )
                          )
  
  # CAJA SUPERIOR DERECHA: INFO EXTRAIDA DE TWITTER PARA LOS OBJETIVOS
  output$boxG1_1 <- renderUI (tags$ul (class="list-group",
                                     tags$li (class="list-group-item",
                                              tags$span(class="badge",
                                                        count_positive),
                                              "Positive comments"),
                                     tags$li (class="list-group-item",
                                              tags$span(class="badge",
                                                        count_negative),
                                              "Negative comments"),
                                     tags$li (class="list-group-item",
                                              tags$span(class="badge",
                                                        count_neutral),
                                              "Neutral comments")
                                     )
                            )
  
  # LITERAL DEL OBJETIVO
  output$titleG1 <- renderUI (tags$h3 (objTitle))
  
  # LISTA DE SUB-OBJETIVOS COLABSAPBLE
  output$uig1 <- renderUI (tags$div (`class`="panel-group",
                                    `id`="accordion",
                                    `role`="tablist",
                                    `aria-multiselectable`="true",
                                     
                                     tags$div(	`class`="panel panel-default",
                                               
                                               tags$div(	`class`="panel-heading", 
                                                         `role`="tab", 
                                                         `id`="headingOne",
                                                         tags$h4(	`class`="panel-title",
                                                                  tags$a (	`role`="button",
                                                                           `data-toggle`="collapse",
                                                                           `data-parent`="#accordion",
                                                                           `href`="#collapseOne",
                                                                           `aria-expanded`="true",
                                                                           `aria-controls`="collapseOne",
                                                                           "Objective 1.1"
                                                                  )
                                                         )
                                               ),
                                               

                                               
                                               tags$div(	`id`="collapseOne",
                                                         `class`="panel-collapse collapse in",
                                                         `role`="tabpanel",
                                                         `aria-labelledby`="headingOne",
                                                         
                                                         tags$div(	`class`="panel-body",
                                                                   
                                                                   "By 2030, eradicate extreme poverty for all people everywhere, currently measured as people living on less than $1.25 a day",
                                                                   
                                                                   tags$span(`class`="label label-success", "clarity"),
                                                                   tags$span(`class`="label label-success", "measurability"),
                                                                   tags$span(`class`="label label-warning", "observability"),
                                                                   
                                                                  
                                                                   
                                                                   plotOutput("plot", height ="300")
                                                         )
                                               )
                                     ),
                                     
                                     tags$div(	`class`="panel panel-default",
                                               
                                               tags$div(	`class`="panel-heading", 
                                                         `role`="tab",
                                                         `id`="headingTwo",
                                                         tags$h4 (	`class`="panel-title",
                                                                   tags$a(	`class`="collapsed",
                                                                           `role`="button",
                                                                           `data-toggle`="collapse",
                                                                           `data-parent`="#accordion",
                                                                           `href`="#collapseTwo",
                                                                           `aria-expanded`="false",
                                                                           `aria-controls`="collapseTwo",
                                                                           "Objective 1.2"
                                                                   )
                                                         )
                                               ),
                                               
                                               tags$div(	`id`="collapseTwo",
                                                         `class`="panel-collapse collapse",
                                                         `role`="tabpanel",
                                                         `aria-labelledby`="headingTwo",
                                                         
                                                         tags$div(	`class`="panel-body",
                                                                   "By 2030, reduce at least by half the proportion of men, women and children of all ages living in poverty in all its dimensions according to national definitions. This includes: Proportion of population living below the national poverty line by sex and age, and Proportion of men, women and children of all ages living in poverty in all its dimensions according to national definitions"
                                                         )
                                               )
                                     ),
                                     
                                     tags$div(	`class`="panel panel-default",
                                               
                                               tags$div(	`class`="panel-heading",
                                                         `role`="tab",
                                                         `id`="headingThree",
                                                         tags$h4(	`class`="panel-title",
                                                                  tags$a(	`class`="collapsed",
                                                                          `role`="button",
                                                                          `data-toggle`="collapse",
                                                                          `data-parent`="#accordion",
                                                                          `href`="#collapseThree",
                                                                          `aria-expanded`="false",
                                                                          `aria-controls`="collapseThree",
                                                                          "Objective 1.3"
                                                                  )
                                                         )
                                               ),
                                               
                                               tags$div(	`id`="collapseThree",
                                                         `class`="panel-collapse collapse",
                                                         `role`="tabpanel",
                                                         `aria-labelledby`="headingThree",
                                                         
                                                         tags$div(	`class`="panel-body",
                                                                   "Implement nationally appropriate social protection systems and measures for all, including floors, and by 2030 achieve substantial coverage of the poor and the vulnerable"               
                                                         )
                                               )
                                     ),
                                     
                                     
                                     tags$div(	`class`="panel panel-default",
                                               
                                               tags$div(	`class`="panel-heading",
                                                         `role`="tab",	
                                                         `id`="headingFour",
                                                         tags$h4(	`class`="panel-title",
                                                                  tags$a(	`class`="collapsed",
                                                                          `role`="button",
                                                                          `data-toggle`="collapse",
                                                                          `data-parent`="#accordion",
                                                                          `href`="#collapseFour",
                                                                          `aria-expanded`="false",
                                                                          `aria-controls`="collapseFour",
                                                                          "Objective 1.4"
                                                                  )
                                                         )
                                               ),
                                               
                                               tags$div(	`id`="collapseFour",
                                                         `class`="panel-collapse collapse",
                                                         `role`="tabpanel",
                                                         `aria-labelledby`="headingFour",
                                                         
                                                         tags$div(	`class`="panel-body",
                                                                   "By 2030, ensure that all men and women, in particular the poor and the vulnerable, have equal rights to economic resources, as well as access to basic services, ownership and control over land and other forms of property, inheritance, natural resources, appropriate new technology and financial services, including microfinance"
                                                         )
                                               )
                                     ),
                                     
                                     tags$div(	`class`="panel panel-default",
                                               
                                               tags$div(	`class`="panel-heading",
                                                         `role`="tab",
                                                         `id`="headingFive",
                                                         tags$h4(	`class`="panel-title",
                                                                  tags$a(	`class`="collapsed",
                                                                          `role`="button",
                                                                          `data-toggle`="collapse",
                                                                          `data-parent`="#accordion",
                                                                          `href`="#collapseFive",
                                                                          `aria-expanded`="false",
                                                                          `aria-controls`="collapseFive",
                                                                          "Objective 1.5"
                                                                  )
                                                         )
                                               ),
                                               
                                               tags$div(	`id`="collapseFive",
                                                         `class`="panel-collapse collapse",
                                                         `role`="tabpanel",
                                                         `aria-labelledby`="headingFive",
                                                         
                                                         tags$div(	`class`="panel-body",
                                                                   "By 2030, build the resilience of the poor and those in vulnerable situations and reduce their exposure and vulnerability to climate-related extreme events and other economic, social and environmental shocks and disasters"
                                                         )
                                               )
                                     ),
                                     
                                     tags$div(	`class`="panel panel-default",
                                               
                                               tags$div(	`class`="panel-heading",
                                                         `role`="tab",
                                                         `id`="headingSix",
                                                         tags$h4(	`class`="panel-title",
                                                                  tags$a(	`class`="collapsed",
                                                                          `role`="button",
                                                                          `data-toggle`="collapse",
                                                                          `data-parent`="#accordion",
                                                                          `href`="#collapseSix" ,
                                                                          `aria-expanded`="false",
                                                                          `aria-controls`="collapseSix",
                                                                          "Objective 1.a"
                                                                  )
                                                         )
                                               ),
                                               
                                               tags$div(	`id`="collapseSix",
                                                         `class`="panel-collapse collapse",
                                                         `role`="tabpanel",
                                                         `aria-labelledby`="headingSix",
                                                         
                                                         tags$div(	`class`="panel-body",
                                                                   "Ensure significant mobilization of resources from a variety of sources, including through enhanced development cooperation, in order to prov`id`e adequate and predictable means for developing countries, in particular least developed countries, to implement programmes and policies to end poverty in all its dimensions"
                                                         )
                                               )
                                     ),
                                     
                                     tags$div(	`class`="panel panel-default",
                                               
                                               tags$div(	`class`="panel-heading", 
                                                         `role`="tab",
                                                         `id`="headingSeven",
                                                         tags$h4(	`class`="panel-title",
                                                                  tags$a(	`class`="collapsed", 
                                                                          `role`="button",
                                                                          `data-toggle`="collapse",
                                                                          `data-parent`="#accordion",
                                                                          `href`="#collapseSeven",
                                                                          `aria-expanded`="false",
                                                                          `aria-controls`="collapseSeven",
                                                                          "Objective 1.b"
                                                                  )
                                                         )
                                               ),
                                               
                                               tags$div(	`id`="collapseSeven",
                                                         `class`="panel-collapse collapse",
                                                         `role`="tabpanel",
                                                         `aria-labelledby`="headingSeven",
                                                         
                                                         tags$div(	`class`="panel-body",
                                                                   "Create sound policy frameworks at the national, regional and international levels, based on pro-poor and gender-sensitive development strategies, to support accelerated investment in poverty eradication actions"
                                                         )
                                               )
                                     )
                            )
                           )
  
  # GRAFICO DEL OBJETIVO: Se renderiza aplicando el principio de REACTIVIDAD 
  output$plot <- renderPlot({
    
    data <- read.csv("Poverty1_cols.csv", header = TRUE)
    
    plot (data$Value, xlab = "Year", ylab = "millions under 1.25$", ylim = c(0,1500), type="o", col="blue")
    axis(1, 1:7, data$Year)
    
  })
  
    

  #************** Rendering Goal 2**************************************
  objTitle2 <- db$distinct("Title", '{"_id":{"$eq":2}}')
  subobjTitle2 <- db$distinct("Description", '{"id_obj":{"$eq":"1.1"}}')
  imgObj2 <- db$distinct("Image", '{"_id":{"$eq":2}}')
  
  output$imageG2 <- renderUI ( tags$img (src=imgObj2,
                                         height="80",
                                         width="80"))
  
  output$titleG2 <- renderUI (tags$h3 (objTitle2))
  
  #************** Rendering Goal 3**************************************
  objTitle3 <- db$distinct("Title", '{"_id":{"$eq":3}}')
  subobjTitle2 <- db$distinct("Description", '{"id_obj":{"$eq":"1.1"}}')
  imgObj3 <- db$distinct("Image", '{"_id":{"$eq":3}}')
  
  output$imageG3 <- renderUI ( tags$img (src=imgObj3,
                                         height="80",
                                         width="80"))
  
  output$titleG3 <- renderUI (tags$h3 (objTitle3))
  
  #************** Rendering Goal 4**************************************
  objTitle4 <- db$distinct("Title", '{"_id":{"$eq":4}}')
  subobjTitle2 <- db$distinct("Description", '{"id_obj":{"$eq":"1.1"}}')
  imgObj4 <- db$distinct("Image", '{"_id":{"$eq":4}}')
  
  output$imageG4 <- renderUI ( tags$img (src=imgObj4,
                                         height="80",
                                         width="80"))
  
  output$titleG4 <- renderUI (tags$h3 (objTitle4))
  
  #************** Rendering Goal 5**************************************
  objTitle5 <- db$distinct("Title", '{"_id":{"$eq":5}}')
  subobjTitle2 <- db$distinct("Description", '{"id_obj":{"$eq":"1.1"}}')
  imgObj5 <- db$distinct("Image", '{"_id":{"$eq":5}}')
  
  output$imageG5 <- renderUI ( tags$img (src=imgObj5,
                                         height="80",
                                         width="80"))
  
  output$titleG5 <- renderUI (tags$h3 (objTitle5))
  
  
  #************** Rendering Goal 6**************************************
  objTitle6 <- db$distinct("Title", '{"_id":{"$eq":6}}')
  subobjTitle2 <- db$distinct("Description", '{"id_obj":{"$eq":"1.1"}}')
  imgObj6 <- db$distinct("Image", '{"_id":{"$eq":6}}')
  
  output$imageG6 <- renderUI ( tags$img (src=imgObj6,
                                         height="80",
                                         width="80"))
  
  output$titleG6 <- renderUI (tags$h3 (objTitle6))
  
  #************** Rendering Goal 7**************************************
  objTitle7 <- db$distinct("Title", '{"_id":{"$eq":7}}')
  subobjTitle2 <- db$distinct("Description", '{"id_obj":{"$eq":"1.1"}}')
  imgObj7 <- db$distinct("Image", '{"_id":{"$eq":7}}')
  
  output$imageG7 <- renderUI ( tags$img (src=imgObj7,
                                         height="80",
                                         width="80"))
  
  output$titleG7 <- renderUI (tags$h3 (objTitle7))
  
  #************** Rendering Goal 8**************************************
  objTitle8 <- db$distinct("Title", '{"_id":{"$eq":8}}')
  subobjTitle2 <- db$distinct("Description", '{"id_obj":{"$eq":"1.1"}}')
  imgObj8 <- db$distinct("Image", '{"_id":{"$eq":8}}')
  
  output$imageG8 <- renderUI ( tags$img (src=imgObj8,
                                         height="80",
                                         width="80"))
  
  output$titleG8 <- renderUI (tags$h3 (objTitle8))
  
  #************** Rendering Goal 9**************************************
  objTitle9 <- db$distinct("Title", '{"_id":{"$eq":9}}')
  subobjTitle2 <- db$distinct("Description", '{"id_obj":{"$eq":"1.1"}}')
  imgObj9 <- db$distinct("Image", '{"_id":{"$eq":9}}')
  
  output$imageG9 <- renderUI ( tags$img (src=imgObj9,
                                         height="80",
                                         width="80"))
  
  output$titleG9 <- renderUI (tags$h3 (objTitle9))
  
  #************** Rendering Goal 10**************************************
  objTitle10 <- db$distinct("Title", '{"_id":{"$eq":10}}')
  subobjTitle2 <- db$distinct("Description", '{"id_obj":{"$eq":"1.1"}}')
  imgObj10 <- db$distinct("Image", '{"_id":{"$eq":10}}')
  
  output$imageG10 <- renderUI ( tags$img (src=imgObj10,
                                         height="80",
                                         width="80"))
  
  output$titleG10 <- renderUI (tags$h3 (objTitle10))
  
  #************** Rendering Goal 11**************************************
  objTitle11 <- db$distinct("Title", '{"_id":{"$eq":11}}')
  subobjTitle2 <- db$distinct("Description", '{"id_obj":{"$eq":"1.1"}}')
  imgObj11 <- db$distinct("Image", '{"_id":{"$eq":11}}')
  
  output$imageG11 <- renderUI ( tags$img (src=imgObj11,
                                          height="80",
                                          width="80"))
  
  output$titleG11 <- renderUI (tags$h3 (objTitle11))
  
  #************** Rendering Goal 12**************************************
  objTitle12 <- db$distinct("Title", '{"_id":{"$eq":12}}')
  subobjTitle2 <- db$distinct("Description", '{"id_obj":{"$eq":"1.1"}}')
  imgObj12 <- db$distinct("Image", '{"_id":{"$eq":12}}')
  
  output$imageG12 <- renderUI ( tags$img (src=imgObj12,
                                          height="80",
                                          width="80"))
  
  output$titleG12 <- renderUI (tags$h3 (objTitle12))
  
  #************** Rendering Goal 13**************************************
  objTitle13 <- db$distinct("Title", '{"_id":{"$eq":13}}')
  subobjTitle2 <- db$distinct("Description", '{"id_obj":{"$eq":"1.1"}}')
  imgObj13 <- db$distinct("Image", '{"_id":{"$eq":13}}')
  
  output$imageG13 <- renderUI ( tags$img (src=imgObj13,
                                          height="80",
                                          width="80"))
  
  output$titleG13 <- renderUI (tags$h3 (objTitle13))
  
  #************** Rendering Goal 14**************************************
  objTitle14 <- db$distinct("Title", '{"_id":{"$eq":14}}')
  subobjTitle2 <- db$distinct("Description", '{"id_obj":{"$eq":"1.1"}}')
  imgObj14 <- db$distinct("Image", '{"_id":{"$eq":14}}')
  
  output$imageG14 <- renderUI ( tags$img (src=imgObj14,
                                          height="80",
                                          width="80"))
  
  output$titleG14 <- renderUI (tags$h3 (objTitle14))
  
  #************** Rendering Goal 15**************************************
  objTitle15 <- db$distinct("Title", '{"_id":{"$eq":15}}')
  subobjTitle2 <- db$distinct("Description", '{"id_obj":{"$eq":"1.1"}}')
  imgObj15 <- db$distinct("Image", '{"_id":{"$eq":15}}')
  
  output$imageG15 <- renderUI ( tags$img (src=imgObj15,
                                          height="80",
                                          width="80"))
  
  output$titleG15 <- renderUI (tags$h3 (objTitle15))
  
  #************** Rendering Goal 16**************************************
  objTitle16 <- db$distinct("Title", '{"_id":{"$eq":16}}')
  subobjTitle2 <- db$distinct("Description", '{"id_obj":{"$eq":"1.1"}}')
  imgObj16 <- db$distinct("Image", '{"_id":{"$eq":16}}')
  
  output$imageG16 <- renderUI ( tags$img (src=imgObj16,
                                          height="80",
                                          width="80"))
  
  output$titleG16 <- renderUI (tags$h3 (objTitle16))
  
  #************** Rendering Goal 17**************************************
  objTitle17 <- db$distinct("Title", '{"_id":{"$eq":17}}')
  subobjTitle2 <- db$distinct("Description", '{"id_obj":{"$eq":"1.1"}}')
  imgObj17 <- db$distinct("Image", '{"_id":{"$eq":17}}')
  
  output$imageG17 <- renderUI ( tags$img (src=imgObj17,
                                          height="80",
                                          width="80"))
  
  output$titleG17 <- renderUI (tags$h3 (objTitle17))
  
  
})




# APP ---------------------------------------------------------------------

shinyApp (ui = ui, server = server)

