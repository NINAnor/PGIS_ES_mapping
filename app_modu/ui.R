
navbarPage('POC PGIS ES
                 ',id = "inTabset",
           tabPanel(title = "About the study", value = "p0",
                    mainPanel(h5("This is the study description, goals, aims, why we need you"),
                              h5("The study consists of two rounds ... if you want to participate in both rounds please fill in zour e-mail, if not you can only participate in R1"),
                              textInput("email",
                                        label = tagList(shiny::icon("envelope"), "Email"),
                                        value = "",
                                        width = NULL,
                                        placeholder = NULL),
                               actionButton('sub0', 'start'))
                   
           ),
          tabPanel(title = "About you", value = "p1",
                          return_quest_UI("return_quest"),
                          selectModUI("map_living"),
                          actionButton('sub1', 'submit answers')
                          ),
           
           tabPanel(title = "Mapping of ecosystem services", value = "p2", 
                    mainPanel(
                      ESmoduleUI("es_quest"),
                      editModUI("map"),
                      DTOutput("tbl"),
                      shiny::uiOutput('dyn_form'),
                      textInput("argue","please provide us a short (100 char max), anonymous post where you explain why you choose your site."),
                      actionButton("sub2","save")
                      
                      # editModUI("map"),
                      # actionButton('sub2', 'submit mapping')
                    )),
           # ,
           # tabPanel(title = paste0("Indicate your ranking of ",sel_es_full), value = "p3",
           #          leafletOutput(outputId = "map2"),
           #          DTOutput("tbl"),
           #          actionButton('sub3', 'calculate extrapolation'),),
           # 
           tabPanel(title = paste0("your extrapolated map of "), value = "p3",
                      # geeUI("map_extra")
                      leafletOutput(outputId = "gee_map")%>% withSpinner(color="#0dc5c1"),
                      textOutput("n_img")
                    )
)
           

