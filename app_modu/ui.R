
navbarPage('POC PGIS ES
                 ',id = "inTabset",
           tabPanel(title = "About the study", value = "p0",
                    mainPanel(h5("This is the study description, goals, aims, why we need you"),
                              h5("The study consists of two rounds ... if you want to participate in both rounds please fill in your e-mail, if not you can only participate in R1"),
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
          tabPanel(title = "Your Task", value = "p2",
                   h5("This explains what you are are asked to to in the following task"),
                   actionButton('sub3', 'go to task')
          ),
           
          tabPanel(title = "Mapping of ecosystem services I", value = "p3", 
                    mainPanel(
                      h3(textOutput("es_title")),
                      textOutput("es_descr"),
                      br(),
                      ESmoduleUI("es_quest"),
                      # trainingUI("es_train"),
                      editModUI("map"),
                      DTOutput("tbl"),
                      shiny::uiOutput('dyn_form'),
                      textInput("argue","please provide us a short (100 char max), anonymous post where you explain why you choose your site."),
                      actionButton("sub2","save")
                    )),
          tabPanel(title = "Mapping of ecosystem services II", value = "p4", 
                   mainPanel(
                     h3(textOutput("es_title2")),
                     textOutput("es_descr2"),
                     # br(),
                     # ESmoduleUI("es_quest2"),
                     # editModUI("map2"),
                     # DTOutput("tbl2"),
                     # shiny::uiOutput('dyn_form2'),
                     textInput("argue2","please provide us a short (100 char max), anonymous post where you explain why you choose your site."),
                     actionButton("sub4","show results")
                   )),
  

           tabPanel(title = paste0("your extrapolated map of "), value = "p5",
                      leafletOutput(outputId = "gee_map")%>% withSpinner(color="#0dc5c1"),
                      textOutput("n_img")
                    )
)
           

