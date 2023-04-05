
navbarPage('POC PGIS ES
                 ',id = "inTabset",
                 tabPanel(title = "About you", value = "p1",
                          
                          # surveyOutput(df_all),
                          mainPanel(textInput("userID","enter user id"),
                          numericInput("age","What's your age?",NULL,min=12,max=110,step=1),
                          selectInput("gender","Which best describes your gender?",c("Female" ="f",
                                                                                     "Male" = "m",
                                                                                     "Prefer not to say"="no_answ"),multiple = F),
                          selectInput("edu","What is the highest level of education you have attained?",c("Upper secondary education" ="f",
                                                                                     "Tertiary vocational programme" = "m",
                                                                                     "Bachelor"="bsc",
                                                                                     "Master" = "msc",
                                                                                     "Doctoral degree" = "phd",
                                                                                     "Prefer not to say"="no_answ"),multiple = F),
                          selectInput("liv","How long have you lived in Trondheim area in total?",c(" I don`t  live in the Trondheim area" ="no_TRD",
                                                                                                          "Less than 5 years" = "few",
                                                                                                          "5 - 10 years"="more",
                                                                                                          "10 - 20 years" = "more2",
                                                                                                          "more than 20 years" = "all",
                                                                                                          "Prefer not to say"="no_answ"),multiple = F),
                          selectInput("fam","How familiar are you with the areas in and around Trondheim?",c("poor" ="poor",
                                                                                                    "fair" = "fair",
                                                                                                    "good"="good"),multiple = F),
                          
                          selectModUI("map_living"),
                          radioMatrixInput("matInput",
                                           rowIDs = c("ES1","ES2","ES3"),
                                           rowLLabels =  c("Experience: It enables to experience nature by watching it", 
                                                           "Physical Use: It enables to use nature by biking, hiking, walking in it",
                                                           "Education: It enables to learn about and investigate the environment (education, research)"),
                                           choices = c("I don`t know", "not important at all", "not very important","of medium importance","quite important","very important"),
                                           choiceNames = c("0", "1", "2","3","4","5")),
                          radioMatrixInput("matInput2",
                                           rowIDs = c("NEP1","NEP2","NEP3"),
                                           rowLLabels =  c("We are approaching the limit of the number of people the Earth can support", 
                                                           "Humans have the right to modify the natural environment to suit their needs.",
                                                           "When humans interfere with nature it often produces disastrous consequences."),
                                           choices = c("strongly agree", "agree", "unsure","disagree","strongly disagree"),
                                           choiceNames = c("0", "1", "2","3","4")),
                          
                          actionButton('sub1', 'submit answers')
                          )
                          ),
                 tabPanel(title = paste0("Mapping of ",sel_es_full), value = "p2", 
                          
                          
                          mainPanel(editModUI("map"),
                                    
                                   
                                    sliderInput("access", "Accessibility",
                                                min = 0, max = 5, value = 3
                                    ),
                                    sliderInput("nat", "Naturalness",
                                                min = 0, max = 5, value = 3
                                    ),
                                    sliderInput("lulc", "Landcover",
                                                min = 0, max = 5, value = 3
                                    ),
                                    sliderInput("imp_own", paste0("How important is ",sel_es_full,  " for you personally in this area?"),
                                                min = 0, max = 5, value = 3
                                    ),
                                    sliderInput("imp_other", paste0("How important is ",sel_es_full,  " for others and the society in this area?"),
                                                min = 0, max = 5, value = 3
                                    ),
                                    textInput("es_desc",paste0("Can you describe in a few words what you understand by ",sel_es_full, " as an ES?")),
                                    
                                   
                                    actionButton('sub2', 'submit mapping')
                                    )),
                 tabPanel(title = paste0("Indicate your ranking of ",sel_es_full), value = "p3",
                          leafletOutput(outputId = "map2"),
                          DTOutput("tbl"),
                          actionButton('sub3', 'calculate extrapolation'),),
           
                 tabPanel(title = paste0("your extrapolated map of ",sel_es_full), value = "p4",
                          mainPanel(
                            leafletOutput(outputId = "gee_map")%>% withSpinner(color="#0dc5c1")
                          ))
)