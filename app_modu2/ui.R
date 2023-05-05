
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
           #### questionnaire general part
           tabPanel(title = "About you", value = "p1",
                    mainPanel(
                      numericInput("age","How old are you?",NULL,min=12,max=110,step=1),
                      selectizeInput(
                        'gender', 'What is your gender?', choices = c(    "Female" ="f",
                                                                          "Male" = "m",
                                                                          "Neutral" = "n",
                                                                          "Prefer not to say"="no_answ"),
                        options = list(
                          placeholder = 'Please select an option below',
                          onInitialize = I('function() { this.setValue(""); }')
                        )
                      ),
      
                      selectizeInput("edu","What is the highest level of education you have attained?",c("please select" = NA,
                                                                                                          "Upper secondary education" ="upper_s",
                                                                                                          "Tertiary vocational programme" = "tertiary",
                                                                                                          "Bachelor"="bsc",
                                                                                                          "Master" = "msc",
                                                                                                          "Doctoral degree" = "phd",
                                                                                                          "Prefer not to say"="no_answ"),options = list(
                                                                                                            placeholder = 'Please select an option below',
                                                                                                            onInitialize = I('function() { this.setValue(""); }')
                                                                                                          )
                                     
                                     ),
                      
                      selectizeInput("work","In which economic sector do you currently work?",
                                  c("Mining and quarrying" = "mining",
                                    "Manufacturing"= "man",
                                    "Electricity, gas, steam and air conditioning supply"= "energy",
                                    "Water supply; sewerage, waste management and remediation activities" =  "water",
                                    "Construction" = "cons",
                                    "Distributive trade sector" = "trade",
                                    "Transportation and storage services" = "transport",
                                    "Accommodation and food services" = "accomodation",
                                    "Information and communication services" = "inform",
                                    "Real estate activities" = "real_estate",
                                    "Professional, scientific and technical activities" = "science",
                                    "Administrative and support service activities" = "admin",
                                    "Repair of computers and personal and household goods" = "repair",
                                    "prefer not to say" = "no_w"),options = list(
                                      placeholder = 'Please select an option below',
                                      onInitialize = I('function() { this.setValue(""); }')
                                    )),
                      br(),
                      h5("Click on the postal area where you currently live"),
                      mapedit::selectModUI("map_living"),
                      
                      
                      selectizeInput("liv","How long have you lived in Trondheim area in total?",c(
                                                                                                    " I don`t  live in the Trondheim area" ="no_TRD",
                                                                                                    "Less than 5 years" = "few",
                                                                                                    "5 - 10 years"="more",
                                                                                                    "10 - 20 years" = "more2",
                                                                                                    "more than 20 years" = "all",
                                                                                                    "Prefer not to say"="no_answ"),options = list(
                                                                                                      placeholder = 'Please select an option below',
                                                                                                      onInitialize = I('function() { this.setValue(""); }')
                                                                                                    )),
                      
                      selectizeInput("fam","How familiar are you with the areas in and around Trondheim?",c("poor" ="poor",
                                                                                                             "fair" = "fair",
                                                                                                             "good"="good",
                                                                                                            " I don`t know" = "na"),options = list(
                                                                                                               placeholder = 'Please select an option below',
                                                                                                               onInitialize = I('function() { this.setValue(""); }')
                                                                                                             )),
                      br(),
                      
                      # new environmental paradigm (NEP)
                      
                      radioMatrixInput("matInput2",
                                       rowIDs = c("NEP1","NEP2","NEP3","NEP4"),
                                       rowLLabels =  c("Naturen har en verdi i seg selv",
                                                       "Jeg blir ofte trist når jeg ser større naturinngrep",
                                                       "Naturens egenverdi er viktigere enn bruks- og nytteverdien",
                                                       "Naturens tilstand sier noe om hvem vi er som samfunn og folk."),
                                       choiceNames = c("1", "2", "3","4","5","99"),
                                       # selected = character(0),
                                       selected = c("I don`t know","I don`t know","I don`t know", "I don`t know"),
                                       choices = c("strongly disagree", "agree", "unsure","disagree","strongly disagree", "I don`t know")),
                      selectizeInput("land","If you could choose one of the following landscapes, which would you prefere",c("A well-ordered landscape, made by and for people" ="a",
                                                                                                                             "A varied, park-like landscape. " = "b",
                                                                                                                             "Untamed nature, with which one may have many interactions."="good",
                                                                                                                             "A landscape in which one may experience the greatness and forces of nature." = "na"),options = list(
                                                                                                                               placeholder = 'Please select an option below',
                                                                                                                               onInitialize = I('function() { this.setValue(""); }')
                                                                                                                             )),
                    # actionButton('sub1', 'submit answers')
                    uiOutput("cond_b1")
                    ),

           ),
           tabPanel(title = "Your Task", value = "p2",
                    h5("This explains what you are asked to do in the following task"),
                    checkboxInput('check', 'I have read and understood the instructions',value = FALSE),
                    uiOutput("cond_b2")
           ),
           
           tabPanel(title = "Mapping of ecosystem services I", value = "p3", 
                    mainPanel(
                      #selectInput("map_poss1",label="your choice:",choices = c("B","A")),
                      mapselectUI("mapping1", "map1 #1"),
                      actionButton('sub2', 'next ES')
                      # uiOutput("cond_b3")
                    )),
           tabPanel(title = "Mapping of ecosystem services II", value = "p4", 
                    mainPanel(
                      mapselectUI("mapping2", "map2"),
                      actionButton('sub4', 'next ES')
                      # uiOutput("cond_b4")
                      
                    )),
           tabPanel(title = "Mapping of ecosystem services III", value = "p5", 
                    mainPanel(
                      mapselectUI("mapping3", "map3"),
                      actionButton('sub5', 'end')
                      # uiOutput("cond_b5")
                      
                    ))
           
           
           
)

