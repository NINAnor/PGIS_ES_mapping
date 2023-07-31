
fluidPage(
  theme = bslib::bs_theme(bootswatch = "cerulean"),
  titlePanel(title =  div(img(src="wendy_logo.png", width ='110'), 'POC mapping ecosystem services'), windowTitle = "ES mapping"),
  tabsetPanel(id = "inTabset",
          ## Study description and user signup
          tabPanel(title = "About the study",
                   value = "p0",
                   mainPanel(
                     h6("Ecosystem services"),
                     "Ecosystem services are services provided by nature. These services have a significant impact on the well-being and health of individuals. But the prosperity and welfare of entire societies also depend on well-functioning ecosystem services. Ecosystems and ecosystem services are affected by economic growth, population increase and changes in climatic conditions. Especially the development of settlements and infrastructures often negatively influences ecosystem services and thus has negative effects for all of us.",
                     br(),
                     br(),
                     h6("Why we need you"),
                     "Not all ecosystem services occure in all areas equally. Their spatial distribution is a complex interaction of soil, air and water. However, to better protect important areas that provide ecosystem services from human interventions, we need to better understand where and which services humans benefit from nature. ",
                     "To find out were important areas for ecosystem services are, we need you. Because you know your homeland, its landscapes and the nature best. You can probably tell us something about many services of the landscape. The whole questionnaire will focus on the following study area:",
                     br(),
                     br(),
                     uiOutput("study_area"),
                     br(),
                     "The study consists of two rounds. This first round will take you max 15min. The second round will take place in a few weeks and might be even shorter. If you want to participate in both rounds please fill in your e-mail, that we can contact you. If you do not provide an e-mail, we are happy that you participate in the first round",
                     br(),
                     textInput(
                       "email",
                       label = tagList(shiny::icon("envelope"), "Email"),
                       value = "",
                       width = NULL,
                       placeholder = NULL
                     ),
                     uiOutput("cond_b0"),
                     actionButton('sub0', 'start', class='btn-primary')
                   )
                ),
          ## general user questionnaire
          tabPanel(title = "About you",
                   value = "p1",
                   mainPanel(
                     numericInput("age",
                                  "How old are you?",
                                  NULL,
                                  min=12,
                                  max=110,
                                  step=1),
                     selectizeInput("gender",
                                    "What is your gender?",
                                    choices = c("Female" ="f",
                                                "Male" = "m",
                                                "Neutral" = "n",
                                                "Prefer not to say"="no_answ"),
                                    options = list(
                                        placeholder = 'Please select an option below',
                                        onInitialize = I('function() { this.setValue(""); }')
                       )
                     ),
                     selectizeInput("edu",
                                    "What is the highest level of education you have attained?",
                                    choices = c("Upper secondary education" ="upper_s",
                                                "Tertiary vocational programme" = "tertiary",
                                                "Bachelor"="bsc",
                                                "Master" = "msc",
                                                "Doctoral degree" = "phd",
                                                "Prefer not to say"="no_answ"),
                                    options = list(
                                      placeholder = 'Please select an option below',
                                      onInitialize = I('function() { this.setValue(""); }'))
                                    
                     ),
                     selectizeInput("work",
                                    "In which economic sector do you currently work?",
                                    choices = c("Mining and quarrying" = "mining",
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
                                                "prefer not to say" = "no_w"),
                                    options = list(
                                        placeholder = 'Please select an option below',
                                        onInitialize = I('function() { this.setValue(""); }')
                                      )),
                     br(),
                     h5("Click on the postal area where you currently live"),
                     "If you don`t live in the area, do not select an area.",
                     br(),
                     mapedit::selectModUI("map_living"),
                     selectizeInput("liv",
                                    "How long have you lived in Trondheim area in total?",
                                    choices = c(" I don`t  live in the Trondheim area" ="no_TRD",
                                                "Less than 5 years" = "few",
                                                "5 - 10 years"="more",
                                                "10 - 20 years" = "more2",
                                                "more than 20 years" = "all",
                                                "Prefer not to say"="no_answ"),
                                    options = list(
                                      placeholder = 'Please select an option below',
                                      onInitialize = I('function() { this.setValue(""); }')
                       )),
                     selectizeInput("fam",
                                    "How familiar are you with the areas in and around Trondheim?",
                                    choices= c("poor" ="poor",
                                               "fair" = "fair",
                                               "good"="good",
                                               "I don`t know" = "dont_know"),
                                    options = list(
                                      placeholder = 'Please select an option below',
                                      onInitialize = I('function() { this.setValue(""); }')
                        )),
                     br(),
                     h5("Your general environmental attitude"),
                     br(),
                     radioMatrixInput("matInput2",
                                      rowIDs = c("NEP1","NEP2","NEP3","NEP4"),
                                      rowLLabels =  c("Naturen har en verdi i seg selv",
                                                      "Jeg blir ofte trist når jeg ser større naturinngrep",
                                                      "Naturens egenverdi er viktigere enn bruks- og nytteverdien",
                                                      "Naturens tilstand sier noe om hvem vi er som samfunn og folk."),
                                      choiceNames = c("1", "2", "3","4","5","99"),
                                      # selected = character(0),
                                      selected = c("I don`t know","I don`t know","I don`t know", "I don`t know"),
                                      choices = c("strongly disagree", "agree", "unsure","disagree","strongly disagree", "I don`t know")
                     ),
                     selectizeInput("land",
                                    "If you could choose one of the following landscapes, which would you prefere",
                                    choices = c("A well-ordered landscape, made by and for people" ="man_made",
                                                "A varied, park-like landscape. " = "park",
                                                "Untamed nature, with which one may have many interactions."="untamed_nat",
                                                "A landscape in which one may experience the greatness and forces of nature." = "compl_nature"),
                                    options = list(
                                      placeholder = 'Please select an option below',
                                      onInitialize = I('function() { this.setValue(""); }')
                                      )
                     ),
                     uiOutput("cond_b1")
                   ) #/main panel
          ), #/tab panel
          
          ## task explanation
          tabPanel(title = "Your Task",
                   value = "p2",
                   mainPanel(
                     uiOutput("task_1"),
                     uiOutput("task_2"),
                     uiOutput("task_3"),
                     uiOutput("task_4"),
                     uiOutput("task_5")


                   )#/main panel
          ), #/tab panel
          
          ## Mapping I
          tabPanel(title = "Mapping of ecosystem services I",
                   value = "p3",
                   mainPanel(
                     mapselectUI("mapping1", "map1 #1")
                   )#/main panel
          ),#/panel
          
          ## Mapping II
          tabPanel(title = "Mapping of ecosystem services II",
                   value = "p4",
                   mainPanel(
                     mapselectUI("mapping2", "map2")
                   ),#/main panel
          ),#/panel
          
          ## Mapping III
          tabPanel(title = "Mapping of ecosystem services III",
                   value = "p5",
                   mainPanel(
                      mapselectUI("mapping3", "map3")
                    )#/main panel
          ),#/panel
          
          ## Pairwise ES section
          tabPanel(title = "Compare ES groups",
                   value = "p6",
            mainPanel(
              ahp_secUI("ahp_section", "ahp #1")
            )#/main panel
          ),#/panel
          
          ## Pairwise ES
          tabPanel(title = "Compare ES",
                   value = "p7",
            mainPanel(
              ahpUI("ahp")
            )#/main panel
          ),#/panel
          
          ## Influence distance
          tabPanel(title = "Impact of wind energy",
                   value = "p8",
            mainPanel(
              "Considering the following four ecosystem services, to which extent do you think a wind turbine could have a negative or positive impact on these?",
              br(),
              sliderTextInput("dist_recr",
                              "impact on recreation value", 
                              grid = F,
                              selected = NULL,
                              force_edges = TRUE,
                              choices = c("2km","4km","6km","8km","10km","12km","14km"),
                              width = "75%"
                              
              ),
              sliderTextInput("dist_aest",
                              "impact on aesthetic values", 
                              grid = F,
                              selected = NULL,
                              force_edges = TRUE,
                              choices = c("2km","4km","6km","8km","10km","12km","14km and more"),
                              width = "75%"
                              
              ),
              sliderTextInput("dist_cult",
                              "impact on cultural values", 
                              grid = F,
                              selected = NULL,
                              force_edges = TRUE,
                              choices = c("2km","4km","6km","8km","10km","12km","14km and more"),
                              width = "75%"
                              
              ),
              sliderTextInput("dist_wild_prod",
                              "impact on wild products", 
                              grid = F,
                              selected = NULL,
                              force_edges = TRUE,
                              choices = c("2km","4km","6km","8km","10km","12km","14km and more"),
                              width = "75%"
                              
              ),
              actionButton('sub8', 'End', class='btn-primary')
            )#/main panel
          )#/panel
  )#/tabset
)#/fluid page