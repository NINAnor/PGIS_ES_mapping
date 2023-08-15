fluidPage(
  # theme = bslib::bs_theme(bootswatch = "cerulean"),
  titlePanel(title =  div(img(src="wendy_logo.png", width ='110'), 'POC remapping ecosystem services'), windowTitle = "ES remapping"),
  progressBar(id = "pb1", value = 5, striped = TRUE, title = "Your progress", display_pct = TRUE),
  tabsetPanel(id = "inTabset",
           tabPanel(title = "Overview", value = "p0",
                    mainPanel(
                      useShinyjs(),
                        fluidRow(
                          column(6,
                                 plotlyOutput("age")
                          ),
                          column(6,
                                 plotlyOutput("edu"))

                        ),
                        fluidRow(
                          column(6,
                               plotlyOutput("gen")
                          ),
                          column(6,
                               plotlyOutput("fam"))

                          ),

                        fluidRow(
                          leafletOutput("map_liv")
                        ),
                        textInput("email",
                                label = tagList(shiny::icon("envelope"), "Email"),
                                value = "",
                                width = NULL,
                                placeholder = NULL),
                      br(),
                        textOutput("login_res"),
                        actionButton('sub0', 'login')

                    )),
           tabPanel(title = "Your task", value = "p1",
                    mainPanel(
                      uiOutput("task_1"),
                      uiOutput("task_2"),
                      uiOutput("task_3"),
                      uiOutput("task_4") 
                    )),
           tabPanel(title= "Remapping ES 1", value = "p2",
                    mainPanel(
                      remapUI("remap1")
                    )
                    ),
           tabPanel(title= "Remapping ES 2", value = "p3",
                    mainPanel(
                      remapUI("remap2")
                    )
           ),
           tabPanel(title= "Remapping ES 3", value = "p4",
                    mainPanel(
                      remapUI("remap3")
                    )
           ),
           # tabPanel(title= "Remapping ES 4", value = "p4",
           #          mainPanel(
           #            remapUI("remap4"),
           #            actionButton("sub4","Finish")
           #          )
           # )

  )#/tabset
)#/fluidpage


