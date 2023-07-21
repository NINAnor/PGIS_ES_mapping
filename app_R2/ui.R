navbarPage("POC Spatial Delphi R2",
           id = "tabset",
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
                        textOutput("login_res"),
                        actionButton('sub0', 'login')

                    )),
           tabPanel(title= "Remapping ES 1", value = "p1",
                    mainPanel(
                      remapUI("remap1")
                    )
                    )


)


