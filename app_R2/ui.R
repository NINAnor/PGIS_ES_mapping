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
                      remapUI("remap1"),
                      actionButton("sub1","next ES")
                    )
                    ),
           tabPanel(title= "Remapping ES 2", value = "p2",
                    mainPanel(
                      remapUI("remap2"),
                      actionButton("sub2","next ES")
                    )
           ),
           tabPanel(title= "Remapping ES 3", value = "p3",
                    mainPanel(
                      remapUI("remap3"),
                      actionButton("sub3","next ES")
                    )
           ),
           tabPanel(title= "Remapping ES 4", value = "p4",
                    mainPanel(
                      remapUI("remap4"),
                      actionButton("sub4","Finish")
                    )
           )


)


