dashboardPage(
  dashboardHeader(title = "POC Spatial Delphi II", titleWidth = 330),
  dashboardSidebar(
    sidebarMenu(id="tabs",
                menuItem("login",tabName="login"),
                menuItemOutput("recOpt")
                
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "login",
              useShinyjs(),
              textInput("email",
                        label = tagList(shiny::icon("envelope"), "Email"),
                        value = "",
                        width = NULL,
                        placeholder = NULL),
              actionButton('sub0', 'login')
      ),
      tabItem("res",
              actionButton('sub1', 'login')  
        
      ),
      tabItem("dash",
              box(plotlyOutput("age")),
              box(plotlyOutput("radar")),
              box(leafletOutput("living"))
      ),
      tabItem("es",
              selectInput("es_individual","recr","recr"),
              #actionButton("sub1","see results"),
              fluidRow(
                box(h3("here is the ES description"))
              ),
              br(),
              fluidRow(
                box(
                  title = "ES map", width = 8, status = "primary",
                  leafletOutput("es_maps")%>% withSpinner(color="#0dc5c1")
                ),
                box(
                  title = span( icon("twitter"), "Reasoning Blog"), status = "warning", width = 4,
                  DTOutput("blog")
                )
              ),
              fluidRow(
                box(
                  title = "General importance of ES",
                  width = 4,
                  plotlyOutput("importance")
                ),
                box(
                  title = "determination parameters",
                  width = 4
                ),
                box(title = "ES comparing to wind energy production",
                    width = 4)
              )


      )
      
    )
  )
)






