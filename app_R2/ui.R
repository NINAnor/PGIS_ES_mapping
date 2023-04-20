dashboardPage(
  dashboardHeader(title = "APP", titleWidth = 330),
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
              actionButton("sub1","see map"),
              fluidRow(
                box(h3("here is the ES description"))
              ),
              #imp stats
             box(plotlyOutput("importance")),
              #blog
             box(DTOutput("blog")),
              # map
             box(leafletOutput("es_maps"))
   
      )
      
    )
  )
)






