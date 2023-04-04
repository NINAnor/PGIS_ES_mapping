
navbarPage('Test leaflet rgee
                 ',id = "inTabset",
                 tabPanel(title = "gen questions", value = "p1",
                          textInput("userID","enter user id"),
                          numericInput("age","your age",NULL,min=12,max=110,step=1),
                          actionButton('sub1', 'submit answers'),
                          mainPanel(selectModUI("map_living"))
                          ),
                 tabPanel(title = paste0("es mapping of ",sel_es_full), value = "p2", 
                          actionButton('sub2', 'submit selection'),
                          
                          mainPanel(editModUI("map"),
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