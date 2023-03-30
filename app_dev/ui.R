
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
                          
                          mainPanel(editModUI("map_training"),
                                    DTOutput(
                                      "my_datatable"
                                    ))),
                 tabPanel(title = paste0("your extrapolated map of ",sel_es_full), value = "p3",
                          mainPanel(
                            leafletOutput(outputId = "gee_map")%>% withSpinner(color="#0dc5c1")
                          ))
)