
navbarPage('Test leaflet rgee
                 ',id = "inTabset",
                 tabPanel(title = "gen questions", value = "p1",
                          textInput("userID","enter user id"),
                          actionButton('sub1', 'submit')),
                 tabPanel(title = paste0("es mapping of ",sel_es_full), value = "p2", 
                          actionButton('sub2', 'submit'),
                          
                          mainPanel(editModUI("map"),
                                    DTOutput(
                                      "my_datatable"
                                    ))),
                 tabPanel(title = paste0("your extrapolated map of ",sel_es_full), value = "p3",
                          mainPanel(
                            leafletOutput(outputId = "gee_map")%>% withSpinner(color="#0dc5c1")
                          ))
)