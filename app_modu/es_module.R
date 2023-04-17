# call module ES assessment
ESmoduleUI = function(id) {
  ns <- NS(id)
  
  tagList(
                sliderInput(ns("access"), "Accessibility",
                            min = 0, max = 5, value = 3
                ),
                sliderInput(ns("nat"), "Naturalness",
                            min = 0, max = 5, value = 3
                ),
                sliderInput(ns("lulc"), "Landcover",
                            min = 0, max = 5, value = 3
                ),
                sliderInput(ns("imp_own"), paste0("How important is ",sel_es_full,  " for you personally in this area?"),
                            min = 0, max = 5, value = 3
                ),
                sliderInput(ns("imp_other"), paste0("How important is ",sel_es_full,  " for others and the society in this area?"),
                            min = 0, max = 5, value = 3
                ),
                textInput(ns("es_desc"),paste0("Can you describe in a few words what you understand by ",sel_es_full, " as an ES?"))
    )
      
  
}


ESmoduleServer = function(input, output, session, area, n_polys,blog) {
  area = area
  n_polys = n_polys
  blog = blog
  # sel_es_full = sel_es_full
  # userID = userID
  
  train_param <- reactive({
    list(
      #userID = userID,
      # es_id = sel_es_full,
      # argue = argue(),
      access=input$access,
      nat=input$nat,
      lulc = input$lulc,
      imp_own = input$imp_own,
      imp_other = input$imp_other,
      es_desc = input$es_desc,
      area = area,
      n_polys = n_polys,
      blog = blog
    )
   
  })
   # return(train_param)
  train_param<-as.data.frame(({ train_param() }))
  write.csv(train_param,"C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/Questionnaire/shiny_output/train_param_dat.csv",
  row.names = T)
}