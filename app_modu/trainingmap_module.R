# call module questionnaire
return_trainmap_UI = function(id) {
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


return_trainmap_Server = function(input, output, session) {

  train_param <- reactive({
    list(
      # es_id = sel_es_full,
      access=input$access,
      nat=input$nat,
      lulc = input$lulc,
      imp_own = input$imp_own,
      imp_other = input$imp_other,
      es_desc = input$es_desc
    )
   
  })
   return(train_param)
}