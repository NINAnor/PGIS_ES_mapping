# call AHP module

## import the es description and make a dummy random selection
es_all<-readRDS("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/es_description.rds")
rand_es_sel<-es_all%>%slice_sample(n=4, replace = F)
rand_es_nonSel<-es_all%>%anti_join(rand_es_sel)


ahpUI<- function(id, label = "ahp") {
    ns <- NS(id)
    tagList(
      uiOutput(ns("slider")),
    # uiOutput(ns("cond_sub"))
    actionButton(ns('sub'), 'store comparisons')
    )
    
    
}


ahpServer<-function(id, rand_es_sel, rand_es_nonSel, round){
  moduleServer(
    id,
    function(input, output, session){
      ns<-session$ns
      
      comp_es<-rand_es_sel[-c(1:round),]
      ## put compare und non selected values together
      comp_es<-rbind(comp_es, rand_es_nonSel)
      
      output$slider <- shiny::renderUI({
        ns <- session$ns
        tagList(
          lapply(1:nrow(comp_es),function(n){
            pair_id <- paste0(rand_es_sel[1,]$es_name_long," - ",comp_es[n,]$es_name_long)
      
            sliderInput(ns(id),
                        pair_id, 
                        min = -8, 
                        max = 8, 
                        step = 1, 
                        value = 0)
          })
        )
  
})
      # output$cond_sub<-renderUI({
      #   validate(
      #     need(input$check, 'Please confirm')
      #     
      #   )
      #   actionButton('sub', 'store comparisons')
      # })
      ### store the values
      observeEvent(input$sub,{
        comp_list<-list()
        res<-lapply(1:nrow(comp_es),function(a){
          pair_id <- paste0(rand_es_sel[1,]$es_name_long," - ",comp_es[a,]$es_name_long)
          comp_list[[a]]<-input[[pair_id]]
          return(comp_list)
        })
        vecA <- unlist(res)
        print(vecA)
      })
      
      
    }
      
  )
}


ui <- fluidPage(
  h2("the first round"),
  ahpUI("ahp1", "Counter #1"),
  h2("the third round"),
  ahpUI("ahp3", "Counter #1")
)

server <- function(input, output, session) {

  ahpServer("ahp1", rand_es_sel, rand_es_nonSel, 1)
  ahpServer("ahp3", rand_es_sel, rand_es_nonSel, 3)
}

shinyApp(ui, server)

