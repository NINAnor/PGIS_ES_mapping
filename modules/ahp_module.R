
# call AHP module
# 
library(dplyr)
library(shinyalert)

# ## import the es description and make a dummy random selection
# es_all<-readRDS("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/es_description.rds")
# rand_es_sel<-es_all%>%slice_sample(n=4, replace = F)
# rand_es_nonSel<-es_all%>%anti_join(rand_es_sel)

ahpUI<- function(id, label = "ahp") {
    ns <- NS(id)
    tagList(
      uiOutput(ns("slider")),
     
      # fluidRow(3,
      #          uiOutput((ns("infotext")))),
    # uiOutput(ns("cond_sub"))
    actionButton(ns('sub'), 'store comparisons')
    )
    
    
}


ahpServer<-function(id, rand_es_sel, rand_es_nonSel, round, userID){
  moduleServer(
    id,
    function(input, output, session){
      ns<-session$ns
      ahp_all<-readRDS("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/ahp.rds")
      
      comp_es<-rand_es_sel[-c(1:round),]
      ## put compare und non selected values together
      comp_es<-rbind(comp_es, rand_es_nonSel)
      
      output$slider <- shiny::renderUI({
        ns <- session$ns
        tagList(
          
            
           lapply(1:nrow(comp_es),function(n){
             # x1 <- round(runif(1, 1, 2))
             # if(x1 == 1){
               pair_id <- paste0(rand_es_sel[round,]$es_id,"_",comp_es[n,]$es_id)
               pair_lable<-paste0(rand_es_sel[round,]$es_name_long," - ",comp_es[n,]$es_name_long)
             # }else{
             #   pair_id <- paste0(comp_es[n,]$es_id,"_",rand_es_sel[round,]$es_id)
             #   pair_lable<-paste0(comp_es[n,]$es_name_long," - ",rand_es_sel[round,]$es_name_long)
             # }
            
      
            
            sliderInput(ns(pair_id),
                        pair_lable, 
                        min = -8, 
                        max = 8, 
                        step = 1, 
                        value = 0)
           

          })
                     

        )
  
})

      ### store the values
      observeEvent(input$sub,{
        val_list<-list()
        # id_ist<-list()
        res<-lapply(1:nrow(comp_es),function(a){
          
          var<-paste0(rand_es_sel[round,]$es_id,"_",comp_es[a,]$es_id)
          val_list[[a]]<-input[[var]]
          return(val_list)
        })
        comp_val <- unlist(res)
        
        comp_es$comp_val<-comp_val
        comp_es$right<-comp_es$es_id
        comp_es$left<-rep(rand_es_sel[round,]$es_id, nrow(comp_es))
        comp_es$userID<-rep(userID,nrow(comp_es))
        comp_es<-comp_es%>%select(userID,comp_val,left,right)
        ahp_all<-rbind(ahp_all,comp_es)
        saveRDS(ahp_all,"C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/ahp.rds")
      })
      
      
    }
      
  )
}

# 
# ui <- fluidPage(
#   h2("the first round"),
#   ahpUI("ahp1", "Counter #1"),
#   h2("the third round"),
#   ahpUI("ahp3", "Counter #1")
# )
# 
# server <- function(input, output, session) {
# 
#   ahpServer("ahp1", rand_es_sel, rand_es_nonSel, 1,"ABC")
#   ahpServer("ahp3", rand_es_sel, rand_es_nonSel, 3,"ABC")
# }
# 
# shinyApp(ui, server)

