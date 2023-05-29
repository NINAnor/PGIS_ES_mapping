
ahp_secUI<- function(id, label = "ahp") {
  ns <- NS(id)
  tagList(
    mainPanel(
      h1("Comparison of landscape benefits"),
      br(),
      h3("Before you are going to compare individual benefits you might gain from the landscape in the study region, you first provide us
         your importance of broad benefit groups. In particular, you will compare the importance of the following three groups within the 
         study region:"),
      h4("  - Cultural landscape benefits: test1"),
      h4("  - Provisioning landscape benefits: test2"),
      h4("  - Regulation landscape benefits: test3"),
      br(),

        uiOutput(ns("slider"))
      ,
      # column(
      #   uiOutput(ns("expl")),
      #   width=4
      # ),

      actionButton(ns('sub'), 'store comparisons')
    )

  )
  
  
}

ahp_secServer<-function(id, es_all, userID){
  moduleServer(
    id,
    function(input, output, session){
      ns<-session$ns

      # es_all<-readRDS("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/es_description.rds")
      
      es_sec<-es_all%>%distinct(section)
      es_sec <- es_sec[sample(1:nrow(es_sec)), ] 
      sec_comb<-as.data.frame(t(combn(es_sec, 2)))
      
      # reg<-es_all%>%filter(section == "regulating")%>%distinct(es_id)
      # reg <- reg[sample(1:nrow(reg)), ] 
      # reg_comb<-combn(reg, 2)
      # 
      # cul<-es_all%>%filter(section == "cultural")%>%distinct(es_id)
      # cul <- cul[sample(1:nrow(cul)), ] 
      # cul_comb<-combn(cul, 2)
      # 
      # prov<-es_all%>%filter(section == "provisioning")%>%distinct(es_id)
      # prov <- prov[sample(1:nrow(prov)), ] 
      # prov_comb<-combn(prov, 2)
      # 

      # comp_es<-rand_es_sel[-c(1:round),]
      ## put compare und non selected values together
      # comp_es<-rbind(comp_es, rand_es_nonSel)
      
      output$slider <- shiny::renderUI({
        ns <- session$ns
        tagList(
          lapply(1:nrow(sec_comb),function(n){
            pair_id <- paste0(sec_comb[n,]$V1,"_",sec_comb[n,]$V2)
            pair_lable<-paste0(sec_comb[n,]$V1," - ",sec_comb[n,]$V2)
            choice1<-paste0(sec_comb[n,]$V1, " is overwhelmingly more important")
            choice2<-paste0(sec_comb[n,]$V1, " is very strongly more important")
            choice3<-paste0(sec_comb[n,]$V1, " is strongly more important")
            choice4<-paste0(sec_comb[n,]$V1, " is moderately more important")

            choice5<-paste0(sec_comb[n,]$V2, " is overwhelmingly more important")
            choice6<-paste0(sec_comb[n,]$V2, " is very strongly more important")
            choice7<-paste0(sec_comb[n,]$V2, " is strongly more important")
            choice8<-paste0(sec_comb[n,]$V2, " is moderately more important")
    
           
            
            sliderTextInput(ns(pair_id),
                            pair_lable, 
                            grid = F,
                            force_edges = TRUE,
                            choices = c(choice1, 
                                        choice2,choice3,choice4, "both are equally important",choice8,choice7,choice6, choice5),
                            width = "75%"
             
            )


          })
          
        )
        
      })
      
      # output$expl   <-shiny::renderUI({
      #   ns <- session$ns
      #   tagList(
      #     lapply(1:nrow(sec_comb),function(m){
      #       h3(paste0("You now compare ",sec_comb[m,]$V1, " with ",sec_comb[m,]$V2))
      #     })
      #   )
      # })
      
      ### store the values
      observeEvent(input$sub,{
        val_list<-list()
        # id_ist<-list()
        res<-lapply(1:nrow(sec_comb),function(a){
       
          
          var<-paste0(sec_comb[a,]$V1,"_",sec_comb[a,]$V2)
          val_list[[a]]<-input[[var]]
          return(val_list)
        })
        comp_val <- unlist(res)
        
        sec_comb$comp_val<-comp_val
        
        sec_comb$recode <- rep(0,nrow(sec_comb))
        
        n<-lapply(1:nrow(sec_comb),function(a){
          if(sec_comb[a,]$comp_val == "both are equally important"){
            sec_comb[a,]$recode <- 1
          } else if(grepl("is overwhelmingly more important",sec_comb[a,]$comp_val) == TRUE){
            sec_comb[a,]$recode <- 8
          } else if(grepl("is very strongly more important",sec_comb[a,]$comp_val) == TRUE){
            sec_comb[a,]$recode <- 6
          } else if(grepl("is strongly more important",sec_comb[a,]$comp_val) == TRUE){
            sec_comb[a,]$recode <- 4
          } else if(grepl("is moderately more important",sec_comb[a,]$comp_val) == TRUE){
            sec_comb[a,]$recode <- 2
          } 
          
          if(grepl(sec_comb[a,]$V1,sec_comb[a,]$comp_val) == TRUE){
            sec_comb[a,]$recode<-sec_comb[a,]$recode*-1
          } else {
            sec_comb[a,]$recode<-sec_comb[a,]$recode
          }
        })
        sec_comb$recode <- unlist(n)
        sec_comb$userID<-rep(userID,nrow(sec_comb))
        colnames(sec_comb)<-c("left","right","comp_val","userID")
        saveRDS(sec_comb,"C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/ahp.RDS")
      })
      
      
    }
    
  )
}


# ui <- fluidPage(
#   fluidRow(
#     column(width = 12,
#            ahpUI("ahp1", "Counter #1")
#            )
#   )
#   
#   
# )
# 
# server <- function(input, output, session) {
# 
#   ahpServer("ahp1", "userID_001")
# 
# }
# 
# shinyApp(ui, server)









