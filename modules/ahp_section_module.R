# library(shinyWidgets)
# library(dplyr)
# bq_auth(path = "C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/rgee-381312-85272383f82d.json")
# # connection to bq
# con <- dbConnect(
#   bigrquery::bigquery(),
#   project = "rgee-381312",
#   dataset = "data_base",
#   billing = "rgee-381312"
# )
# 
# es_all<-tbl(con, "es_descr")
# es_all<-select(es_all,esID,esNUM,esDESCR,esNAME,esSECTION)%>%collect()


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

ahp_secServer<-function(id, userID, siteID, es_all){
  moduleServer(
    id,
    function(input, output, session){
      ns<-session$ns
      es_sec<-es_all%>%distinct(esSECTION)
      #randomization
      #es_sec <- es_sec[sample(1:nrow(es_sec)), ] 
      es_sec<-unlist(as.vector(es_sec))
      sec_comb<-as.data.frame(t(combn(es_sec, 2)))

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
        sec_comb$recode <- as.integer(unlist(n))
        sec_comb$userID<-rep(userID,nrow(sec_comb))
        sec_comb$siteID<-rep(siteID,nrow(sec_comb))
        sec_comb$group <- rep(4,nrow(sec_comb))
        sec_comb$group <- as.integer(sec_comb$group)
  
        colnames(sec_comb)<-c("ES_left","ES_right","selection_text","selection_val","userID","siteID", "ahp_section")
        m<-lapply(1:nrow(sec_comb), function(a){
          sec_comb[a,]$es_pairUID <- paste0( sec_comb[a,]$userID, "_", sec_comb[a,]$siteID,"_", sec_comb[a,]$ES_left,"_",sec_comb[a,]$ES_right)
        })
        sec_comb$es_pairUID <- unlist(m)
        insert_upload_job("rgee-381312", "data_base", "es_pair", sec_comb)

      })
      
      
    }
    
  )
}

# # 
# ui <- fluidPage(
#   fluidRow(
#     column(width = 12,
#            ahp_secUI("ahp1", "Counter #1")
#            )
#   )
# 
# 
# )
# 
# server <- function(input, output, session) {
# #   
# 
# 
#   ahp_secServer("ahp1","KcdePm2lep","NOR-SNJ", es_all)
# 
# }
# 
# shinyApp(ui, server)
