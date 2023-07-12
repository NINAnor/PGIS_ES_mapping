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

ahpUI<- function(id, label = "ahp2") {
  ns <- NS(id)
  tagList(
    mainPanel(
      h1("Comparison of landscape benefits"),
      br(),
      h3("You will now compare the importance of different landscape benefits within the 
         study region. If you need more information about a specific benefit please click on the respective name."),
        br(),
        br(),
        # textOutput(ns("expl1")),
        uiOutput(ns("slider_es1")),
        br(),
        br(),
        # textOutput(ns("expl2")),
        uiOutput(ns("slider_es2")),
        br(),
        br(),
        # textOutput(ns("expl3")),
        uiOutput(ns("slider_es3")),

      actionButton(ns('sub'), 'store comparisons')
    )

  )
  
  
}

ahpServer<-function(id, userID, siteID, es_all){
  moduleServer(
    id,
    function(input, output, session){
      ns<-session$ns

      # es_all<-readRDS("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/es_description.rds")
      reg<-es_all%>%filter(esSECTION == "regulating")%>%distinct(esID)
      reg <- unlist(as.vector(reg))
      reg_comb<-as.data.frame(t(combn(reg, 2)))
      reg_comb$ind<-rep(1,nrow(reg_comb))

      cul<-es_all%>%filter(esSECTION == "cultural")%>%distinct(esID)
      cul <- unlist(as.vector(cul))
      cul_comb<-as.data.frame(t(combn(cul, 2)))
      cul_comb$ind<-rep(2,nrow(cul_comb))

      prov<-es_all%>%filter(esSECTION == "provisioning")%>%distinct(esID)
      prov <- unlist(as.vector(prov))
      prov_comb<-as.data.frame(t(combn(prov, 2)))
      prov_comb$ind<-rep(3,nrow(prov_comb))
      
      all_comb<-rbind(reg_comb,cul_comb,prov_comb)
      all_comb$ind<-as.integer(all_comb$ind)
      

      #randomize the blocks
      # rand<-sample(1:3, 3, replace=F)
      
      # es1<-all_comb%>%filter(ind == rand[1]) 
      # es2<-all_comb%>%filter(ind == rand[2]) 
      # es3<-all_comb%>%filter(ind == rand[3]) 
      
      es1<-all_comb%>%filter(ind == 1) 
      es2<-all_comb%>%filter(ind == 2) 
      es3<-all_comb%>%filter(ind == 3) 
      

      ## first ES block
      output$slider_es1 <- shiny::renderUI({
        ns <- session$ns
        tagList(
          lapply(1:nrow(es1),function(n){
            pair_id <- paste0(es1[n,]$V1,"_",es1[n,]$V2)
            pair_lable<-paste0(es1[n,]$V1," - ",es1[n,]$V2)
            choice1<-paste0(es1[n,]$V1, " is overwhelmingly more important")
            choice2<-paste0(es1[n,]$V1, " is very strongly more important")
            choice3<-paste0(es1[n,]$V1, " is strongly more important")
            choice4<-paste0(es1[n,]$V1, " is moderately more important")

            choice5<-paste0(es1[n,]$V2, " is overwhelmingly more important")
            choice6<-paste0(es1[n,]$V2, " is very strongly more important")
            choice7<-paste0(es1[n,]$V2, " is strongly more important")
            choice8<-paste0(es1[n,]$V2, " is moderately more important")
    
           
            
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
      
      ## second ES block
      output$slider_es2 <- shiny::renderUI({
        ns <- session$ns
        tagList(
          lapply(1:nrow(es2),function(m){
            pair_id2 <- paste0(es2[m,]$V1,"_",es2[m,]$V2)
            pair_lable2<-paste0(es2[m,]$V1," - ",es2[m,]$V2)
            choice1<-paste0(es2[m,]$V1, " is overwhelmingly more important")
            choice2<-paste0(es2[m,]$V1, " is very strongly more important")
            choice3<-paste0(es2[m,]$V1, " is strongly more important")
            choice4<-paste0(es2[m,]$V1, " is moderately more important")
            
            choice5<-paste0(es2[m,]$V2, " is overwhelmingly more important")
            choice6<-paste0(es2[m,]$V2, " is very strongly more important")
            choice7<-paste0(es2[m,]$V2, " is strongly more important")
            choice8<-paste0(es2[m,]$V2, " is moderately more important")
            
            
            
            sliderTextInput(ns(pair_id2),
                            pair_lable2, 
                            grid = F,
                            force_edges = TRUE,
                            choices = c(choice1, 
                                        choice2,choice3,choice4, "both are equally important",choice8,choice7,choice6, choice5),
                            width = "75%"
                            
            )
            
            
          })
          
        )
        
      })

      ## third ES block
      output$slider_es3 <- shiny::renderUI({
        ns <- session$ns
        tagList(
          lapply(1:nrow(es3),function(o){
            pair_id3 <- paste0(es3[o,]$V1,"_",es3[o,]$V2)
            pair_lable3<-paste0(es3[o,]$V1," - ",es3[o,]$V2)
            choice1<-paste0(es3[o,]$V1, " is overwhelmingly more important")
            choice2<-paste0(es3[o,]$V1, " is very strongly more important")
            choice3<-paste0(es3[o,]$V1, " is strongly more important")
            choice4<-paste0(es3[o,]$V1, " is moderately more important")
            
            choice5<-paste0(es3[o,]$V2, " is overwhelmingly more important")
            choice6<-paste0(es3[o,]$V2, " is very strongly more important")
            choice7<-paste0(es3[o,]$V2, " is strongly more important")
            choice8<-paste0(es3[o,]$V2, " is moderately more important")
            
            
            
            sliderTextInput(ns(pair_id3),
                            pair_lable3, 
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
        val_list1<-list()
        val_list2<-list()
        val_list3<-list()

        res1<-lapply(1:nrow(es1),function(a){
          var<-paste0(es1[a,]$V1,"_",es1[a,]$V2)
          val_list1[[a]]<-input[[var]]
          return(val_list1)
        })
        comp_val1 <- unlist(res1)
        
        res2<-lapply(1:nrow(es2),function(b){
          var2<-paste0(es2[b,]$V1,"_",es2[b,]$V2)
          val_list2[[b]]<-input[[var2]]
          return(val_list2)
        })
        comp_val2 <- unlist(res2)
        
        res3<-lapply(1:nrow(es3),function(c){
          var3<-paste0(es3[c,]$V1,"_",es3[c,]$V2)
          val_list3[[c]]<-input[[var3]]
          return(val_list3)
        })
        comp_val3 <- unlist(res3)
        
        es1$comp_val<-comp_val1
        es1$recode <- rep(0,nrow(es1))
        es2$comp_val<-comp_val2
        es2$recode <- rep(0,nrow(es2))
        es3$comp_val<-comp_val3
        es3$recode <- rep(0,nrow(es3))
        
        es<-rbind(es1,es2,es3)
        
        n<-lapply(1:nrow(es),function(a){
          if(es[a,]$comp_val == "both are equally important"){
            es[a,]$recode <- 1
          } else if(grepl("is overwhelmingly more important",es[a,]$comp_val) == TRUE){
            es[a,]$recode <- 8
          } else if(grepl("is very strongly more important",es[a,]$comp_val) == TRUE){
            es[a,]$recode <- 6
          } else if(grepl("is strongly more important",es[a,]$comp_val) == TRUE){
            es[a,]$recode <- 4
          } else if(grepl("is moderately more important",es[a,]$comp_val) == TRUE){
            es[a,]$recode <- 2
          } 
          
          if(grepl(es[a,]$V1,es[a,]$comp_val) == TRUE){
            es[a,]$recode<-es[a,]$recode*-1
          } else {
            es[a,]$recode<-es[a,]$recode
          }
        })
        
        es$recode <- as.integer(unlist(n))
        es$userID<-rep(userID,nrow(es))
        es$siteID<-rep(siteID,nrow(es))
        es<-es%>%select(V1,V2,comp_val,recode,userID,siteID,ind)
        colnames(es)<-c("ES_left","ES_right","selection_text","selection_val","userID","siteID", "ahp_section")
        m<-lapply(1:nrow(es), function(a){
          es[a,]$es_pairUID <- paste0( es[a,]$userID, "_", es[a,]$siteID,"_", es[a,]$ES_left,"_",es[a,]$ES_right)
        })
        es$es_pairUID <- unlist(m)
        insert_upload_job("rgee-381312", "data_base", "es_pair", es)
        
        # all_ahp<-readRDS("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/ahp_es.RDS")
        # all_ahp<-rbind(all_ahp,es)
        # saveRDS(all_ahp,"C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/ahp_es.RDS")
      })
      
      
    }
    
  )
}
# 
# # 
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
#   ahpServer("ahp1", "KcdePm2lep", "NOR-SNJ", es_all)
# 
# }
# 
# shinyApp(ui, server)









