### mapping module V2

mappingUI = function(id){
  ns <- NS(id)
  tagList(
    mainPanel(
      h3(textOutput(ns("es_title"))),
      textOutput(ns("es_descr")),
      br(),
      selectizeInput(
        ns("mapping_ES"), 'Are you able to show on a map, areas of high and low ES?', choices = c("no","yes"),
        options = list(
          placeholder = 'Please select an option below',
          onInitialize = I('function() { this.setValue(""); }')
        )
      ),
      br(),
      actionButton(ns("sub0"),"save")
    )
  )
}

mapping_server = function(input, output, session, rand_es_sel, userID,sf_bound, comb, geometry){

  
  ## names and description of es
  output$es_title<-renderText(rand_es_sel$es_name_long)
  output$es_descr<-renderText(rand_es_sel$description)
  
  ### mapping condition
  observeEvent(input$mapping_ES, {
    ns = session$ns
    if(input$mapping_ES == "yes"){

      insertUI(
        selector = paste0("#", ns("mapping_ES")),
        where = "afterEnd",
        ui =     mainPanel(h3("Where are good ES regions?"),
                           editModUI(ns("good_map"), width = "500px"),
                           br(),
                           h3("Where are bad ES regions?"),
                           editModUI(ns("bad_map"), width = "500px"),
                           textInput(ns("argue"),"please provide us a short (100 char max), anonymous post where you explain why you choose your site."),
                           
        )
      )
      
      removeUI(
        selector = paste0("#", ns("mapping_ES")),

      )
      
      #radioButtons(ns("expert_map"),label = "Would you trust an expert based map of this ES?",choices = c("yes","no"))
      removeUI(
        selector = paste0("#", ns("expert_map")),
        
      )
 
      

        edits_good  <- callModule(
          module = editMod,
          leafmap = {
            mapv <- mapview::mapview(sf_bound,alpha.regions = 0, popup = NA,verbose =F,label =F, map.types = "CartoDB.Positron")@map
            mapv},
          id = "good_map",
          targetLayerId = 'editLayer',
          sf = TRUE,
          
        )


        edits_bad  <- callModule(
          module = editMod,
          leafmap = {
            mapv <- mapview::mapview(sf_bound,alpha.regions = 0, popup = NA,verbose =F,label =F, map.types = "CartoDB.Positron")@map
            mapv},
          id = "bad_map",
          targetLayerId = 'editLayer',
          sf = TRUE)
          
      
      #edits_<-mapedit::editMap(mapview::mapview(sf_bound,col.regions = NA,map.types = "CartoDB.Positron")@map)
      
      gee_poly<-shiny::eventReactive(input$sub0, {
        
        edits_good<-edits_good()$finished
        edits_bad<-edits_bad()$finished

        # out_good <- sf::st_sf(df_good$data,crs=4326)
        # out_bad <- sf::st_sf(df_bad$data,crs=4326)
        # saveRDS(out_good,"C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/test.rds")

        
        ##remove first col since this is the training dat
        # good<-as.data.frame(edits_good$finished)
        # bad<-as.data.frame(edits_bad$finished)
        
        #value geoms
        edits_good$ES_value<-rep(5,nrow(edits_good))
        edits_bad$ES_value<-rep(1,nrow(edits_bad))
        geom<-rbind(edits_good,edits_bad)
        
        geom$userID<-rep(userID,nrow(geom))
 
        geom<-st_as_sf(geom)
        pol_path <- "C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/output/train_polys_R1"
        st_write(geom, paste0(pol_path, "/",userID,"_recr", ".shp"), delete_layer = TRUE)
        gee_poly<-rgee::sf_as_ee(geom, via = "getInfo")
        
      })
      
      
      prediction <- eventReactive(input$sub0, {
        gee_poly<-gee_poly()
        bands <- c("landcover","be75","landcover_count","slope","slope_mean","aspect")
        
        poly_pts = comb$select(bands)$sampleRegions(
          collection= gee_poly,
          geometries = T
        )
        
        classifier <- ee$Classifier$smileRandomForest(100, NULL, 1,0.5,NULL,0)$setOutputMode("REGRESSION")$train(
          features=poly_pts,
          classProperty= "ES_value",
          inputProperties = list("landcover","be75","landcover_count","slope","slope_mean","aspect")
        )
        
        regression <- comb$select("landcover","be75","landcover_count","slope","slope_mean","aspect")$classify(classifier, "predicted")

      })
      
      # ##saving
      observeEvent(input$sub0, {

        es_ak<-"recr"

        prediction<-prediction()
        assetid <- paste0(ee_get_assethome(), '/rgee/individual_R1_',es_ak,"/", userID)
        task_img <- ee_image_to_asset(
          image = prediction,
          assetId = assetid,
          overwrite = F,
          region = geometry
        )
        
        task_img$start()
        
      })

    }else if (input$mapping_ES == "no"){
      insertUI(
        selector = paste0("#", ns("mapping_ES")),
        where = "afterEnd",
        ui =  
      radioButtons(ns("expert_map"),label = "Would you trust an expert based map of this ES?",choices = c("yes","no")),
      )
      removeUI(
        selector = paste0("#", ns("good_map")),
        
      )
      
      removeUI(
        selector = paste0("#", ns("bad_map")),
        
      )
      
      removeUI(
        selector = paste0("#", ns("argue")),
        
      )
      
      removeUI(
        selector = paste0("#", ns("mapping_ES")),
        
      )
    } else {
      
      removeUI(
        selector = paste0("#", ns("good_map")),
        
      )
      
      removeUI(
        selector = paste0("#", ns("bad_map")),
        
      )
      
      removeUI(
        selector = paste0("#", ns("argue")),
        
      )
      
      removeUI(
        selector = paste0("#", ns("expert_map")),
        
      )
      
    }
    
  })
}