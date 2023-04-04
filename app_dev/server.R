function(input, output, session) {
  ## hiding all tabs but not start
  hideTab(inputId = "inTabset", target = "p2")
  hideTab(inputId = "inTabset", target = "p3")
  hideTab(inputId = "inTabset", target = "p4")
  ## submit, switch and remove tab one
  observeEvent(input$sub1, {
    updateTabsetPanel(session, "inTabset",
                      selected = "p2")
  })
  observeEvent(input$sub1, {
    hideTab(inputId = "inTabset", target = "p1")
  })
  observeEvent(input$sub1, {
    showTab(inputId = "inTabset", target = "p2")
    
  })
  
  
  ## submit switch to tab 3 and remove others
  observeEvent(input$sub2, {
    updateTabsetPanel(session, "inTabset",
                      selected = "p3")
  })
  observeEvent(input$sub2, {
    hideTab(inputId = "inTabset",
            target = "p2")
  })
  observeEvent(input$sub2, {
    showTab(inputId= "inTabset",
            target = "p3")
  })
  
  
  ## submit switch to tab 4 and remove others
  observeEvent(input$sub3, {
    updateTabsetPanel(session, "inTabset",
                      selected = "p4")
  })
  observeEvent(input$sub3, {
    hideTab(inputId = "inTabset",
            target = "p3")
  })
  observeEvent(input$sub3, {
    showTab(inputId= "inTabset",
            target = "p4")
  })
  
  
  

  
  liv_pol <- callModule(selectMod, "map_living", 
                        leaflet() %>% 
                          addProviderTiles(provider= "CartoDB.Positron")%>%setView(10.42,63.44,10)%>%
                          addFeatures(st_sf(grd), layerId = ~seq_len(length(grd)))
                        
                        
  )
  
##select poly of living 
observeEvent(input$sub1, { 
     gs<-liv_pol()
     gs<-st_sf(grd[as.numeric(gs[which(gs$selected==TRUE),"id"])])

    cent<-st_centroid(gs)
    time_sub<-Sys.time()
    userID = input$userID
    age= input$age
    
    ## write user data to file
    data <- data.frame(time=time_sub, userID=userID, age=age, user_live_lng =st_coordinates(cent)[1], user_live_lat = st_coordinates(cent)[2])
    write.csv(data,"C:/Users/reto.spielhofer/git/PGIS_ES_mapping/test_user_out/user_dat.csv",
              row.names = T)
    
    })


  training_pol <- callModule(
    editMod,
    leafmap = mapview(map.types = "CartoDB.Positron")@map %>% leafem::addFeatures(data = data_copy,
                                                                                  layerId = data_copy$leaf_id,
                                                                                  group = 'editLayer',
                                                                                  popup = leafpop::popupTable(data_copy)),
    id = "map",
    targetLayerId = 'editLayer',
    sf = TRUE,
    editorOptions = list(editOptions = leaflet.extras::editToolbarOptions(edit = TRUE)),
  )
  
  table_edit <- eventReactive(input$sub2,{
    training_pol<-training_pol()$finished
    training_pol<-as.data.frame(training_pol)
    training_pol$ES_ranking<-rep(0,nrow(training_pol))
    training_pol$Comments<-rep(0,nrow(training_pol))
    training_pol$confidence<-rep(0,nrow(training_pol))
    training_pol<-st_as_sf(training_pol)
    
  

  })

  output$tbl <- renderDT(
    table_edit()
)

  

output$map2<-renderLeaflet(
  leaflet(training_pol()$finished)%>%addProviderTiles("CartoDB.Positron")%>%
    addPolygons(color = "green", label = training_pol()$finished$leaf_id)
)
  
 

  ## create gee polygon as soon as submit button 3 is pressed
  gee_poly<-eventReactive(input$sub3, {
    geom <- training_pol()$finished
    
    # if (!is.null(geom)) {
    #   assign('user_geom', geom, envir = .GlobalEnv)
    #   sf::write_sf(
    #     geom,
    #     'user_geom.geojson',
    #     delete_layer = T,
    #     delete_dsn = T
    #   )
    # }
    geom<-as.data.frame(geom)
    
    geom<-st_as_sf(geom)
    # geom$rec_val<-c(1,2,3)
    gee_poly<-rgee::sf_as_ee(geom, via = "getInfo")
    
    
  })
  
  
  
 
  
  
  prediction <- eventReactive(input$sub3, {
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
  
  observeEvent(input$sub3, {
    
    prediction<-prediction()
    userid <- input$userID
    assetid <- paste0(ee_get_assethome(), '/rgee/individual_R1_',sel_es_ab,"/",userid)
    task_img <- ee_image_to_asset(
      image = prediction,
      assetId = assetid,
      overwrite = T,
      region = geometry
    )
    
    task_img$start()
    
    
  })
  
  map3 <- eventReactive(input$sub3,{
    prediction<-prediction()
    gee_poly <- gee_poly()
    Map$setCenter(10.38649, 63.40271,10)
    Map$addLayer(
      eeObject = prediction,
      list(min = 1, max=3),
      name = "prediction ee"
    )+Map$addLayer(gee_poly, list(color = "red"), "colored")
  },
  ignoreNULL = FALSE
  )
  
  
  
  output$gee_map <- renderLeaflet({
    map3()
  })
  
  
}
