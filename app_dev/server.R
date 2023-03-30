function(input, output, session) {
  ## hiding all tabs but not start
  hideTab(inputId = "inTabset", target = "p2")
  hideTab(inputId = "inTabset", target = "p3")
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
  ## save map results and send it to gee
  
  edits <- callModule(
    editMod,
    leafmap = map,
    id = "map"
  )
  
  ## create gee polygon as soon as submit button 2 is pressed
  gee_poly<-eventReactive(input$sub2, {
    geom <- edits()$finished
    
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
    geom$rec_val<-c(1,2,3)
    gee_poly<-rgee::sf_as_ee(geom, via = "getInfo")
    
    
  })
  
  
  
  output$my_datatable <- renderDT({
    
    edits()$finished %>% 
      datatable()
    
    
  })
  
  
  prediction <- eventReactive(input$sub2, {
    gee_poly<-gee_poly()
    bands <- c("landcover","be75","landcover_count","slope","slope_mean","aspect")
    
    poly_pts = comb$select(bands)$sampleRegions(
      collection= gee_poly,
      geometries = T
    )
    
    classifier <- ee$Classifier$smileRandomForest(100, NULL, 1,0.5,NULL,0)$setOutputMode("REGRESSION")$train(
      features=poly_pts,
      classProperty= "rec_val",
      inputProperties = list("landcover","be75","landcover_count","slope","slope_mean","aspect")
    )
    
    regression <- comb$select("landcover","be75","landcover_count","slope","slope_mean","aspect")$classify(classifier, "predicted")
    
    
  })
  
  observeEvent(input$sub2, {
    
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
  
  map2 <- eventReactive(input$sub2,{
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
    map2()
  })
  
  
}
