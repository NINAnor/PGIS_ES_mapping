mappingUI = function(id){
  ns <- NS(id)
  tagList(
     mainPanel(
      h3(textOutput(ns("es_title"))),
      textOutput(ns("es_descr")),
      br(),
      ESmoduleUI(ns("es_quest")),
      br(),
      h3("Where are good ES regions?"),
      editModUI(ns("good_map"), width = "500px"),
      br(),
      h3("Where are bad ES regions?"),
      editModUI(ns("bad_map"), width = "500px"),
      br(),
      textInput(ns("argue"),"please provide us a short (100 char max), anonymous post where you explain why you choose your site."),
      br(),
      actionButton(ns("sub_0"),"show my map"),
      br(),
      leafletOutput(outputId = ns("gee_map"), width = "800px")%>% withSpinner(color="#0dc5c1")
    )) 
}

mapping_server = function(input, output, session, rand_es_sel, userID, sf_bound, comb, geometry){
  output$gee_map <- renderLeaflet(NULL)

  rand_es_sel <- rand_es_sel
  userID <- userID
  # output$n_img<-renderText("test tab")
  output$es_title<-renderText(rand_es_sel$es_name_long)
  output$es_descr<-renderText(rand_es_sel$description)
 
      
      map_leaf<-leaflet(sf_bound)%>%setView(lat = 63.426392, lng = 10.397289, zoom = 12)%>%
        addPolygons(color = "orange", weight = 3, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0)%>%
        addProviderTiles(providers$CartoDB.Positron,options = tileOptions(minZoom = 10, maxZoom = 15))%>%
        addDrawToolbar(targetGroup='drawPoly',
                       polylineOptions = F,
                       polygonOptions = F,
                       circleOptions = F,
                       markerOptions = F,
                       circleMarkerOptions = F,
                       rectangleOptions = T,
                       singleFeature = FALSE,
                       editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))
        
      
      edits_good  <- callModule(
        module = editMod,
        leafmap = map_leaf,
        id = "good_map",
        sf = TRUE
      )
      
      edits_bad  <- callModule(
        module = editMod,
        leafmap = map_leaf,
        id = "bad_map",
        sf = TRUE)

      ### polygons
      gee_poly<-shiny::eventReactive(input$sub_0, {

        edits_good<-edits_good()$finished
        edits_bad<-edits_bad()$finished
        

        
        #value geoms
        edits_good$ES_value<-rep(5,nrow(edits_good))
        edits_bad$ES_value<-rep(1,nrow(edits_bad))
        geom<-rbind(edits_good,edits_bad)
        
        geom$userID<-rep(userID,nrow(geom))
        blog <-input$argue
        geom<-st_as_sf(geom)
        area<-sum(st_area(geom))
        ### save args of poly
        callModule(ESmoduleServer, "es_quest",area,nrow(geom),blog, userID, rand_es_sel$es_id)
        
        
        # pol_path <- "C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/output/train_polys_R1"
        # st_write(geom, paste0(pol_path, "/",userID,"_recr", ".shp"), delete_layer = TRUE)
        gee_poly<-rgee::sf_as_ee(geom, via = "getInfo")
        
      })
      
      ### individual prediction
      prediction <- eventReactive(input$sub_0, {
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
      
      observeEvent(input$sub_0,{
        removeUI(
          selector = "div:has(> #good_map)"
        )
 
      })
      
      observeEvent(input$sub_0,{
        removeUI(
          selector = "div:has(> #bad_map)"
        )
        
      })
      
      ### saving on gee
      observeEvent(input$sub_0, {
        
      

        es_ak<-rand_es_sel$es_id

        prediction<-prediction()
        assetid <- paste0(ee_get_assethome(), '/rgee/individual_R1_',es_ak,"/", userID)
        task_img <- ee_image_to_asset(
          image = prediction,
          assetId = assetid,
          overwrite = F,
          region = geometry
        )

        task_img$start()
        
        ### vis parameter for img, mean
        labels <- c("low", "moderate", "intermediate", "high","very high")
        cols   <- c("#e80909", "#fc8803", "#d8e03f", "#c4f25a","#81ab1f")
        vis_qc <- list(min = 1, max = 5, palette = cols, values = labels)
        
        ### vis param for diff map
        labels_diff <- c("low", "moderate", "intermediate", "high","very high")
        cols_diff<-c("#81ab1f","#c4f25a", "#d8e03f", "#fc8803","#e80909")
        vis_diff <- list(min = 0, max = -4, palette = cols_diff,  values = labels_diff)
        
        path<-paste0('users/SPRETO/rgee/individual_R1_',es_ak)
        
        prediction<-ee$Image(prediction)
        gee_poly <- gee_poly()
        
        col_es <- ee$ImageCollection(path)
        # user_img_es_1<-ee$Image(paste0(path1,"/",userID))
        
        mean <- col_es$reduce(ee$Reducer$mean())
        diff <- prediction$subtract(mean)
        
        
        ## vis params in global
        
        Map$setCenter(10.38649, 63.40271,10)
        m1<-Map$addLayer(
          eeObject = prediction,
          vis_qc,
          opacity = 0.4
        ) +Map$addLegend(vis_qc,name = "your map", color_mapping = "character") 
        # +
        #   Map$addLayer(gee_poly, list(color = "blue"), "colored")
        m2<-Map$addLayer(
          eeObject = mean,
          vis_qc,
          opacity = 0.4
        ) +Map$addLayer(
          eeObject = diff,
          vis_diff,
          opacity = 0.4
        ) +Map$addLegend(vis_diff,name = "difference", color_mapping = "character")
        
        map_ind<-m1  | m2
        
        output$gee_map <- renderLeaflet({
          map_ind
        })

      })
      

     
      
      output$n_img <-renderText({
        user_es <- rand_es_sel()
        col <- ee$ImageCollection(paste0('users/SPRETO/rgee/individual_R1_',user_es$es_id))
        n_img<-length(col$getInfo()$features)
        paste0("The crowd map is absed on ", n_img," other participants")
})
    
}