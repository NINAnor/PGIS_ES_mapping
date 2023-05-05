# call mapping module

mapeditUI<- function(id, label = "selector") {
  ns <- NS(id)
  tagList(
    editModUI(ns("map_edit")),
    actionButton(ns("submit"),"save changes")

  )
  
}



mapeditServer<-function(id, es_id, userID, vis_qc){
  moduleServer(
    id,
    function(input, output, session){
      ns<-session$ns
      
      poly_path1<-paste0("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/poly_R1/",es_id,"_",userID,".shp")
      poly_r1<-st_read(poly_path1)
      poly_r1<-st_as_sf(poly_r1)
      poly_r1 <- st_transform(
        poly_r1, 
        crs = 4326
      )
      cent_poly <- st_centroid(poly_r1)
      
      ##raster
      imgpath1<-paste0(ee_get_assethome(), '/rgee/individual_R1_',es_id,"/",userID)
      img_ind1<-ee$Image(imgpath1)
      

      Map$setCenter(10.38649, 63.40271,10)
      m1<-Map$addLayer(
        eeObject = img_ind1,
        vis_qc,
        opacity = 0.4
      )+Map$addLegend(vis_qc,name = "prediction", color_mapping = "character")
      

     map<-leaflet(poly_r1)%>%
        addPolygons(color = "orange", weight = 3, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0, group = "editable")%>%
       addLabelOnlyMarkers(data = cent_poly,
                           lng = ~st_coordinates(cent_poly)[,1], lat = ~st_coordinates(cent_poly)[,2], label = cent_poly$es_valu,
                           labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE,
                                                       style = list(
                                                         "color" = "red",
                                                         "font-family" = "serif",
                                                         "font-style" = "bold",
                                                         "font-size" = "20px"
                                                       )))%>%
        addProviderTiles(providers$CartoDB.Positron,options = tileOptions(minZoom = 10, maxZoom = 15))%>%
        addDrawToolbar(targetGroup='editable',
                       polylineOptions = F,
                       polygonOptions = F,
                       circleOptions = F,
                       markerOptions = F,
                       circleMarkerOptions = F,
                       rectangleOptions = F,
                       singleFeature = FALSE,
                       editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))+m1

     #edits<-editMap(map, targetLayerId = "editable")
      
      edits<-callModule(
        module = editMod,
        leafmap = map,
        id = "map_edit",
        targetLayerId = "editable")

      
      gee_poly<-eventReactive(input$submit,{
        # tbl<-tbl_out()
        polygon<-edits()$finished
        
          
          polypath <- paste0( 'C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/poly_R2/',es_id,"_",userID,".shp")
          ## save poly
          st_write(polygon,polypath)
          
          gee_poly<-rgee::sf_as_ee(polygon, via = "getInfo")
          
        
      })
      
      prediction<-eventReactive(input$submit,{
        gee_poly<-gee_poly()
        #### earth engine part
        bands <- c("landcover","be75","landcover_count","slope","slope_mean","aspect")
        
        poly_pts = comb$select(bands)$sampleRegions(
          collection= gee_poly,
          geometries = T
        )
        
        classifier <- ee$Classifier$smileRandomForest(100, NULL, 1,0.5,NULL,0)$setOutputMode("REGRESSION")$train(
          features=poly_pts,
          classProperty= "es_value",
          inputProperties = list("landcover","be75","landcover_count","slope","slope_mean","aspect")
        )
        
        regression <- comb$select("landcover","be75","landcover_count","slope","slope_mean","aspect")$classify(classifier, "predicted")
        
        #rand_es_sel <- rand_es_sel()
        es_ak<-es_ak
        userID <- userID
        # prediction<-prediction()
        assetid <- paste0(ee_get_assethome(), '/rgee/individual_R2_',es_id,"/", userID)
        print(assetid)
        start_time<-Sys.time()
        task_img <- ee_image_to_asset(
          image = regression,
          assetId = assetid,
          overwrite = F,
          region = geometry
        )
        
        task_img$start()
        prediction<-regression
        
      })
      
      # map_ind<-eventReactive(input$submit,{
      #   prediction<-prediction()
      #   gee_poly <- gee_poly()
      #   
      #   Map$setCenter(10.38649, 63.40271,10)
      #   m1<-Map$addLayer(
      #     eeObject = prediction,
      #     vis_qc,
      #     opacity = 0.4
      #   ) +Map$addLegend(vis_qc,name = "prediction", color_mapping = "character") +
      #     Map$addLayer(gee_poly, list(color = "blue"), "colored")
      # })    
      # 
      # output$gee_map <- renderLeaflet({
      #   map_ind()
      # })
      
  
      
    }
  )
}