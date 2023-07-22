# call mapping module

remapUI<- function(id, label = "selector") {
  ns <- NS(id)
  tagList(
    textOutput(ns("mapping_status")),
    br(),
    leafletOutput(ns("base_map")),
    br(),
    DTOutput(ns("blog")),
    br(),
    # uiOutput(ns("map_adjust")),
    selectizeInput(ns("map_poss"),label="Do you want to adjust your areas of good ES?",choices = c("Yes","No"),options = list(
      placeholder = 'Please select an option below',
      onInitialize = I('function() { this.setValue(""); }'))),
    actionButton(ns("confirm"),'confirm'),
    br(),
    conditionalPanel(
      condition = "output.mappable == 1", ns=ns,
      editModUI(ns("map_sel"))
    ),
    conditionalPanel(
      condition = "output.mappable == 2", ns=ns,
      textOutput(ns("test2"))
    )
    
    # uiOutput(ns("edit_map"))
    #textOutput(ns("test")),
    # editModUI(ns("map_sel"))
    
    
    # conditionalPanel(
    #   condition = "input.editmap == 'Yes'", ns=ns,
    
    #   ,
    # 
    #   br(),
    #   
    # ),
    # conditionalPanel(
    #   condition = "input.map_adjust == 'No'", ns=ns,
    # 
    # )
    # editModUI(ns("map_edit")),
    # actionButton(ns("submit"),"save changes")

  )
  
}



remapServer<-function(id, userID_sel, blog_data, es_descr, userES, siteID_sel, geometry, sf_bound, vis_qc, mapping_round){
  moduleServer(
    id,
    function(input, output, session){
      ns<-session$ns
      
      userES_sel<-userES[mapping_round,]
      esID_sel<-userES_sel$esID
      es_descr_sel<-es_descr%>%filter(esID == esID_sel)
      blog_data_sel<-blog_data%>%filter(esID == esID_sel)
      
      ## all raster path
      imgpath1<-paste0(ee_get_assethome(), '/R_1/all_part/',"1_","drinking_wat", "_", siteID_sel)
      img_all<-ee$Image(imgpath1)$select("probability")
      
      
      ## blog
      output$blog<-renderDT(blog_data_sel,rownames= FALSE, colnames="Blog entries")
      

      if(userES_sel$mapping == "Yes"){
        output$mapping_status<-renderText(paste0("In the previous mapping round you have mapped ", es_descr_sel$esNAME))
        imgpath2<-paste0(ee_get_assethome(), '/R_1/ind_maps/',"1_",userID_sel, "_", esID_sel, "_", siteID_sel)
        img_ind<-ee$Image(imgpath2)

        Map$setCenter(10.38649, 63.40271,10)
        m1<-Map$addLayer(geometry)+
          Map$addLayer(
          eeObject = img_ind,
          vis_qc,
          opacity = 0.4,
          name = "Your map"
        )+Map$addLayer(
          eeObject = img_all,
          vis_qc,
          opacity = 0.4,
          name = "all participants")
  
      }else{
        output$mapping_status<-renderText(paste0("In the previous mapping round you have not mapped ", es_descr_sel$esNAME))
        Map$setCenter(10.38649, 63.40271,10)
        m1<-Map$addLayer(geometry)+
            Map$addLayer(
              eeObject = img_all,
              vis_qc,
              opacity = 0.4,
              name = "all participants")
        
      }
      output$base_map <- renderLeaflet({
        m1
        })
      
      output$mappable<-eventReactive(input$confirm,{
        if(input$map_poss == "Yes" & userES_sel$mapping == "Yes"){
          1
        }else{
          2 
        }
      })
      outputOptions(output, 'mappable', suspendWhenHidden=FALSE)
      
      

        poly_path1<-paste0("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/poly_R1/", userID_sel, "_", esID_sel, "_", siteID_sel, ".shp")
            poly_r1<-st_read(poly_path1)
            poly_r1<-st_as_sf(poly_r1)
            poly_r1 <- st_transform(
                poly_r1,
                crs = 4326
            )
            cent_poly <- st_centroid(poly_r1)
            map1<-leaflet(poly_r1)%>%
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
              leaflet.extras::addDrawToolbar(targetGroup='editable',
                                             polylineOptions = F,
                                             polygonOptions = F,
                                             circleOptions = F,
                                             markerOptions = F,
                                             circleMarkerOptions = F,
                                             rectangleOptions = F,
                                             editOptions = editToolbarOptions(),
                                             singleFeature = FALSE)

          edits<-callModule(
              module = editMod,
              leafmap = map1,
              id = "map_sel")
   
        output$test2<-renderText("you can NOT map")
      
      
      
      
      
      
      # output$map_adjust<-renderUI({
      #   lable <- paste0("Based on the information above do you want remap ", es_descr_sel$esNAME,"?")
      #   # selectizeInput(ns("map_poss"),label=lable,choices = c("Yes","No"),options = list(
      #   #   placeholder = 'Please select an option below',
      #   #   onInitialize = I('function() { this.setValue(""); }')
      #   ))
      # })
      # 
      # observeEvent(input$confirm,{
      #    output$edit_map<-renderUI({
      #       if(input$map_poss == "Yes" & userES_sel$mapping == "Yes"){
      # 
      #         editModUI(ns("map_sel"))
      #       }else{
      #         textOutput("you are NOT allowed to map")
      #       }
      #   })
      # })
      # 
      # observeEvent(input$confirm,{
      #   if(input$map_poss == "Yes" & userES_sel$mapping == "Yes"){
      #     poly_path1<-paste0("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/poly_R1/", userID_sel, "_", esID_sel, "_", siteID_sel, ".shp")
      #     poly_r1<-st_read(poly_path1)
      #     poly_r1<-st_as_sf(poly_r1)
      #     poly_r1 <- st_transform(
      #         poly_r1,
      #         crs = 4326
      #     )
      #           cent_poly <- st_centroid(poly_r1)
      #     map1<-leaflet(poly_r1)%>%
      #       addPolygons(color = "orange", weight = 3, smoothFactor = 0.5,
      #                   opacity = 1.0, fillOpacity = 0, group = "editable")%>%
      #       addLabelOnlyMarkers(data = cent_poly,
      #                           lng = ~st_coordinates(cent_poly)[,1], lat = ~st_coordinates(cent_poly)[,2], label = cent_poly$es_valu,
      #                           labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE,
      #                                                       style = list(
      #                                                         "color" = "red",
      #                                                         "font-family" = "serif",
      #                                                         "font-style" = "bold",
      #                                                         "font-size" = "20px"
      #                                                       )))%>%
      #       addProviderTiles(providers$CartoDB.Positron,options = tileOptions(minZoom = 10, maxZoom = 15))%>%
      #       leaflet.extras::addDrawToolbar(targetGroup='editable',
      #                                      polylineOptions = F,
      #                                      polygonOptions = F,
      #                                      circleOptions = F,
      #                                      markerOptions = F,
      #                                      circleMarkerOptions = F,
      #                                      rectangleOptions = F,
      #                                      editOptions = editToolbarOptions(),
      #                                      singleFeature = FALSE)
      #     
      #     edits<-callModule(
      #       module = editMod,
      #       leafmap = map1,
      #       id = "map_sel")
      #   }else{
      #     edits<-NULL
      #   }
      #   edits
      # })
      #      
      # 
      # 
      
      
      # observeEvent(input$confirm, {
      #   if(input$map_poss == "Yes" & userES_sel$mapping == "Yes"){
      #     
      #     output$edit_map<-renderUI(textOutput("you ARE allowed to map"))
      #     
      #   }else{
      #     # mapping_allowed<-"you are NOT allowed"
      #     output$edit_map<-renderUI(textOutput("you are NOT allowed to map"))
      #   }
      #   
      # })
      # 
      # output$test<-renderText(edits())
      
      # edits<-eventReactive(input$map_poss,{
      #     if(input$map_poss == "Yes" & userES_sel$mapping == "Yes"){
      #       poly_path1<-paste0("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/poly_R1/", userID_sel, "_", esID_sel, "_", siteID_sel, ".shp")
      #       poly_r1<-st_read(poly_path1)
      #       poly_r1<-st_as_sf(poly_r1)
      #       poly_r1 <- st_transform(
      #         poly_r1,
      #         crs = 4326
      #       )
      #       cent_poly <- st_centroid(poly_r1)
      #       map1<-leaflet(poly_r1)%>%
      #         addPolygons(color = "orange", weight = 3, smoothFactor = 0.5,
      #                     opacity = 1.0, fillOpacity = 0, group = "editable")%>%
      #         addLabelOnlyMarkers(data = cent_poly,
      #                             lng = ~st_coordinates(cent_poly)[,1], lat = ~st_coordinates(cent_poly)[,2], label = cent_poly$es_valu,
      #                             labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE,
      #                                                         style = list(
      #                                                           "color" = "red",
      #                                                           "font-family" = "serif",
      #                                                           "font-style" = "bold",
      #                                                           "font-size" = "20px"
      #                                                         )))%>%
      #         addProviderTiles(providers$CartoDB.Positron,options = tileOptions(minZoom = 10, maxZoom = 15))%>%
      #         leaflet.extras::addDrawToolbar(targetGroup='editable',
      #                                        polylineOptions = F,
      #                                        polygonOptions = F,
      #                                        circleOptions = F,
      #                                        markerOptions = F,
      #                                        circleMarkerOptions = F,
      #                                        rectangleOptions = F,
      #                                        editOptions = editToolbarOptions(),
      #                                        singleFeature = FALSE)
      # 
      # 
      #     
      #   }else{
      #     print("no selection")
      #   }
      #   edits<-callModule(
      #     module = editMod,
      #     leafmap = map1,
      #     id = "map_sel")
      #   
      # })

        
 
      
      
      # # for editing / mapping
      # if(userES_sel$mapping == "Yes"){
      
      #   
      # }else{
      #   ## her we start from scratch without a poly like R1
      #   map1<-leaflet(sf_bound)%>%
      #     addPolygons(color = "orange", weight = 3, smoothFactor = 0.5,
      #                 opacity = 1.0, fillOpacity = 0)%>%
      #     addProviderTiles(providers$CartoDB.Positron,options = tileOptions(minZoom = 10, maxZoom = 14))%>%
      #     addDrawToolbar(targetGroup='drawPoly',
      #                    polylineOptions = F,
      #                    polygonOptions = F,
      #                    circleOptions = F,
      #                    markerOptions = F,
      #                    circleMarkerOptions = F,
      #                    rectangleOptions = T,
      #                    singleFeature = FALSE,
      #                    editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))+m1
      #   
      # }
      
      # edits<-editMap(map1, targetLayerId = "editable")
      
      

      
      
      
      ### call the edit map module from the mapedit package

      # edits<-mapedit::editMap(map)
      

      
      
      
      # esID_sel<-es_descr_sel$esID
      # 
      # poly_path1<-paste0("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/poly_R1/", userID_sel, "_", esID_sel, "_", siteID_sel, ".shp")
      # poly_r1<-st_read(poly_path1)
      # poly_r1<-st_as_sf(poly_r1)
      # poly_r1 <- st_transform(
      #   poly_r1, 
      #   crs = 4326
      # )
      # cent_poly <- st_centroid(poly_r1)
      


      
      
      
      ##raster
      

     #  Map$setCenter(10.38649, 63.40271,10)
     #  m1<-Map$addLayer(
     #    eeObject = img_ind1,
     #    vis_qc,
     #    opacity = 0.4
     #  )+Map$addLegend(vis_qc, name = "probability of ES", color_mapping = "character")
     #  
     # 
     # map<-leaflet(poly_r1)%>%
     #    addPolygons(color = "orange", weight = 3, smoothFactor = 0.5,
     #                opacity = 1.0, fillOpacity = 0, group = "editable")%>%
     #   addLabelOnlyMarkers(data = cent_poly,
     #                       lng = ~st_coordinates(cent_poly)[,1], lat = ~st_coordinates(cent_poly)[,2], label = cent_poly$es_valu,
     #                       labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE,
     #                                                   style = list(
     #                                                     "color" = "red",
     #                                                     "font-family" = "serif",
     #                                                     "font-style" = "bold",
     #                                                     "font-size" = "20px"
     #                                                   )))%>%
     #    addProviderTiles(providers$CartoDB.Positron,options = tileOptions(minZoom = 10, maxZoom = 15))%>%
     #   leaflet.extras::addDrawToolbar(targetGroup='editable',
     #                   polylineOptions = F,
     #                   polygonOptions = F,
     #                   circleOptions = F,
     #                   markerOptions = F,
     #                   circleMarkerOptions = F,
     #                   rectangleOptions = F,
     #                   singleFeature = FALSE)+m1

      #edits<-editMap(map, targetLayerId = "editable")
      # 
      # edits<-callModule(
      #   module = editMod,
      #   leafmap = map,
      #   id = "map_edit",
      #   targetLayerId = "editable")

      
      # gee_poly<-eventReactive(input$submit,{
      #   # tbl<-tbl_out()
      #   polygon<-edits()$finished
      #   
      #     
      #     polypath <- paste0( 'C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/poly_R2/',es_id,"_",userID,".shp")
      #     ## save poly
      #     st_write(polygon,polypath)
      #     
      #     gee_poly<-rgee::sf_as_ee(polygon, via = "getInfo")
      #     
      #   
      # })
      
      # prediction<-eventReactive(input$submit,{
      #   gee_poly<-gee_poly()
      #   #### earth engine part
      #   bands <- c("landcover","be75","landcover_count","slope","slope_mean","aspect")
      #   
      #   poly_pts = comb$select(bands)$sampleRegions(
      #     collection= gee_poly,
      #     geometries = T
      #   )
      #   
      #   classifier <- ee$Classifier$smileRandomForest(100, NULL, 1,0.5,NULL,0)$setOutputMode("REGRESSION")$train(
      #     features=poly_pts,
      #     classProperty= "es_value",
      #     inputProperties = list("landcover","be75","landcover_count","slope","slope_mean","aspect")
      #   )
      #   
      #   regression <- comb$select("landcover","be75","landcover_count","slope","slope_mean","aspect")$classify(classifier, "predicted")
      #   
      #   #rand_es_sel <- rand_es_sel()
      #   es_ak<-es_ak
      #   userID <- userID
      #   # prediction<-prediction()
      #   assetid <- paste0(ee_get_assethome(), '/R_2/ind_maps_',es_id,"_", userID)
      #   print(assetid)
      #   start_time<-Sys.time()
      #   task_img <- ee_image_to_asset(
      #     image = regression,
      #     assetId = assetid,
      #     overwrite = F,
      #     region = geometry
      #   )
      #   
      #   task_img$start()
      #   prediction<-regression
      #   
      # })
      
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