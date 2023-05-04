# call mapping module

mapselectUI<- function(id, label = "selector") {
  ns <- NS(id)
  tagList(
    textOutput(ns("title_es")),
    br(),
    textOutput(ns("descr_es")),
    br(),
    h5("Importance of ESx?"),
    sliderInput(ns("imp_own"), paste0("How important is for you personally in this area?"),
                min = 0, max = 5, value = 3
    ),
    sliderInput(ns("imp_other"), paste0("How important is for others and the society in this area?"),
                min = 0, max = 5, value = 3
    ),
    textInput(ns("es_desc"),paste0("Can you describe in a few words what you understand by as an ES?")),
    br(),
    selectizeInput(ns("map_poss"),label="Are you able to map this ES?",choices = c("Yes","No"),options = list(
      placeholder = 'Please select an option below',
      onInitialize = I('function() { this.setValue(""); }')
    )),
    br(),
    conditionalPanel(
      condition = "input.map_poss == 'Yes'", ns = ns ,
      h5("where do you find good spots of ESx?"),
      editModUI(ns("map_sel")),
      br(),
      ###initial button save map
      actionButton(ns("savepoly"),"save polygons"),
      leafletOutput(ns("map_res")),
      uiOutput(ns("slider")),
      br(),
      h5("How important are the following aspects for ESx benefit?"),
      sliderInput(ns("access"), "Accessibility",
                  min = 0, max = 5, value = 3
      ),
      sliderInput(ns("nat"), "Naturalness",
                  min = 0, max = 5, value = 3
      ),
      sliderInput(ns("lulc"), "Landcover",
                  min = 0, max = 5, value = 3
      ),
      textInput(ns("blog"),"Please provide us a short explanation why you choosed and rated your sites as you did"),
      actionButton(ns("submit"),"save values"),
      br(),
      h5("Your ESx map"),
      leafletOutput(ns("gee_map"))%>% withSpinner(color="#0dc5c1")
      
    ),
    conditionalPanel(
      condition = "input.map_poss == 'No'", ns = ns ,
      selectizeInput(ns("expert_map"),label="Would you trust an expert map",choices = c("Yes","No"),options = list(
        placeholder = 'Please select an option below',
        onInitialize = I('function() { this.setValue(""); }'))),
      actionButton(ns("submit2"),"save")
      )
    
  )
  
}

callback <- c(
  '$("#remove").on("click", function(){',
  '  table.rows(".selected").remove().draw();',
  '});'
)

mapselectServer<-function(id, sf_bound, comb, rand_es_sel, userID, geometry, vis_qc){
  moduleServer(
    id,
    function(input, output, session){
      ns<-session$ns
      es_ak<-rand_es_sel$es_id
      output$title_es<-renderText(rand_es_sel$es_name_long)
      output$descr_es<-renderText(rand_es_sel$description)
      
      
     map<-leaflet(sf_bound)%>%
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
      
      map_res<-leaflet(sf_bound)%>%
        addProviderTiles(providers$CartoDB.Positron,options = tileOptions(minZoom = 10, maxZoom = 15))%>%
        addDrawToolbar(targetGroup='drawPoly',
                       polylineOptions = F,
                       polygonOptions = F,
                       circleOptions = F,
                       markerOptions = F,
                       circleMarkerOptions = F,
                       rectangleOptions = F,
                       singleFeature = FALSE,
                       editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))
      
      
      
      edits<-callModule(
        module = editMod,
        leafmap = map,
        id = "map_sel")
      
      
      
      tbl_out<-eventReactive(input$savepoly,{
        tbl<-edits()$finished
        tbl<-tbl%>%st_drop_geometry()
        tbl$value_es<-rep(NA,(nrow(tbl)))
        tbl
      })
      
      observeEvent(input$savepoly, {
        tbl<-tbl_out()
        polygon<-edits()$finished
        cent_poly <- st_centroid(polygon)
        output$map_res<-renderLeaflet(map_res %>% 
                                        addPolygons(data=polygon) %>%
                                        addLabelOnlyMarkers(data = cent_poly,
                                                            lng = ~st_coordinates(cent_poly)[,1], lat = ~st_coordinates(cent_poly)[,2], label = cent_poly$`_leaflet_id`,
                                                            labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE,
                                                                                        style = list(
                                                                                          "color" = "red",
                                                                                          "font-family" = "serif",
                                                                                          "font-style" = "bold",
                                                                                          "font-size" = "20px"
                                                                                        )))
        )
        
        
        
        
      })
      # output$data_tbl<-renderDT(tbl_out(),editable = list(target = "column", disable = list(columns = c(1,2))))
      
      observeEvent(input$savepoly,{
        tbl<-tbl_out()
        output$slider <- shiny::renderUI({
          ns <- session$ns
          tagList(
            h5("please rate ESx for each area"),
            lapply(1:nrow(tbl),function(n){
              polynr <- tbl[n,]$`_leaflet_id`
              id<-paste0("id_",polynr)
              lable<-paste0("Polygon Nr: ",polynr)
              sliderInput(ns(id),lable, min = 1, max = 5, step = 1, value = 3)
            })
          )
          
        })
      })
      
      gee_poly<-eventReactive(input$submit,{
        # tbl<-tbl_out()
        polygon<-edits()$finished
        polygon<-as.data.frame(polygon)
        polygon$es_value<-rep(NA,nrow(polygon))
        sliderval<-list()
        #sl2<-list()
        
        
        ## value vector
        
        #print(reactive({paste0("input$id_",polygon[1,]$`_leaflet_id`)}))
        res<-lapply(1:nrow(polygon),function(a){
          var<-paste0("id_",polygon[a,]$`_leaflet_id`)
          # print(as.numeric(input[[var]]))
          # polygon$es_value[a]<-as.numeric(input[[var]])
          sliderval[[a]]<-input[[var]]
          return(sliderval)
        })
        
        vecA <- unlist(res)
        ## as df
        polygon$es_value <- vecA
        n_polys <-nrow(polygon)
        ## as sf
        polygon<-st_as_sf(polygon)
        area<-sum(st_area(polygon))
        
        train_param <- 
          list(
            userID = userID,
            es_id = es_ak,
            access=input$access,
            nat=input$nat,
            lulc = input$lulc,
            imp_own = input$imp_own,
            imp_other = input$imp_other,
            es_desc = input$es_desc,
            area = area,
            n_polys = n_polys,
            blog = input$blog
            # expert = NA
          )
          train_param<-as.data.frame(train_param)
          ## write to rds file
          es_user<-readRDS("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/es_user_data.rds")
          es_user<-rbind(es_user,train_param)
          # row.names = T)
          saveRDS(es_user,"C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/es_user_data.rds")
        
          
          polypath <- paste0( 'C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/poly_R1/',es_ak,"_",userID,".shp")
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
        assetid <- paste0(ee_get_assethome(), '/rgee/individual_R1_',es_ak,"/", userID)
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
      
      map_ind<-eventReactive(input$submit,{
        prediction<-prediction()
        gee_poly <- gee_poly()
        
        Map$setCenter(10.38649, 63.40271,10)
        m1<-Map$addLayer(
          eeObject = prediction,
          vis_qc,
          opacity = 0.4
        ) +Map$addLegend(vis_qc,name = "prediction", color_mapping = "character") +
          Map$addLayer(gee_poly, list(color = "blue"), "colored")
      })    
      
      output$gee_map <- renderLeaflet({
        map_ind()
      })
      
      observeEvent(input$submit2,{
        train_param <- 
          list(
            userID = userID,
            es_id = es_ak,
            access=NA,
            nat=NA,
            lulc = NA,
            imp_own = input$imp_own,
            imp_other = input$imp_other,
            es_desc = input$es_desc,
            area = NA,
            n_polys = NA,
            blog = NA
            # expert = input$expert_map
          )
        train_param<-as.data.frame(train_param)
        ## write to rds file
        es_user<-readRDS("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/es_user_data.rds")
        es_user<-rbind(es_user,train_param)
        # write.csv(train_param,"C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/output/train_para_R1/train_param_dat.csv",
        # row.names = T)
        saveRDS(es_user,"C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/es_user_data.rds")
        
      })
      
      # a <- reactive({
      #   input$submit
      # })
      # 
      # b <- reactive({
      #   input$submit2
      # })
      # 
      # return(a)
      # return(b)
      # 
      

      
      
    }
  )
}