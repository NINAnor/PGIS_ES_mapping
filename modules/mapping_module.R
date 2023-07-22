# call mapping module

mapselectUI<- function(id, label = "selector") {
  ns <- NS(id)
  tagList(
    textOutput(ns("title_es")),
    br(),
    textOutput(ns("descr_es")),
    br(),
    textOutput(ns("text5")),
    sliderInput(ns("imp_own"), paste0("... for you personally in this area?"),
                min = 0, max = 5, value = 3
    ),
    sliderInput(ns("imp_other"), paste0("... for others and the society in this area?"),
                min = 0, max = 5, value = 3
    ),
    br(),
    # selectizeInput(ns("map_poss"),label="Are you able to map this ES?",choices = c("Yes","No"),options = list(
    #   placeholder = 'Please select an option below',
    #   onInitialize = I('function() { this.setValue(""); }')
    # )),
    uiOutput(ns("map_poss")),
    br(),
    conditionalPanel(
      condition = "input.map_poss == 'Yes'", ns = ns ,
      # h5("where do you find good spots of ESx?"),
      textOutput(ns("text4")),
      editModUI(ns("map_sel")),
      br(),
      ###initial button save map
      actionButton(ns("savepoly"),"save polygons"),
      leafletOutput(ns("map_res")),
      uiOutput(ns("slider")),
      br(),
      textOutput(ns("text1")),
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
      # h5("Your ESx map"),
      textOutput(ns("text3")),
      leafletOutput(ns("gee_map"))%>% withSpinner(color="#0dc5c1"),
      br(),
      textOutput(ns("text2"))

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

mapselectServer<-function(id, sf_bound, comb, rand_es_sel, order, userID, siteID, geometry, vis_qc){
  moduleServer(
    id,
    function(input, output, session){
      ns<-session$ns
      
      es_ak<-rand_es_sel[order,]$esID
      output$title_es<-renderText(rand_es_sel[order,]$esNAME)
      output$descr_es<-renderText(rand_es_sel[order,]$esDESCR)
      output$text1<-renderText(paste0("How important are the following aspects to get a benefit of ",rand_es_sel[order,]$esNAME))
      output$text2<-renderText(paste0("if you look at the map, how important would you rate ",rand_es_sel[order,]$esNAME,"compared to other ecosystem services, given there is no co existance between the two?"))
      output$text3<-renderText(paste0("Your personal map of ",rand_es_sel[order,]$esNAME))
      output$text4<-renderText(paste0("Where do you find good spots for ", rand_es_sel[order,]$esNAME))
      output$text5<-renderText(paste0("How important is ", rand_es_sel[order,]$esNAME,"..."))
      
      output$map_poss<-renderUI({
        lable <- paste0("Are you able to map ", rand_es_sel[order,]$esNAME,"?")
        selectizeInput(ns("map_poss"),label=lable,choices = c("Yes","No"),options = list(
          placeholder = 'Please select an option below',
          onInitialize = I('function() { this.setValue(""); }')
        ))
      })
      

      
     map<-leaflet(sf_bound)%>%
        addPolygons(color = "orange", weight = 3, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0)%>%
        addProviderTiles(providers$CartoDB.Positron,options = tileOptions(minZoom = 10, maxZoom = 14))%>%
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
                                                                                        ))))
        
        
        
        
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
        polygon$esID <- rep(es_ak,nrow(polygon))
        polygon$userID <- rep(userID,nrow(polygon))
        polygon$siteID <- rep(siteID,nrow(polygon))
        polygon$mapping_order <- rep(order,nrow(polygon))
        
        
        for(i in 1: nrow(polygon)){
          polygon$drawing_order[i] <- as.integer(i)
        }
        
        polygon$polyUID<-paste0(userID,"_",esID,"_",siteID,"_",drawing_order)
        
        n_polys <-nrow(polygon)
        ## as sf
        polygon<-st_as_sf(polygon)
        area<-sum(st_area(polygon))
        
        train_param <- 
          list(
            esID = es_ak,
            userID = userID,
            siteID = siteID,
            imp_acc= input$access,
            imp_nat= input$nat,
            imp_lulc = input$lulc,
            imp_own = input$imp_own,
            imp_other = input$imp_other,
            area = area,
            n_poly = n_polys,
            blog = input$blog,
            mapping = "Yes",
            expert_trust = NA,
            es_order = order,
            extrap_RMSE = NA,
            extrap_accIMP = NA,
            extrap_lulcIMP = NA,
            extrap_natIMP = NA,
            mappingR1_UID = paste0(userID,"_",es_ak,"_",siteID)
          )
          train_param<-as.data.frame(train_param)
          ## write to rds file
          # es_user<-readRDS("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/es_user_data.rds")
          # es_user<-rbind(es_user,train_param)
          # # row.names = T)
          # saveRDS(es_user,"C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/es_user_data.rds")
          # 
          
          polypath <- paste0( 'C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/poly_R1/',es_ak,"_",userID,".shp")
          ## save poly
          st_write(polygon,polypath)
          
          gee_poly<-rgee::sf_as_ee(polygon, via = "getInfo")
          
        
      })
      

      
      prediction<-eventReactive(input$submit,{
        gee_poly<-gee_poly()
        #### earth engine part
        bands <- c("landcover","be75","landcover_count","slope","slope_mean","aspect")
        
        poly_pts = comb$select(bands)$sampleRegions(collection= gee_poly,
                                                    properties = list("es_value"),
                                                    scale = 100,
                                                    geometries = T
        )
        
        poly_pts = poly_pts$randomColumn('random')
        
        #split data
        training = poly_pts$filter(ee$Filter$gt('random',0.3)) # 70% training
        validation = poly_pts$filter(ee$Filter$lte('random',0.3)) # 30% testing
        
        
        rfReg <- ee$Classifier$smileRandomForest(100, NULL, 1,0.5,NULL,0)$setOutputMode("REGRESSION")$train(
          features=training,
          classProperty= "es_value",
          inputProperties = list("landcover","be75","landcover_count","slope","slope_mean","aspect")
        )
        
        esPred <- comb$select("landcover","be75","landcover_count","slope","slope_mean","aspect")$classify(rfReg, "predicted")
        esPred <- esPred$set('es_id', es_ak,
                             'userID', userID,
                             'order_id', order)
        
        #rand_es_sel <- rand_es_sel()
        es_ak<-es_ak
        userID <- userID
        
        ## retrieve covariate importance
        varImp = rfReg$explain()$get("importance")$getInfo()%>% #get importance
          as_tibble()%>%#make tibble
          pivot_longer(cols = c(1:ncol(.)))%>% #long df for plotting
          arrange(desc(value))%>% #sort decreasing values
          slice(1:10)
        varImp$rel<-varImp$value/sum(varImp$value)
        varImp$es_ak<-rep(es_ak,nrow(varImp))
        varImp$userID<-rep(userID,nrow(varImp))
        
        b<-readRDS("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/varImp.rds")
        b<-rbind(b,varImp)
        saveRDS(b,"C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/varImp.rds")
        
        # retrieve model quality and
        pred = validation$classify(rfReg, "predictions")$
          select(c("es_value", "predictions"))
        
        
        ### rmse of validation
        #get residuals
        addResid <- function(feature) {
          res <- ee$Number(feature$get("es_value"))$ #subtract observed from predicted
            subtract(ee$Number(feature$get("predictions")))
          feature$set(list(res = res)) #create new feature in featureCollection
        }
        
        #apply function to FeatureCollection for residuals
        res = pred$map(addResid)
        
        #calculate RMSE
        rmse = ee$Array(res$aggregate_array("res"))$
          pow(2)$ #square it
          reduce("mean", list(0))$ #get mean for the residuals
          sqrt()
        
        #print RMSE
        a<-as.data.frame(rmse$getInfo())
        a$userID<-userID
        a$es_ak<-es_ak
        colnames(a)<-c("RMSE","userID","es_ak")
        b<-readRDS("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/RMSE.rds")
        b<-rbind(b,a)
        saveRDS(b,"C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/RMSE.rds")
      
        
        # prediction<-prediction()
        assetid <- paste0(ee_get_assethome(), '/R_1/ind_maps/',"1_",es_ak,"_", userID)
        #print(assetid)
        start_time<-Sys.time()
        task_img <- ee_image_to_asset(
          image = esPred,
          assetId = assetid,
          overwrite = F,
          region = geometry
        )
        
        task_img$start()
        prediction<-esPred
        
      })
      
      map_ind<-eventReactive(input$submit,{
        prediction<-prediction()
        gee_poly <- gee_poly()
        
        Map$setCenter(10.38649, 63.40271,10)
        m1<-Map$addLayer(
          eeObject = prediction,
          vis_qc,
          opacity = 0.4
        ) +Map$addLegend(vis_qc,name = rand_es_sel[order,]$es_name_long, color_mapping = "character") +
          Map$addLayer(gee_poly, list(color = "blue"), "colored")
      })    
      
      output$gee_map <- renderLeaflet({
        map_ind()
      })
      
      observeEvent(input$submit2,{
        list(
          esID = es_ak,
          userID = userID,
          siteID = siteID,
          imp_acc= input$access,
          imp_nat= input$nat,
          imp_lulc = input$lulc,
          imp_own = input$imp_own,
          imp_other = input$imp_other,
          area = NA,
          n_poly = NA,
          blog = NA,
          mapping = "No", 
          expert_trust = input$expert_map,
          es_order = order,
          extrap_RMSE = NA,
          extrap_accIMP = NA,
          extrap_lulcIMP = NA,
          extrap_natIMP = NA,
          mappingR1_UID = paste0(userID,"_",es_ak,"_",siteID)
        )
        train_param<-as.data.frame(train_param)
        insert_upload_job("rgee-381312", "data_base", "es_mappingR1", train_param)
       
      })
      

      

      
      
    }
  )
}

