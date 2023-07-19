
############ UI of maxent mapping module
mapselectUI<- function(id, label = "selector") {
  ns <- NS(id)
  tagList(
    # title 
    textOutput(ns("title_es")),
    br(),
    # description of es 
    textOutput(ns("descr_es")),
    br(),
    # questions of importance
    textOutput(ns("imp_text")),
    sliderInput(ns("imp_own"), paste0("... for you personally in this area?"),
                min = 0, max = 5, value = 3
    ),
    sliderInput(ns("imp_other"), paste0("... for others and the society in this area?"),
                min = 0, max = 5, value = 3
    ),
    br(),
    # are you able to map the ES?
    uiOutput(ns("map_poss")),
    br(),
    conditionalPanel(
      condition = "input.map_poss == 'Yes'", ns = ns ,
      # h5("where do you find good spots of ESx?"),
      textOutput(ns("es_quest")),
      editModUI(ns("map_sel")),
      br(),
      # initial button to save the map
      actionButton(ns("savepoly"),"save polygons"),
      # show mapping result and slider to adjust importance
      leafletOutput(ns("map_res")),
      uiOutput(ns("slider")),
      br(),
      textOutput(ns("varimp_text")),
      sliderInput(ns("access"), "Accessibility",
                  min = 0, max = 5, value = 3
      ),
      # a short expl. why this sites
      textInput(ns("blog"),"Please provide us a short explanation why you choosed and rated your sites as you did"),
      actionButton(ns("submit"),"save values"),
      br(),
      # show results
      textOutput(ns("res_text")),
      leafletOutput(ns("gee_map"))
      ,
      
      
    ),
    # if ES not mappable
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

############# Server part of mapping module
mapselectServer<-function(id, sf_bound, comb, bands, rand_es_sel, order, userID, siteID, geometry, maxentviz){
  moduleServer(
    id,
    function(input, output, session){
      ns<-session$ns
      
      esID<-rand_es_sel[order,]$esID
      output$title_es<-renderText(rand_es_sel[order,]$esNAME)
      output$descr_es<-renderText(rand_es_sel[order,]$esDESCR)
      output$varimp_text<-renderText(paste0("How important are the following aspects to get a benefit of ",rand_es_sel[order,]$esNAME))
      output$res_text<-renderText(paste0("Your personal map of ",rand_es_sel[order,]$esNAME))
      output$es_quest<-renderText(paste0("Where do you find good spots for ", rand_es_sel[order,]$esNAME))
      output$imp_text<-renderText(paste0("How important is ", rand_es_sel[order,]$esNAME,"..."))
      
      # UI rendered to ask if able to map ES
      output$map_poss<-renderUI({
        lable <- paste0("Are you able to map ", rand_es_sel[order,]$esNAME,"?")
        selectizeInput(ns("map_poss"),label=lable,choices = c("Yes","No"),options = list(
          placeholder = 'Please select an option below',
          onInitialize = I('function() { this.setValue(""); }')
        ))
      })
      
      ### prepare the two background maps
      # for editing / mapping
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
      
      # second for results
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
      
      ### call the edit map module from the mapedit package
      edits<-callModule(
        module = editMod,
        leafmap = map,
        id = "map_sel")
      # edits<-mapedit::editMap(map)
      
      
      ### confirm the drawings and render the results table
      tbl_out<-eventReactive(input$savepoly,{
        tbl<-edits()$finished
        tbl<-tbl%>%st_drop_geometry()
        tbl$value_es<-rep(NA,(nrow(tbl)))
        tbl
      })
      
      ### confirm the drawings and render the leaflet map
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
      
      ### create a slider for each of the polygons
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
      
      ### predict probability of ES with maxent, save prediction, save poly and save drawing summaries on gee/bq
      
      ### gather poly
      prediction<-eventReactive(input$submit, {
        withProgress(message = "save your drawings",value = 0.1,{
          polygon<-edits()$finished
          polygon<-as.data.frame(polygon)
          polygon$es_value<-rep(NA,nrow(polygon))
          sliderval<-list()
          
          # extract the values from the slider
          res<-lapply(1:nrow(polygon),function(a){
            var<-paste0("id_",polygon[a,]$`_leaflet_id`)
            # print(as.numeric(input[[var]]))
            # polygon$es_value[a]<-as.numeric(input[[var]])
            sliderval[[a]]<-input[[var]]
            return(sliderval)
          })
          vecA <- unlist(res)
          
          # write attributes to geometry
          polygon$es_value <- vecA
          polygon$esID <- rep(esID,nrow(polygon))
          polygon$userID <- rep(userID,nrow(polygon))
          polygon$siteID <- rep(siteID,nrow(polygon))
          polygon$mapping_order <- rep(order,nrow(polygon))
          polygon$delphi_round<-rep(1, nrow(polygon))
          polygon$drawing_order<-rep(NA,nrow(polygon))
          
          # a drawing order index
          for(i in 1: nrow(polygon)){
            polygon$drawing_order[i] <- as.integer(i)
            # # UID of each single poly
            # polygon$polyUID[i]<-paste0(userID,"_",esID,"_",siteID,"_",polygon$drawing_order[i])
          }
          n_polys <-nrow(polygon)
          polygon<-st_as_sf(polygon)
          
          ## save as shp (up to now)
          polypath <- paste0( 'C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/poly_R1/',userID,"_",esID,"_",siteID,".shp")
          ## save poly
          st_write(polygon,polypath)
          
          poly_area<-as.numeric(sum(st_area(polygon)))
          
          # make ee object and save
          gee_poly<-rgee::sf_as_ee(polygon, via = "getInfo")
          #set features
          # gee_poly <- gee_poly$set('es_id', esID,
          #                          'userID', userID,
          #                          'siteID', siteID,
          #                          'order_id', order,
          #                          'delphi_round', 1)
          
          ## load full collection
          # ee_asset_path<-paste0(ee_get_assethome(), "/train_polys")
          # ee_poly_old<-ee$FeatureCollection(ee_asset_path)
          # 
          # #merge
          # ee_poly_old<-ee_poly_old$merge(gee_poly)
          # 
          # # #save poly old with added gee poly
          # poly_load<-ee_table_to_asset(ee_poly_old,
          #                              description = "upload poly",
          #                              assetId = ee_asset_path,
          #                              overwrite = T
          # )
          # poly_load$start()
          
          
          ############ training pts
          incProgress(amount = 0.2,message = "prepare training data")
          
          
          
          ## N background (outside poly points) according to area of extrapolation
          A_roi<-as.numeric(st_area(sf_bound))
          # area of smallest poly
          A_min<-as.numeric(min(st_area(polygon)))
          # area of largest poly
          A_max<-as.numeric(max(st_area(polygon)))
          
          # max pts for efficient extrapolation each 250x250 cell
          max_pts<- round(A_roi/(300*300),0)
          
          
          # ratio poly area vs whole area
          ratio_A<-poly_area/A_roi
          
          ## although zooming on the map while drawing is limited, we assure that at least 10pts are within a poly
          min_in_pts<-10
          abs_min_res<-100
          min_in_eff_pts<-(sqrt(A_min)/abs_min_res)^2
          
          
          if(min_in_eff_pts<min_in_pts){
            pts_min <- min_in_pts
          } else {
            pts_min <- min_in_eff_pts
          }
          
          # amount of background pts
          pts_out<-round(1/ratio_A*pts_min,0)
          
          # sample backgraound pts
          # pts_out = st_sample(sf_bound, pts_out,type="random")
          pts_out = st_sample(sf_bound, max_pts,type="random")
          
          # don`t allow intersection with polygons
          pts_out <- st_difference(st_combine(pts_out), st_combine(polygon)) %>% st_cast('POINT')
          pts_out<-st_as_sf(pts_out)
          pts_out$inside<-rep(0,nrow(pts_out))
          pts_all<-pts_out
          
          # inside pts are area + es value weighted
          for (i in 1:nrow(polygon)) {
            A_tmp <- as.numeric(st_area(polygon[i,]))
            #tmp_ratio<-A_tmp/A_min
            tmp_ratio<-A_tmp/A_roi
            # npts in this poly must be max_pts*tmp_ratio*es_value
            #tmp_pts = st_sample(polygon[i,], round(tmp_ratio*pts_min,0)*polygon[i,]$es_value,type="random")
            tmp_pts = st_sample(polygon[i,], round(max_pts*tmp_ratio,0)*polygon[i,]$es_value,type="random")
            tmp_pts<-st_as_sf(tmp_pts)
            tmp_pts$inside<-rep(1,nrow(tmp_pts))
            pts_ee<-rbind(pts_all,tmp_pts)
            
          }
          # ee object of sampling pts 6k pts = 7sec
          pts_ee<-rgee::sf_as_ee(pts_ee, via = "getInfo")
          
          # define target bands of comb (indep. var) and sample vars by pts
          pts_ee = comb$select(bands)$sampleRegions(collection= pts_ee,
                                                    properties = list("inside"),
                                                    geometries = T
          )
          
          ############ maxent
          incProgress(amount = 0.2,message = "calculate map")
          
          mEntclass = ee$Classifier$amnhMaxent()$train(
            features = pts_ee,
            classProperty = 'inside',
            inputProperties = bands
          )
          
          imageClassified = comb$select(bands)$classify(mEntclass)
          
          #### varImp and ROC takes a lot of time -- postprocessing?
          # varImp<- mEntclass$explain()$get("Contributions")$getInfo()%>% #get importance
          #   as_tibble()%>%#make tibble
          #   pivot_longer(cols = c(1:ncol(.)))%>% #long df for plotting
          #   arrange(desc(value))%>% #sort decreasing values
          #   slice(1:10)
          # 
          # 
          # AUC_maxent<-mEntclass$explain()$get("Training AUC")$getInfo()%>% #get importance
          #   as_tibble()
          
          
          #############
          train_param <-
            list(
              esID = esID,
              userID = userID,
              siteID = siteID,
              mappingR1_UID = paste0(userID,"_",esID,"_", siteID),
              imp_acc= as.integer(input$access),
              imp_nat= as.integer(0),
              imp_lulc = as.integer(0),
              imp_own = as.integer(input$imp_own),
              imp_other = as.integer(input$imp_other),
              area = as.integer(poly_area),
              n_poly = as.integer(n_polys),
              blog = input$blog,
              mapping = "Yes",
              expert_trust = "no_expert",
              mapping_order = as.integer(order),
              extrap_RMSE = 0,
              extrap_accIMP = 0,
              extrap_lulcIMP = 0,
              extrap_natIMP = 0
              
            )
          train_param<-as.data.frame(train_param)
          
          ############ maxent
          incProgress(amount = 0.1,message = "update data base")
          # write to bq
          insert_upload_job("rgee-381312", "data_base", "es_mappingR1", train_param)
          
          prediction<-imageClassified$select("probability")
          
          ############ save map
          incProgress(amount = 0.2,message = "store your map")
          img_assetid <- paste0(ee_get_assethome(), '/R_1/ind_maps/',"1_",userID,"_",esID,"_", siteID)
          
          #set features of img
          prediction <- prediction$set('es_id', esID,
                                       'userID', userID,
                                       'siteID', siteID,
                                       'order_id', order,
                                       'delphi_round', 1)
          
          start_time<-Sys.time()
          task_img <- ee_image_to_asset(
            image = prediction,
            assetId = img_assetid,
            overwrite = T,
            region = geometry
          )
          
          task_img$start()
          
          
          ############ prepare map
          incProgress(amount = 0.1,message = "prepare interactive map")
          Map$setCenter(10.38649, 63.40271,10)
          
          prediction<-Map$addLayer(
            eeObject = prediction,
            maxentviz,
            "Probability of ES",
            opacity = 0.4)
        })  
        prediction<-prediction
        
      })
      
      
      output$gee_map <- renderLeaflet({
        prediction()
      })
      
      ### store infos if mapping is not possible
      observeEvent(input$submit2,{
        train_param<-list(
          esID = esID,
          userID = userID,
          siteID = siteID,
          mappingR1_UID = paste0(userID,"_",esID,"_",siteID),
          imp_acc= as.integer(0),
          imp_nat= as.integer(0),
          imp_lulc = as.integer(0),
          imp_own = as.integer(input$imp_own),
          imp_other = as.integer(input$imp_other),
          area = as.integer(0),
          n_poly = as.integer(0),
          blog = "NA",
          mapping = "No",
          expert_trust = input$expert_map,
          mapping_order = as.integer(order),
          extrap_RMSE = 0,
          extrap_accIMP = 0,
          extrap_lulcIMP = 0,
          extrap_natIMP = 0
          
        )
        train_param<-as.data.frame(train_param)
        insert_upload_job("rgee-381312", "data_base", "es_mappingR1", train_param)
        
      })
      
    }
  )
}