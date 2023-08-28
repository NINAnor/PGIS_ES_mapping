## mapping light module for remapping module
maplight_UI<-function(id, label = "mappinglight"){
  ns<-NS(id)
  tagList(
    uiOutput(ns("es_quest_where")),
    br(),
    editModUI(ns("map_sel")),
    htmlOutput(ns("overlay_result")),
    uiOutput(ns("btn1"))
    
  )
}#/ui
maplight_server<-function(id,sf_bound, comb, bands, esID_sel, userID_sel, studyID, img_all, geometry, maxentviz, es_descr_sel){
  moduleServer(
    id,
    function(input, output, session){
      ns<-session$ns
      output$es_quest_where<-renderUI(h4(paste0("Where do you find good areas for ", es_descr_sel$esNAME,"?")))
      output$es_quest_how<-renderUI(h6(paste0("How do you rate the quality of ",es_descr_sel$esNAME, " for your adjusted areas?")))
      output$rating_task<-renderUI(h4(paste0("Indicate how good the areas are to benefit from ",es_descr_sel$esNAME, " (1 = ok, 5= very good)")))
      Map$setCenter(10.38649, 63.40271,10)
      R1_map_all<-Map$addLayer(
                    eeObject = img_all,
                    maxentviz,
                    opacity = 0.4,
                    name = "all participants")
      
      #1. prepare the maps, based on the R1 consensus map
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
                       editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))+R1_map_all
      
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
      
      
      #2. call the mapping module
      rv<-reactiveValues(
        edits = reactive({})
      )

      rv$edits<-callModule(
        module = editMod,
        leafmap = map,
        id = "map_sel")
      
      ## check for intersecting polys
      observe({
        req(rv$edits)
        rectangles <- rv$edits()$all
        n_poly<-nrow(as.data.frame(rectangles))
        
        if(n_poly==1){
          n_within<-nrow(as.data.frame(st_within(rectangles,sf_bound)))
          if(n_within<n_poly){
            output$overlay_result <- renderText({
              paste("<font color=\"#FF0000\"><b>","You can`t save the polygons:","</b> <li>Place your polygon completely into the the study area<li/></font>")
            })
            removeUI(
              selector = paste0("#",ns("savepoly")))
          }else{
            output$btn1<-renderUI(
              actionButton(ns("savepoly"),"save")
            )
            output$overlay_result <- renderText({
              "Save or draw further polygons"
            })
          }
          
        }else if (n_poly>1){
          n_within<-nrow(as.data.frame(st_within(rectangles,sf_bound)))
          n_inter<-nrow(as.data.frame(st_intersects(rectangles)))
          q=n_inter-n_poly
          if(q!=0 & n_within<n_poly){
            removeUI(
              selector = paste0("#",ns("savepoly")))
            
            output$overlay_result <- renderText({
              paste("<font color=\"#FF0000\"><b>","You can`t save the polygons:","</b><li>Place your polygon completely into the the study area<li/><li>Remove overlays<li/></font>")
              
            })
          }else if(q==0 & n_within<n_poly){
            removeUI(
              selector = paste0("#",ns("savepoly")))
            
            output$overlay_result <- renderText({
              paste("<font color=\"#FF0000\"><b>","You can`t save the polygons:","</b> <li>Place your polygon completely into the the study area<li/></font>")
              
            })
          }else if(q!=0 & n_within==n_poly){
            removeUI(
              selector = paste0("#",ns("savepoly")))
            
            output$overlay_result <- renderText({
              paste("<font color=\"#FF0000\"><b>","You can`t save the polygons:","</b> <li>Remove overlays<li/></font>")
              
            })
          }else if(q==0 & n_within==n_poly){
            output$btn1<-renderUI(
              actionButton(ns("savepoly"),"save")
            )
            output$overlay_result <- renderText({
              "Save or draw further polygons"
            })
          }
        }
        
      })
      #3. call the sliders and blog
      
      ### confirm the drawings and render the results table
      tbl_out<-eventReactive(input$savepoly,{
        tbl<-rv$edits()$finished
        req(tbl, cancelOutput = FALSE)
        tbl<-tbl%>%st_drop_geometry()
        tbl$value_es<-rep(NA,(nrow(tbl)))
        tbl
      })
      
      observeEvent(input$savepoly,{
        tbl<-tbl_out()
        polygon<-rv$edits()$finished
        # do not give possibility to submit map without polygons
        req(polygon, cancelOutput = FALSE)
        
        insertUI(
          selector = paste0("#",ns("savepoly")),
          where = "afterEnd",
          ui = tagList(
            uiOutput(ns("es_quest_how")),
            br(),
            leafletOutput(ns("map_res")),
            br(),
            uiOutput(ns("rating_task")),
            uiOutput(ns("slider")),
            br(),
            # a short expl. why this sites
            uiOutput(ns("blogdescr")),
            textInput(ns("blog"), label = ""),
            br(),
            conditionalPanel(
              condition = "input.blog != ''", ns=ns,
              actionButton(ns("submit"),"save values (wait app. 30 sec)")
            )
          )
        )
        
        removeUI(
          selector = paste0("#",ns("savepoly")))
        removeUI(
          selector = paste0("#",ns("map_sel"),"-map"))
        removeUI(
          selector = paste0("#",ns("es_quest_where")))
        
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
        # create sliders according to number of polys
        output$slider <- shiny::renderUI({
          ns <- session$ns
          tagList(
            paste0("The Nr. of the slider refer to the number of the rectangle in the map"),
            br(),
            lapply(1:nrow(tbl),function(n){
              polynr <- tbl[n,]$`_leaflet_id`
              id<-paste0("id_",polynr)
              lable<-paste0("Polygon Nr in map: ",polynr)
              sliderInput(ns(id),lable, min = 1, max = 5, step = 1, value = 3)
            })
          )
          
        })
      })#/observer

      #4. maxent
      ## remove map UI and sliders show result
      observeEvent(input$submit, {
        
        insertUI(
          selector = paste0("#",ns("submit")),
          where = "afterEnd",
          ui = tagList(
            textOutput(ns("res_text")),
            leafletOutput(ns("gee_map"))
          )
        )
        
        removeUI(
          selector = paste0("#",ns("map_res"))
        )
        removeUI(
          selector = paste0("#",ns("slider"))
        )
        removeUI(
          selector = paste0("#",ns("blogdescr"))
        )
        removeUI(
          selector = paste0("#",ns("blog"))
        )
        removeUI(
          selector = paste0("#",ns("submit"))
        )
        removeUI(
          selector = paste0("#",ns("es_quest_how"))
        )
        removeUI(
          selector = paste0("#",ns("rating_task"))
        )
        
      })
      
      prediction<-eventReactive(input$submit, {
        withProgress(message = "save your drawings",value = 0.1,{
          polygon<-rv$edits()$finished
          req(polygon, cancelOutput = FALSE)
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
          polygon$esID <- rep(esID_sel,nrow(polygon))
          polygon$userID <- rep(userID_sel,nrow(polygon))
          polygon$siteID <- rep(studyID,nrow(polygon))
          polygon$mapping_order <- rep(1,nrow(polygon))
          polygon$delphi_round<-rep(2, nrow(polygon))
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
          polypath <- paste0( 'C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/poly_R2/',userID_sel,"_",esID_sel,"_",studyID,".shp")
          ## save poly
          st_write(polygon,polypath)
          
          poly_area<-as.numeric(sum(st_area(polygon)))
          
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
          
          # inside pts are area + es value weighted
          for (i in 1:nrow(polygon)) {
            A_tmp <- as.numeric(st_area(polygon[i,]))
            #tmp_ratio<-A_tmp/A_min
            tmp_ratio<-A_tmp/A_roi
            # npts in this poly must be max_pts*tmp_ratio*es_value
            tmp_pts = st_sample(polygon[i,], round(max_pts*tmp_ratio,0)*polygon[i,]$es_value,type="random")
            tmp_pts<-st_as_sf(tmp_pts)
            tmp_pts$inside<-rep(1,nrow(tmp_pts))
            # pts_ee<-rbind(pts_all,tmp_pts)
            if(i==1){
              pts_in<-tmp_pts
            }else{
              pts_in<-rbind(pts_in,tmp_pts)
            }
            
          }
          # ee object of sampling pts 6k pts = 7sec
          pts_ee<-rbind(pts_out,pts_in)
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
 
          
          
          #############
          train_param <-
            list(
              esID = esID_sel,
              userID = userID_sel,
              siteID = studyID,
              mappingR2_UID = paste0(userID_sel,"_",esID_sel,"_", studyID),
              # imp_acc= as.integer(input$access),
              area = as.integer(poly_area),
              n_poly = as.integer(n_polys),
              blog = input$blog,
              map_adjust = "no_first_mapping",
              mapping_order = as.integer(1),
              extrap_RMSE = 0,
              extrap_accIMP = 0,
              extrap_lulcIMP = 0,
              extrap_natIMP = 0,
              edited = "yes"
              
            )
          train_param<-as.data.frame(train_param)
          
          ############ maxent
          incProgress(amount = 0.1,message = "update data base")
          # write to bq
          # insert_upload_job("rgee-381312", "data_base", "es_mappingR2", train_param)
          
          prediction<-imageClassified$select("probability")
          
          ############ save map
          incProgress(amount = 0.2,message = "store your map")
          img_assetid <- paste0(ee_get_assethome(), '/R_2/ind_maps/',userID_sel,"_",esID_sel,"_", studyID)
          
          #set features of img
          # prediction <- prediction$set('es_id', esID,
          #                              'userID', userID,
          #                              'siteID', siteID,
          #                              'order_id', order,
          #                              'delphi_round', 2)
          # 
          # start_time<-Sys.time()
          # task_img <- ee_image_to_asset(
          #   image = prediction,
          #   assetId = img_assetid,
          #   overwrite = T,
          #   region = geometry
          # )
          # 
          # task_img$start()
          
          
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
      
      #5. visualization
      output$gee_map <- renderLeaflet({
        prediction()
      })
      
      #6. next btn as output
      insertUI(
        selector = paste0("#",ns("submit")),
        where = "afterEnd",
        ui = tagList(
          actionButton(ns("confirm"), "Next task", class='btn-primary')
        )
      )
      
      cond <- reactive({input$confirm})
      
      return(cond) 
    }#function
  )#/module server
}#/server
