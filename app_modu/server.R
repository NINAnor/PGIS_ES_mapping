function(input, output, session) {
  
  #track_usage(storage_mode = store_rds(path = "logs/"))
  
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
    # quest_data$t2<-Sys.time()
  })

  ## submit switch to tab 4 and remove others
  # observeEvent(input$sub3, {
  #   updateTabsetPanel(session, "inTabset",
  #                     selected = "p4")
  # })
  # observeEvent(input$sub3, {
  #   hideTab(inputId = "inTabset",
  #           target = "p3")
  # })
  # observeEvent(input$sub3, {
  #   showTab(inputId= "inTabset",
  #           target = "p4")
  # })

  
  ########## 1. Questionnaire and living map
 
  liv_pol <- callModule(selectMod, "map_living",
                        leaflet() %>%
                          addProviderTiles(provider= "CartoDB.Positron")%>%setView(10.42,63.44,10)%>%
                          addFeatures(st_sf(grd), layerId = ~seq_len(length(grd)))


  )
  # 
  dat_quest<-eventReactive(input$sub1, { 
    ## extract centroid
    gs<-liv_pol()
    gs<-st_sf(grd[as.numeric(gs[which(gs$selected==TRUE),"id"])])
    cent<-st_centroid(gs)

    quest<-callModule(ESmoduleServer, "return_quest",st_coordinates(cent)[2],st_coordinates(cent)[1])
    dat_quest<-as.data.frame(({ quest() }))
    write.csv(dat_quest,"C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/Questionnaire/shiny_output/user_dat.csv",
              row.names = T)
    })
  
  ########## 2. first ES selection

  df <- shiny::reactiveValues(types = sapply(dat, class),
                              data = data_copy,
                              zoom_to = zoomto,
                              edit_logic = le)
  
  shiny::observe({
    edits  <- callModule(
      module = editMod,
      leafmap = {
        mapv <- mapview::mapview(df$zoom_to,
                                 map.types = "CartoDB.Positron")@map %>%
          leaflet::hideGroup('df$zoom_to') %>%
          leafem::addFeatures(data = df$data,
                              layerId = df$data$leaf_id,
                              group = 'editLayer',
                              popup = leafpop::popupTable(df$data)) 
        mapv},
      id = "map",
      targetLayerId = 'editLayer',
      sf = TRUE,
      # editorOptions = list(circleMarkerOptions = leaflet.extras::drawCircleMarkerOptions(F),
      #                      rectangleOptions = leaflet.extras::drawRectangleOptions(F),
      #                       editOptions = leaflet.extras::editToolbarOptions(edit = df$edit_logic)),
    )
  })
  
  
  proxy_map <- leaflet::leafletProxy('map-map', session)
  # render new row form based on the existing data structure
  shiny::observe({
    
    output$dyn_form <- shiny::renderUI({
      
      shiny::tagList(
        lapply(1:length(df$types), function(n){
          name <- names(df$types[n])
          label <- paste0(names(df$types[n]), ' (', df$types[n], ')')
          if (df$types[n] == 'character') {
            shiny::textInput(name, label, width = '100%')
          } else if (df$types[n] == 'factor') {
            shiny::selectInput(name, label, width = '100%',
                               choices = levels(dat[[names(df$types[n])]]),
                               selected = NULL,
                               selectize = TRUE)
          } else if (df$types[n] %in% c('numeric','integer')) {
            shiny::numericInput(name, label, width = '100%', value = NA)
          } else if (df$types[n] == 'Date') {
            shiny::dateInput(name, label, width = '100%', value = NA)
          }
        }),
        # we don't want to see this element but it is needed to form data structure
        htmltools::tags$script("document.getElementById('leaf_id-label').hidden
= true; document.getElementById('leaf_id').style.visibility = 'hidden';")
      )
      
    })
  })
  
  
  output$tbl <- DT::renderDataTable({
    
    n <- grep('leaf_id|geom', colnames(df$data)) # used to hide geometry/leaf_id column
    
    DT::datatable(
      df$data,
      options = list(scrollY="200px",
                     pageLength = 50,
                     scrollX = TRUE,
                     columnDefs = list(list(visible=FALSE, targets=n))),
      # could support multi but do single for now
      selection = "single",
      height = 200,
      editable = TRUE,
    )
  })
  
  proxy = DT::dataTableProxy('tbl')
  
  # modify namespace to get map ID
  nsm <- function(event="", id="map") {
    paste0(session$ns(id), "-", event)
  }
  
  EVT_DRAW <- "map_draw_new_feature"
  EVT_EDIT <- "map_draw_edited_features"
  EVT_DELETE <- "map_draw_deleted_features"
  
  #create a vector input for 'row_add'
  EVT_ADD_ROW <- "row_add"
  
  # determines whether to use 'row_add' or 'map_draw_feature'
  # also, if rows are selected then it won't trigger the 'map_draw_feature'
  addRowOrDrawObserve <- function(event, id) {
    shiny::observeEvent(
      if(is.na(id)){
        
        input[[event]]
        
      } else {
        
        input[[nsm(event, id = id)]]},{
          
          if(!is.null(input$tbl_rows_selected)){
            
          } else {
            
            # creates first column and row (must be more elegant way)
            new_row <- data.frame(X = input[[names(df$types[1])]])
            colnames(new_row) <- names(df$types[1])
            
            # remaining columns will be correct size
            for (i in 2:length(df$types)) {
              new_row[names(df$types[i])] <- input[[names(df$types[i])]]
            }
            
            new_row <- sf::st_as_sf(new_row, geometry = 
                                      sf::st_sfc(sf::st_point()), crs = APP_CRS)
            
            suppressWarnings({
              # add to data_copy data.frame and update visible table
              df$data <- df$data %>%
                rbind(new_row)
            })
            
            # reset input table
            
          }
        })
  }
  
  addRowOrDrawObserve(EVT_ADD_ROW, id = NA)
  addRowOrDrawObserve(EVT_DRAW, id = 'map')
  addDrawObserve <- function(event) {
    shiny::observeEvent(
      input[[nsm(event)]],
      {
        evt <- input[[nsm(event)]]
        
        # this allows the user to edit geometries or delete and then save without selecting row.
        # you can also select row and edit/delete as well but this gives the ability to not do so.
        if(event == EVT_DELETE) {
          
          ids <- vector()
          
          for(i in 1:length(evt$features)){
            iter <- evt$features[[i]]$properties[['layerId']]
            ids <- append(ids, iter)
          }
          
          df$data <- dplyr::filter(df$data, !df$data$leaf_id %in% ids)
          df$ids <- ids
          
        } else if (event == EVT_EDIT) {
          
          for(i in 1:length(evt$features)){
            
            evt_type <- evt$features[[i]]$geometry$type
            leaf_id <- evt$features[[i]]$properties[['layerId']]
            geom <- unlist(evt$features[[i]]$geometry$coordinates)
            
            if (evt_type == 'Point') {
              sf::st_geometry(df$data[df$data$leaf_id %in% leaf_id,]) <- sf::st_sfc(sf::st_point(geom))
            } else if (evt_type == 'Polygon'){
              geom <- matrix(geom, ncol = 2, byrow = T)
              sf::st_geometry(df$data[df$data$leaf_id %in% leaf_id,]) <- sf::st_sfc(sf::st_polygon(list(geom)))
            } else if (evt_type == 'LineString'){
              geom <- matrix(geom, ncol = 2, byrow = T)
              sf::st_geometry(df$data[df$data$leaf_id %in% leaf_id,]) <- sf::st_sfc(sf::st_linestring(geom))
            }
          }
          
        } else {
          
          # below determines whether to use 'row_add' or 'map_draw_feature' for adding geometries
          if(!is.null(input$tbl_rows_selected)) {
            selected <- shiny::isolate(input$tbl_rows_selected)
          }  else if (event == EVT_DRAW){
            selected <- length(input$tbl_rows_all) + 1
          }
          
          skip = F
          
          # ignore if selected is null
          if(is.null(selected)) {skip = TRUE}
          
          # replace if draw or edit
          if(skip==FALSE) {
            sf::st_geometry(df$data[selected,]) <- sf::st_geometry(
              mapedit:::st_as_sfc.geo_list(evt))
            
            #adding the leaf_id when we draw or row_add
            df$data[selected, 'leaf_id'] <- 
              as.integer(evt$properties[['_leaflet_id']])
            
          }
        }
      })
  }
  
  addDrawObserve(EVT_DRAW)
  addDrawObserve(EVT_EDIT)
  addDrawObserve(EVT_DELETE)

  # update table cells with double click on cell
  shiny::observeEvent(input$tbl_cell_edit, {
    
    df$data <- DT::editData(df$data, input$tbl_cell_edit, 'tbl', 
                            resetPaging = F)
    DT::replaceData(proxy, df$data, rownames = FALSE, resetPaging = FALSE)
    
  })
  

  
  # provide mechanism to return after all done
  gee_poly<-shiny::eventReactive(input$sub2, {

     out <- sf::st_sf(df$data,crs=user_crs)
     
     ### area and amount of polys
     area <-sum(st_area(out))
     ###
     callModule(ESmoduleServer, "es_quest",area,nrow(out))
     # dat_quest<-as.data.frame(({ quest() }))     

     geom<-as.data.frame(out)
     # out$user_ID<-rep(dat_quest$userID,nrow(out))
     # out$ES<-rep(sel_es_ab,nrow(out))
     geom<-st_as_sf(geom)    
     gee_poly<-rgee::sf_as_ee(geom, via = "getInfo")
    
  })
  

#   ## create gee polygon as soon as submit button 3 is pressed
  # gee_poly<-eventReactive(input$sub2, {
    # geom <- out
    # geom<-as.data.frame(geom)
    # geom<-st_as_sf(geom)
  #   # geom$rec_val<-c(1,2,3)
  #   gee_poly<-rgee::sf_as_ee(geom, via = "getInfo")
  # 
  # 
  # })
#   
  

  prediction <- eventReactive(input$sub2, {
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
    
    # assetid <- paste0(ee_get_assethome(), '/rgee/individual_R1_',sel_es_ab,"/","abc")
    # task_img <- ee_image_to_asset(
    #   image = regression,
    #   assetId = assetid,
    #   overwrite = T,
    #   region = geometry
    # )
    # 
    # task_img$start()

  })
  
  # 
  ##saving
  # observeEvent(input$sub2, {
  # 
  #   prediction<-prediction()
  #   # userid <- input$userID
  #   assetid <- paste0(ee_get_assethome(), '/rgee/individual_R1_',sel_es_ab,"/","abc")
  #   task_img <- ee_image_to_asset(
  #     image = prediction,
  #     assetId = assetid,
  #     overwrite = T,
  #     region = geometry
  #   )
  # 
  #   task_img$start()
  # 
  # 
  # })
  # 
  map3 <- eventReactive(input$sub2,{
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
