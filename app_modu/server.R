function(input, output, session) {
  
  #track_usage(storage_mode = store_rds(path = "logs/"))
  
  ## hiding all tabs but not start
  hideTab(inputId = "inTabset", target = "p1")
  hideTab(inputId = "inTabset", target = "p2")
  hideTab(inputId = "inTabset", target = "p3")
  hideTab(inputId = "inTabset", target = "p4")
  hideTab(inputId = "inTabset", target = "p5")
  
  ## submit mail, switch and remove tab one
  observeEvent(input$sub0, {
    updateTabsetPanel(session, "inTabset",
                      selected = "p1")
  })
  observeEvent(input$sub0, {
    hideTab(inputId = "inTabset", target = "p0")
  })
  observeEvent(input$sub0, {
    showTab(inputId = "inTabset", target = "p1")
  })
  userID<-eventReactive(input$sub0, {
    # create random large string
    UID_part<-stri_rand_strings(1, 10, pattern = "[A-Za-z0-9]")
    if(is.null(input$email)){
      email<-"not_provided"
    } else {
      email = input$email}
    
    user_conf<-data.frame(email = email,UID = UID_part)
    ### save user conf
    user_all<-readRDS("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/user_conf.rds")
    ###here it might be wise to double check if the UID is unique
    ####
    
    user_all<-rbind(user_all,user_conf)
    saveRDS(user_all,"C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/user_conf.rds")
    rm(user_conf,user_all)
    userID<-as.character(UID_part)
    return(userID)
  })

  ## submit questionnaire and switch to expl
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
  

  ## confirm expl switch to tab mapping I
  observeEvent(input$sub3, {
    updateTabsetPanel(session, "inTabset",
                      selected = "p3")
  })
  observeEvent(input$sub3, {
    hideTab(inputId = "inTabset",
            target = "p2")
  })
  observeEvent(input$sub3, {
    showTab(inputId= "inTabset",
            target = "p3")
    # quest_data$t2<-Sys.time()
  })
  
  ## confirm expl switch to tab mapping II
  observeEvent(input$sub2, {
    updateTabsetPanel(session, "inTabset",
                      selected = "p4")
  })
  observeEvent(input$sub2, {
    hideTab(inputId = "inTabset",
            target = "p3")
  })
  observeEvent(input$sub2, {
    showTab(inputId= "inTabset",
            target = "p4")
    # quest_data$t2<-Sys.time()
  })

  ## confirm mapping switch to view map
  observeEvent(input$sub4, {
    updateTabsetPanel(session, "inTabset",
                      selected = "p5")
  })
  observeEvent(input$sub4, {
    hideTab(inputId = "inTabset",
            target = "p4")
  })
  observeEvent(input$sub4, {
    showTab(inputId= "inTabset",
            target = "p5")
  })

  
  ########## 1. Questionnaire and living map
 
  liv_pol <- callModule(selectMod, "map_living",
                        leaflet() %>%
                          addProviderTiles(provider= "CartoDB.Positron")%>%setView(10.42,63.44,10)%>%
                          addFeatures(st_sf(plz), layerId = ~seq_len(length(plz)))


  )
  # 
  observeEvent(input$sub1, { 
    ## extract centroid
    userID<-userID()
    gs<-liv_pol()
    gs<-st_sf(plz[as.numeric(gs[which(gs$selected==TRUE),"id"])])
    cent<-st_centroid(gs)
    user_lat <- st_coordinates(cent)[2]
    user_lng <- st_coordinates(cent)[1]

    callModule(return_quest_Server, "return_quest", userID, user_lat, user_lng)
    })
  
  
  ####### 2. select a random ES as soon as we press sub1 button on second page show it on page 4 as plain text
  rand_es_sel<-eventReactive(input$sub3,{
    rand_ind<-sample(es_descr$es_ind,1)
    rand_es_sel<-es_descr%>%filter(es_ind %in% rand_ind)

    return(rand_es_sel)
  })
  
  observeEvent(input$sub3, {
    userID<-userID()
    rand_es_sel<-rand_es_sel()
    callModule(mapping_server,"es_train", rand_es_sel, userID,sf_bound, comb,geometry)
    # output$es_title<-renderText(rand_es_sel$es_name_long)
    # output$es_descr<-renderText(rand_es_sel$description)
  })
  # observeEvent(input$sub3, {
  # callModule(training_Server,"es_train",df)
  # })
#   ########## 3. first ES selection
# 
#     df <- shiny::reactiveValues(types = sapply(dat, class),
#                                 data = data_copy,
#                                 zoom_to = zoomto,
#                                 edit_logic = le)
#     
#     shiny::observe({
#       edits  <- callModule(
#         module = editMod,
#         leafmap = {
#           mapv <- mapview::mapview(df$zoom_to,
#                                    map.types = "CartoDB.Positron")@map %>%
#             leaflet::hideGroup('df$zoom_to') %>%
#             leafem::addFeatures(data = df$data,
#                                 layerId = df$data$leaf_id,
#                                 group = 'editLayer',
#                                 popup = leafpop::popupTable(df$data))
#           mapv},
#         id = "map",
#         targetLayerId = 'editLayer',
#         sf = TRUE,
#         
#       )
#     })
#     
#     
#     proxy_map <- leaflet::leafletProxy('map-map', session)
#     # render new row form based on the existing data structure
#     
#     
#     shiny::observe({
#       
#       output$dyn_form <- shiny::renderUI({
#         
#         shiny::tagList(
#           lapply(1:length(df$types), function(n){
#             name <- names(df$types[n])
#             label <- paste0(names(df$types[n]), ' (', df$types[n], ')')
#             if (df$types[n] == 'character') {
#               shiny::textInput(name, label, width = '100%')
#             }  else if (df$types[n] %in% c('numeric','integer')) {
#               shiny::sliderInput(name, label,1,5,3,1, width = '100%')
#             }
#           }),
#           # we don't want to see this element but it is needed to form data structure
#           htmltools::tags$script("document.getElementById('leaf_id-label').hidden
# = true; document.getElementById('leaf_id').style.visibility = 'hidden';")
#         )
#         
#       })
#     })
#     
#     
#     output$tbl <- DT::renderDataTable({
#       
#       n <- grep('leaf_id|geom', colnames(df$data)) # used to hide geometry/leaf_id column
#       
#       DT::datatable(
#         df$data,
#         options = list(scrollY="200px",
#                        pageLength = 50,
#                        scrollX = TRUE,
#                        columnDefs = list(list(visible=FALSE, targets=n))),
#         # could support multi but do single for now
#         selection = "single",
#         height = 200,
#         editable = TRUE,
#       )
#     })
#     
#     proxy = DT::dataTableProxy('tbl')
#     
#     # modify namespace to get map ID
#     nsm <- function(event="", id="map") {
#       paste0(session$ns(id), "-", event)
#     }
#     
#     EVT_DRAW <- "map_draw_new_feature"
#     EVT_EDIT <- "map_draw_edited_features"
#     EVT_DELETE <- "map_draw_deleted_features"
#     
#     #create a vector input for 'row_add'
#     EVT_ADD_ROW <- "row_add"
#     
#     # determines whether to use 'row_add' or 'map_draw_feature'
#     # also, if rows are selected then it won't trigger the 'map_draw_feature'
#     addRowOrDrawObserve <- function(event, id) {
#       shiny::observeEvent(
#         if(is.na(id)){
#           
#           input[[event]]
#           
#         } else {
#           
#           input[[nsm(event, id = id)]]},{
#             
#             if(!is.null(input$tbl_rows_selected)){
#               
#             } else {
#               
#               # creates first column and row (must be more elegant way)
#               new_row <- data.frame(X = input[[names(df$types[1])]])
#               colnames(new_row) <- names(df$types[1])
#               
#               # remaining columns will be correct size
#               for (i in 2:length(df$types)) {
#                 new_row[names(df$types[i])] <- input[[names(df$types[i])]]
#               }
#               
#               new_row <- sf::st_as_sf(new_row, geometry =
#                                         sf::st_sfc(sf::st_point()), crs = APP_CRS)
#               
#               suppressWarnings({
#                 # add to data_copy data.frame and update visible table
#                 df$data <- df$data %>%
#                   rbind(new_row)
#               })
#               
#               # reset input table
#               
#             }
#           })
#     }
#     
#     addRowOrDrawObserve(EVT_ADD_ROW, id = NA)
#     addRowOrDrawObserve(EVT_DRAW, id = 'map')
#     
#     addDrawObserve <- function(event) {
#       shiny::observeEvent(
#         input[[nsm(event)]],
#         {
#           
#           evt <- input[[nsm(event)]]
#           
#           # this allows the user to edit geometries or delete and then save without selecting row.
#           # you can also select row and edit/delete as well but this gives the ability to not do so.
#           if(event == EVT_DELETE) {
#             
#             ids <- vector()
#             
#             for(i in 1:length(evt$features)){
#               iter <- evt$features[[i]]$properties[['layerId']]
#               ids <- append(ids, iter)
#             }
#             
#             df$data <- dplyr::filter(df$data, !df$data$leaf_id %in% ids)
#             df$ids <- ids
#             
#           } else if (event == EVT_EDIT) {
#             
#             for(i in 1:length(evt$features)){
#               
#               evt_type <- evt$features[[i]]$geometry$type
#               leaf_id <- evt$features[[i]]$properties[['layerId']]
#               geom <- unlist(evt$features[[i]]$geometry$coordinates)
#               
#               if (evt_type == 'Point') {
#                 sf::st_geometry(df$data[df$data$leaf_id %in% leaf_id,]) <- sf::st_sfc(sf::st_point(geom))
#               } else if (evt_type == 'Polygon'){
#                 geom <- matrix(geom, ncol = 2, byrow = T)
#                 sf::st_geometry(df$data[df$data$leaf_id %in% leaf_id,]) <- sf::st_sfc(sf::st_polygon(list(geom)))
#               } else if (evt_type == 'LineString'){
#                 geom <- matrix(geom, ncol = 2, byrow = T)
#                 sf::st_geometry(df$data[df$data$leaf_id %in% leaf_id,]) <- sf::st_sfc(sf::st_linestring(geom))
#               }
#             }
#             
#           } else {
#             
#             # below determines whether to use 'row_add' or 'map_draw_feature' for adding geometries
#             # if(!is.null(input$tbl_rows_selected)) {
#             #   selected <- shiny::isolate(input$tbl_rows_selected)
#             # }  else if (event == EVT_DRAW){
#             selected <- length(input$tbl_rows_all) + 1
#             # }
#             
#             skip = F
#             
#             # ignore if selected is null
#             if(is.null(selected)) {skip = TRUE}
#             
#             # replace if draw or edit
#             if(skip==FALSE) {
#               sf::st_geometry(df$data[selected,]) <- sf::st_geometry(
#                 mapedit:::st_as_sfc.geo_list(evt))
#               
#               #adding the leaf_id when we draw or row_add
#               df$data[selected, 'leaf_id'] <-
#                 as.integer(evt$properties[['_leaflet_id']])
#               
#             }
#           }
#         })
#       
#     }
#     
#     addDrawObserve(EVT_DRAW)
#     addDrawObserve(EVT_EDIT)
#     addDrawObserve(EVT_DELETE)
#     
#     
#     # update table cells with double click on cell
#     shiny::observeEvent(input$tbl_cell_edit, {
#       
#       df$data <- DT::editData(df$data, input$tbl_cell_edit, 'tbl',
#                               resetPaging = F)
#       DT::replaceData(proxy, df$data, rownames = FALSE, resetPaging = FALSE)
#       
#     })
#     
#     
#     # provide mechanism to return after all done
#     gee_poly<-shiny::eventReactive(input$sub2, {
#       user_es <- rand_es_sel()
#       userID <- userID()
#       out <- sf::st_sf(df$data,crs=user_crs)
#       
#       ### area and amount of polys
#       area <-sum(st_area(out))
#       blog <-input$argue
#       ###
#       callModule(ESmoduleServer, "es_quest",area,nrow(out),blog, userID, user_es$es_id)
#       # dat_quest<-as.data.frame(({ quest() }))
#       
#       ##remove first col since this is the training dat
#       geom<-as.data.frame(out)[-1,]
#       geom$userID<-rep(input$userID,nrow(geom))
#       # out$user_ID<-rep(dat_quest$userID,nrow(out))
#       # out$ES<-rep(sel_es_ab,nrow(out))
#       geom<-st_as_sf(geom)
#       # pol_path <- "C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/output/train_polys_R1"
#       # st_write(geom, paste0(pol_path, "/",userID,"_",user_es$es_id, ".shp"), delete_layer = TRUE)
#       gee_poly<-rgee::sf_as_ee(geom, via = "getInfo")
#       
#     })
#     
#     
#     prediction <- eventReactive(input$sub2, {
#       gee_poly<-gee_poly()
#       bands <- c("landcover","be75","landcover_count","slope","slope_mean","aspect")
#       
#       poly_pts = comb$select(bands)$sampleRegions(
#         collection= gee_poly,
#         geometries = T
#       )
#       
#       classifier <- ee$Classifier$smileRandomForest(100, NULL, 1,0.5,NULL,0)$setOutputMode("REGRESSION")$train(
#         features=poly_pts,
#         classProperty= "ES_value",
#         inputProperties = list("landcover","be75","landcover_count","slope","slope_mean","aspect")
#       )
#       
#       regression <- comb$select("landcover","be75","landcover_count","slope","slope_mean","aspect")$classify(classifier, "predicted")
#       
#       
#       
#     })
#     
#     # ##saving
#     observeEvent(input$sub2, {
#       
#       rand_es_sel <- rand_es_sel()
#       es_ak<-rand_es_sel$es_id
#       userID <- userID()
#       prediction<-prediction()
#       assetid <- paste0(ee_get_assethome(), '/rgee/individual_R1_',es_ak,"/", userID)
#       task_img <- ee_image_to_asset(
#         image = prediction,
#         assetId = assetid,
#         overwrite = F,
#         region = geometry
#       )
#       
#       task_img$start()
#       
#     })
#     

    rand_es_sel2<-eventReactive(input$sub2,{
      rand_es_sel <- rand_es_sel()
      rand_es_sel2<-es_descr
      #remove the old value
      rand_es_sel2<-rand_es_sel2%>%filter(es_id!=rand_es_sel$es_id)
      #sampel another out of the rest
      
      rand_ind<-sample(rand_es_sel2$es_ind,1)
      rand_es_sel2<-rand_es_sel2%>%filter(es_ind %in% rand_ind)
      
      return(rand_es_sel2)
    })
    
    observeEvent(input$sub2, {
      rand_es_sel2<-rand_es_sel2()
      output$es_title2<-renderText(rand_es_sel2$es_name_long)
      output$es_descr2<-renderText(rand_es_sel2$description)
    })
    

  
  
  ########################4. 2nd ES
  ## new es not old one
  

  

  
  ####### 5. show maps with own prediction, mean and diff map
  map_ind <- eventReactive(input$sub4,{
    prediction<-prediction()
    prediction<-ee$Image(prediction)
    gee_poly <- gee_poly()
    user_es <- rand_es_sel()
    collection <- ee$ImageCollection(paste0('users/SPRETO/rgee/individual_R1_',user_es$es_id))
    # col <- ee$ImageCollection(paste0('users/SPRETO/rgee/individual_R1_water_stor'))
    # 
    # a<-ee$Image("users/SPRETO/rgee/individual_R1_water_stor/Jyz5pk2E")
    mean <- collection$reduce(ee$Reducer$mean())
    diff <- prediction$subtract(mean)
    
    
    ## vis params in global
    
    Map$setCenter(10.38649, 63.40271,10)
    m1<-Map$addLayer(
      eeObject = prediction,
      vis_qc,
      opacity = 0.4
    ) +Map$addLegend(vis_qc,name = "prediction", color_mapping = "character") +
      Map$addLayer(gee_poly, list(color = "blue"), "colored")
    m2<-Map$addLayer(
      eeObject = mean,
      vis_qc,
      opacity = 0.4
    ) +Map$addLayer(
      eeObject = diff,
      vis_diff,
      opacity = 0.4
    ) +Map$addLegend(vis_diff,name = "difference", color_mapping = "character")+
      Map$addLayer(gee_poly, list(color = "blue"), "colored")
    m1  | m2
  },
  ignoreNULL = FALSE
  )

  output$gee_map <- renderLeaflet({
    map_ind()
  })
  
  output$n_img <-renderText({
    user_es <- rand_es_sel()
    col <- ee$ImageCollection(paste0('users/SPRETO/rgee/individual_R1_',user_es$es_id))
    n_img<-length(col$getInfo()$features)
    paste0("The crowd map is absed on ", n_img," other participants")
    
  })
}
