


trainingUI = function(id) {
  ns <- NS(id)
  
  tagList(
    mainPanel(
      editModUI(ns("map")),
      dataTableOutput(ns("tbl")),
      shiny::uiOutput(ns('dyn_form'))
    )
  )
  
}


training_Server = function(input, output, session, dat,data_copy) {
  # ns <- session$ns

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
          }  else if (df$types[n] %in% c('numeric','integer')) {
            shiny::sliderInput(name, label,1,5,3,1, width = '100%')
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
          # if(!is.null(input$tbl_rows_selected)) {
          #   selected <- shiny::isolate(input$tbl_rows_selected)
          # }  else if (event == EVT_DRAW){
          selected <- length(input$tbl_rows_all) + 1
          # }
          
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
  
}

