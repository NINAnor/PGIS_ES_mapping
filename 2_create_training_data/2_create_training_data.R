# call module selection

library(DT)
library(mapedit)
library(dplyr)
library(shiny)
library(leaflet)
library(leaflet.extras)


APP_CRS <- 4326
# Need to parse out spatial objects if input data is spatial type <- c('sf', 'SpatVector') 
le = TRUE 


user_crs <- APP_CRS
zoomto = "Trondheim"
zoomto_area <- tmaptools::geocode_OSM(zoomto) 
zoomto <- sf::st_as_sfc(zoomto_area$bbox) %>% sf::st_sf() %>%
  sf::st_set_crs(APP_CRS)


dat <- data.frame(es_value = 0 , further_comments = 'CHANGE ME') %>% 
  mutate(leaf_id = 1)


dat <- dat %>% mutate(leaf_id = 1:nrow(dat))
data_copy <- sf::st_as_sf(
  dat,
  geometry = 
    sf::st_sfc(lapply(seq_len(nrow(dat)),function(i){sf::st_polygon(list(cbind(c(0,1,1,0,0), c(0,0,1,1,0))))}))
) %>% sf::st_set_crs(APP_CRS)


ui<-mainPanel(
  editModUI("map"),
  DTOutput("tbl"),
  actionButton("donebtn","save"),
  shiny::uiOutput('dyn_form')
)

server<-function(input, output, session) {
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
  
  ## this is used to keep the zoom of leaflet relevant
  # shiny::observeEvent(input[[nsm(EVT_DRAW)]],{
  #   
  #   click <- input[[nsm('map_draw_new_feature')]]
  #   
  #   if (click$geometry$type == 'Point') {
  #     
  #     clat <- click$geometry$coordinates[[2]]
  #     clng <- click$geometry$coordinates[[1]]
  #     proxy_map  %>%
  #       leaflet::setView(lng = clng, lat = clat, zoom = 
  #                          input[[nsm('map_zoom')]])
  #     
  #   } else {
  #     
  #     click_mat <- matrix(unlist(click$geometry$coordinates),ncol=2, 
  #                         byrow=TRUE)
  #     
  #     if(click$geometry$type == 'LineString'){
  #       clat <- click_mat[[1,2]]
  #       clng <- click_mat[[1,1]]
  #       proxy_map %>%
  #         leaflet::setView(lng = clng, lat = clat, zoom = 
  #                            input[[nsm('map_zoom')]])
  #     } else {
  #       bb <- sf::st_bbox(sf::st_geometry(sf::st_polygon(x = 
  #                                                          list(click_mat))))
  #       proxy_map %>%
  #         leaflet::fitBounds(bb[['xmin']], bb[['ymin']], bb[['xmax']], 
  #                            bb[['ymax']])
  #       
  #     }
  #   }
  # })
  
  # shiny::observeEvent(
  #   input$tbl_rows_selected,
  #   {
  #     selected <- input$tbl_rows_selected
  #     
  #     if(!is.null(selected)) {
  #       rowsel <- df$data[selected, ]
  #       # simple check to see if feature available
  #       #   and leaflet id populated
  #       if 
  #       (all(!is.na(sf::st_coordinates(sf::st_geometry(rowsel)[[1]])))) {
  #         
  #         if (sf::st_geometry_type(rowsel) == 'POINT') {
  #           pnt <- sf::st_coordinates(rowsel) %>% as.data.frame()
  #           proxy_map %>%
  #             leaflet::flyTo(lng = pnt$X, lat = pnt$Y, zoom = 
  #                              input[[nsm('map_zoom')]])
  #         } else {
  #           bb <- sf::st_bbox(sf::st_geometry(rowsel))
  #           proxy_map %>%
  #             leaflet::flyToBounds(bb[['xmin']], bb[['ymin']], 
  #                                  bb[['xmax']], bb[['ymax']])
  #         }
  #         
  #       }
  #     }
  #   }
  # )
  
  
  # update table cells with double click on cell
  shiny::observeEvent(input$tbl_cell_edit, {
    
    df$data <- DT::editData(df$data, input$tbl_cell_edit, 'tbl', 
                            resetPaging = F)
    DT::replaceData(proxy, df$data, rownames = FALSE, resetPaging = FALSE)
    
  })
  
  # provide mechanism to return after all done
  shiny::observeEvent(input$donebtn, {
    
    # if (testing) shiny::stopApp()
    
    if(grepl(class(df$data$geometry)[[1]], "sfc_GEOMETRY")){
      
      if (any(sf::st_is_empty(df$data$geometry))) {
        shinyWidgets::show_alert('Missing Geometry',
                                 'some features are missing geometry, these must be entered before saving',
                                 type = 'warning')
      } else {
        shiny::stopApp({
          out <- df$data %>% dplyr::select(-leaf_id) %>%
            dplyr::mutate(geo_type = as.character(sf::st_geometry_type(.)))
          out <- sf::st_sf(out, crs = user_crs)
          out <- split(out , f = out$geo_type)
          
          # clean bounding box just in case
          for(i in 1:length(out)){
            attr(sf::st_geometry(out[[i]]), "bbox") <- 
              sf::st_bbox(sf::st_union(out[[i]]$geometry))
          }
          
          out
          
        })
      }
      
    } else {
      
      if (any(sf::st_is_empty(df$data$geometry))) {
        shinyWidgets::show_alert('Missing Geometry',
                                 'some features are missing geometry, these must be entered before saving',
                                 type = 'warning')
      } else {
        shiny::stopApp({
          # ensure export is sf and same as input crs
          out <- sf::st_sf(df$data,crs=user_crs)
          
          # clean bounding box just in case
          attr(sf::st_geometry(out), "bbox") <- sf::st_bbox(sf::st_union(out$geometry))
          out %>% dplyr::select(-leaf_id)
        })
      }
    }
  })
  
  
  
} 
shinyApp(ui, server)
