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
      editModUI(ns("map_sel")),
      br(),
      # initial button to save the remap
      actionButton(ns("savepoly"),"save polygons"),
      # show mapping result and slider to adjust importance
      leafletOutput(ns("map_res")),
      uiOutput(ns("slider")),
      br(),
      actionButton(ns("submit"),"save values")
    ),
    conditionalPanel(
      condition = "output.mappable == 2", ns=ns,
      textOutput(ns("test2"))
    )
    
  )
  
}
callback <- c(
  '$("#remove").on("click", function(){',
  '  table.rows(".selected").remove().draw();',
  '});'
)


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
      
      ########## overview map
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
      
      ## if mapping is possible
      if(userES_sel$mapping == "Yes"){
        # load possible user poly of ESID
        poly_path1<-paste0("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/poly_R1/", userID_sel, "_", esID_sel, "_", siteID_sel, ".shp")
        poly_r1<-st_read(poly_path1)
        poly_r1<-st_as_sf(poly_r1)
        poly_r1 <- st_transform(
          poly_r1,
          crs = 4326
        )
        cent_poly <- st_centroid(poly_r1)
        # prepare map for editing      
        map_edit<-leaflet(poly_r1)%>%
          addPolygons(color = "orange", weight = 3, smoothFactor = 0.5,
                      opacity = 1.0, fillOpacity = 0, group = "poly_r1")%>%
          addLabelOnlyMarkers(data = cent_poly,
                              lng = ~st_coordinates(cent_poly)[,1], lat = ~st_coordinates(cent_poly)[,2], label = paste0("Your valuation: ",cent_poly$es_valu),
                              labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE,
                                                          style = list(
                                                            "color" = "red",
                                                            "font-family" = "serif",
                                                            "font-style" = "bold",
                                                            "font-size" = "20px"
                                                          )))%>%
          addProviderTiles(providers$CartoDB.Positron,options = tileOptions(minZoom = 10, maxZoom = 14))%>%
          leaflet.extras::addDrawToolbar(targetGroup='poly_r1',
                                         polylineOptions = F,
                                         polygonOptions = F,
                                         circleOptions = F,
                                         markerOptions = F,
                                         circleMarkerOptions = F,
                                         rectangleOptions = T,
                                         editOptions = editToolbarOptions(),
                                         singleFeature = F)+m1
        
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
          leafmap = map_edit,
          id = "map_sel",
          targetLayerId = "poly_r1",
          record = T,
          sf = T,
          editor = c("leaflet.extras", "leafpm"))
        
        
        
        # edits<-mapedit::editMap(map_edit, targetLayerId = "poly_r1", record = T,sf = T,editor = c("leaflet.extras", "leafpm"))
        
        
        final_edits<-eventReactive(input$savepoly,{
          edits2<-edits()
          # copy old plyg
          final_edits<-poly_r1
          
          #filter polys that have not been edited
          ## attach values of R1 ES
          ## if final_edits geom == p_all(feature type NA) -> status = no_edit
          final_edits$status<-NA
          for(i in 1: nrow(final_edits)){
            if(st_geometry(final_edits[i,]) %in% st_geometry(edits2$all)){
              final_edits[i,]$status<-"no_edit_R2"
            }else{
              final_edits[i,]$status<-"old_geom"
            }
          }
          ## remove status == NA
          # final_edits<-final_edits%>%filter(!is.na(status))
          
          ## old plyg edited
          if(!is_empty(edits2$edited)){
            p_edit<-edits2$edited
            p_edit$X_lflt_d<-p_edit$`_leaflet_id`
            p_edit$ftr_typ<-rep("rectangle",nrow(p_edit))
            p_edit$es_valu<-rep(NA,nrow(p_edit))
            p_edit$esID<-rep(esID_sel,nrow(p_edit))
            p_edit$userID<-rep(userID_sel,nrow(p_edit))
            p_edit$siteID<-rep(siteID_sel,nrow(p_edit))
            p_edit$mppng_r<-rep(mapping_round,nrow(p_edit))
            p_edit$dlph_rn<-rep(2,nrow(p_edit))
            p_edit$drwng_r<-rep(NA,nrow(p_edit))
            p_edit$status<-rep("edited",nrow(p_edit))
            p_edit<-p_edit%>%select(X_lflt_d,ftr_typ,es_valu,esID,userID,siteID,mppng_r,dlph_rn,drwng_r,status)
            final_edits<-rbind(final_edits,p_edit)
            
            ## new plyg
          } 
          
          if(!is_empty(edits2$drawn)){
            p_new<-edits2$drawn
            p_new$X_lflt_d<-p_new$`_leaflet_id`
            p_new$ftr_typ<-rep("rectangle",nrow(p_new))
            p_new$es_valu<-rep(NA,nrow(p_new))
            p_new$esID<-rep(esID_sel,nrow(p_new))
            p_new$userID<-rep(userID_sel,nrow(p_new))
            p_new$siteID<-rep(siteID_sel,nrow(p_new))
            p_new$mppng_r<-rep(mapping_round,nrow(p_new))
            p_new$dlph_rn<-rep(2,nrow(p_new))
            p_new$drwng_r<-rep(NA,nrow(p_new))
            p_new$status<-rep("new_drawn",nrow(p_new))
            p_new<-p_new%>%select(X_lflt_d,ftr_typ,es_valu,esID,userID,siteID,mppng_r,dlph_rn,drwng_r,status)
            final_edits<-rbind(final_edits,p_new)
            
            ## deleted poly (just for tracking)-- do not include in new training
          } 
          
          if(!is_empty(edits2$deleted)){
            p_del<-edits2$deleted
            p_del$X_lflt_d<-p_del$`_leaflet_id`
            p_del$ftr_typ<-rep("rectangle",nrow(p_del))
            p_del$es_valu<-rep(NA,nrow(p_del))
            p_del$esID<-rep(esID_sel,nrow(p_del))
            p_del$userID<-rep(userID_sel,nrow(p_del))
            p_del$siteID<-rep(siteID_sel,nrow(p_del))
            p_del$mppng_r<-rep(mapping_round,nrow(p_del))
            p_del$dlph_rn<-rep(2,nrow(p_del))
            p_del$drwng_r<-rep(NA,nrow(p_del))
            p_del$status<-rep("deleted",nrow(p_del))
            p_del<-p_del%>%select(X_lflt_d,ftr_typ,es_valu,esID,userID,siteID,mppng_r,dlph_rn,drwng_r,status)
            final_edits<-rbind(final_edits,p_del)
          }
          final_edits
          
        })
        
        ### only for drawings and new drawn polys:
        observeEvent(input$savepoly, {
          polygon<-final_edits()
          polygon<-polygon%>%filter(status == "edited" | status == "new_drawn")
          cent_poly <- st_centroid(polygon)
          output$map_res<-renderLeaflet(map_res %>% 
                                          addPolygons(data=polygon) %>%
                                          addLabelOnlyMarkers(data = cent_poly,
                                                              lng = ~st_coordinates(cent_poly)[,1], lat = ~st_coordinates(cent_poly)[,2], label = cent_poly$X_lflt_d,
                                                              labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE,
                                                                                          style = list(
                                                                                            "color" = "red",
                                                                                            "font-family" = "serif",
                                                                                            "font-style" = "bold",
                                                                                            "font-size" = "20px"
                                                                                          ))))
        })
        
        ### create a slider for each of the NEW polygons
        observeEvent(input$savepoly,{
          tbl<-final_edits()
          tbl<-tbl%>%st_drop_geometry()%>%filter(status == "edited" | status == "new_drawn")
          output$slider <- shiny::renderUI({
            ns <- session$ns
            tagList(
              h5("please rate ESx for each area"),
              lapply(1:nrow(tbl),function(n){
                polynr <- tbl[n,]$X_lflt_d
                id<-paste0("id_",polynr)
                lable<-paste0("Polygon Nr: ",polynr)
                sliderInput(ns(id),lable, min = 1, max = 5, step = 1, value = 3)
              })
            )
            
          })
        })
        
        observeEvent(input$submit,{
          poly_all<-final_edits()
          poly_not_edited<-poly_all%>%filter(status == "no_edit_R2")
          poly_edited<-poly_all%>%filter(status == "edited" | status == "new_drawn")
          poly_edited<-as.data.frame(poly_edited)
          poly_edited$es_valu<-rep(NA,nrow(poly_edited))
          sliderval<-list()
          
          # extract the values from the slider
          res<-lapply(1:nrow(poly_edited),function(a){
            var<-paste0("id_",poly_edited[a,]$`X_lflt_d`)
            sliderval[[a]]<-input[[var]]
            return(sliderval)
          })
          vecA <- unlist(res)
          
          # write attributes to geometry
          poly_edited$es_valu <- vecA
          
          poly_edited<-st_as_sf(poly_edited)
          poly_all<-rbind(poly_not_edited,poly_edited)
          polypath <- paste0( 'C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/poly_R2/',userID_sel,"_",esID_sel,"_",siteID_sel,".shp")
          ## save poly
          st_write(poly_all,polypath)
          
          
          ## write information to poly 2
          mapping_param <-
            list(
              esID = esID_sel,
              userID = userID_sel,
              siteID = siteID_sel,
              mappingR2_UID = paste0(userID_sel,"_",esID_sel,"_", siteID_sel),
              area = as.integer(sum(st_area(poly_all))),
              n_poly = as.integer(nrow(poly_all)),
              blog = "test_blog",
              map_adjust = input$map_poss,
              mapping_order = as.integer(mapping_round),
              extrap_RMSE = 0,
              extrap_accIMP = 0,
              extrap_lulcIMP = 0,
              extrap_natIMP = 0
            )
          mapping_param<-as.data.frame(mapping_param)
          
          ############ maxent
          incProgress(amount = 0.1,message = "update data base")
          # write to bq
          insert_upload_job("rgee-381312", "data_base", "es_mappingR2", mapping_param)
          
          
        })
      }
      
      
     
      output$test2<-renderText("you can NOT map")

      
    }
  )
}