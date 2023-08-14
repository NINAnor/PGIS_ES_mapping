## remapping module

remapUI<- function(id, label = "selector") {
  ns<-NS(id)
  tagList(
    uiOutput(ns("title_es")),
    br(),
    fluidRow(
      column(5,
             uiOutput(ns("descr_es"))),
      column(2),
      column(5,
             uiOutput(ns("image_es")))
    ),
    br(),
    conditionalPanel(
      condition = "userES_sel$mapping == 'Yes'",
      ns=ns,
      textOutput(ns("text0")),
      br(),
      leafletOutput(ns("map_res_ind")),
      br(),
      h6(paste0("Why people rated areas as high potential to benefit from ")),
      DTOutput(ns("blog")),
      br(),
      uiOutput(ns("remap_poss")),
      br(),
      uiOutput(ns("cond_b1"))
      
      
      # actionButton(ns("confirm"),'confirm')
    ),#/cond ui 1
    conditionalPanel(
      condition = "userES_sel$mapping == 'No'",
      ns = ns,
      # paste0("In this map you can see areas of high probability to benefit from ",es_descr_sel$esNAME),
      textOutput(ns("text1")),
      br(),
      leafletOutput(ns("map_res_all")),
      br(),
      uiOutput(ns("ui2"))
    ),#/cond ui 2
  )#/tag list
}#/ui module

callback <- c(
  '$("#remove").on("click", function(){',
  '  table.rows(".selected").remove().draw();',
  '});'
)

remapServer<-function(id, userID_sel, es_descr, userES, studyID, geometry, sf_bound, vis_qc, mapping_round){
  moduleServer(
    id,
    function(input,output,session){
      ns<-session$ns
      # 

      userES_sel<-userES%>%dplyr::filter(userID == userID_sel)%>%slice(mapping_round)
      esID_sel<-userES_sel$esID
      es_descr_sel<-es_descr%>%dplyr::filter(esID %in% esID_sel)
      output$title_es<-renderUI(h5(es_descr_sel$esNAME))
      blog_data_sel<-userES%>%dplyr::filter(esID %in% esID_sel & (blog !="NA"))%>%filter(blog!="")%>%select(blog)
      output$blog<-renderDT(blog_data_sel,rownames= FALSE, colnames="Why people choosed their sites")
      output$descr_es<-renderUI(es_descr_sel$esDESCR)

      output$remap<-renderUI(h4(paste0("Modify, add or delete areas that provide good ",es_descr_sel$esNAME)))
      output$es_quest_how<-renderUI(h6(paste0("How do you rate the quality of ",es_descr_sel$esNAME, " for your adjusted areas?")))
      
      output$image_es<-renderUI({
        tags$figure(
          class = "centerFigure",
          tags$img(
            src = paste0(esID_sel,".jpg"),
            width = 600,
            alt = "Picture of an astragalus (bone die)"
          ),
          tags$figcaption("Image of Astragalus by Yaan, 2007")
        )
      })
      
      ## all raster path (adjust!!! recr)
      imgpath1<-paste0(ee_get_assethome(), '/R_1/all_part/',"recr", "_", studyID)
      img_all<-ee$Image(imgpath1)$select("probability")
      
      if(userES_sel$mapping == "Yes"){
        imgpath2<-paste0(ee_get_assethome(), '/R_1/ind_maps/',"1_",userID_sel, "_", esID_sel, "_", studyID)
        img_ind<-ee$Image(imgpath2)
        
        Map$setCenter(10.38649, 63.40271,10)
        m1<-Map$addLayer(
            eeObject = img_ind,
            vis_qc,
            opacity = 0.4,
            name = "Your map"
          )| Map$addLayer(
            eeObject = img_all,
            vis_qc,
            opacity = 0.4,
            name = "all participants")+
          Map$addLegend(vis_qc, name =paste0("Probability to benefit from ",es_descr_sel$esNAME) , color_mapping = "character")
        
        output$text0<-renderText(paste0("In the previous mapping round you have mapped ", es_descr_sel$esNAME, ". The maps below show the areas of high probability to benefit form ",es_descr_sel$esNAME,
                                        " based on your individual contribution and all other participants contribution."))
        
        output$map_res_ind <- renderLeaflet({
          m1
        })
          
          output$remap_poss<-renderUI({
            tagList(
            h4(paste0("Do you want to adjust your areas for good ", es_descr_sel$esNAME,"?")),
            selectizeInput(ns("remap_poss"),label="",choices = c("Yes","No"),options = list(
              onInitialize = I('function() { this.setValue(""); }')
            ))
            )
          })
          
          output$cond_b1<-renderUI({
            validate(
              need(input$remap_poss, 'Please select an option')
            )
            actionButton(ns("confirm1"), "confirm")
          })
          
          
          poly_path1<-paste0("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/poly_R1/", userID_sel, "_", esID_sel, "_", studyID, ".shp")
          print(poly_path1)
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
                                lng = ~st_coordinates(cent_poly)[,1], lat = ~st_coordinates(cent_poly)[,2], label = paste0("Your valuation in round 1: ",cent_poly$es_valu),
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
                                           singleFeature = F)+
            m1
          

      }else{
        Map$setCenter(10.38649, 63.40271,10)
        m1<-Map$addLayer(
            eeObject = img_all,
            vis_qc,
            opacity = 0.4,
            name = "all participants")+
          Map$addLegend(vis_qc, name =paste0("Probability to benefit from ",es_descr_sel$esNAME) , color_mapping = "character")

        
        output$map_res_all <- renderLeaflet({
          m1
        })
        output$text1<-renderText(paste0("In the previous mapping round you have not mapped ", es_descr_sel$esNAME))
        output$ui2<-renderUI({
          actionButton(ns("confirm2"),"Confirm", class='btn-primary')
        })
        map_edit<-leaflet()
      }
      
      observeEvent(input$confirm1,{
        if(input$remap_poss == "Yes"){
          output$blog2<-renderDT(blog_data_sel,rownames= FALSE, colnames="Blog entries")
          insertUI(
            selector = paste0("#",ns("confirm1")),
            where = "afterEnd",
            ui = tagList(
              uiOutput(ns("remap")),
              fluidRow(strong(" -please adjust inside the boundaries")),
              br(),
              fluidRow(strong(" -please do not overlap polygons")),
              fluidRow(
                column(5,editModUI(ns("map_sel"))
                       ),
                column(2,
                       ),
                column(5,DTOutput(ns("blog2"))
                       )
              ),
              br(),
              br(),
              # initial button to save the remap
              actionButton(ns("savepoly"),"save polygons")
            )
          )

          
          
        }else{
          insertUI(
            selector = paste0("#",ns("confirm1")),
            where = "afterEnd",
            ui = tagList(
              "We will use your data from the previous round!",
              br(),
              actionButton(ns("confirm2"),"Confirm", class='btn-primary')
            )
          )
          
          ## save R1 poly values in R2!
          # polypath <- paste0( 'C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/poly_R2/',userID_sel,"_",esID_sel,"_",studyID,".shp")
          # ## save poly
          # st_write(poly_r1,polypath)
          print(poly_r1)
          
          ## write information to poly 2
          mapping_param <-
            list(
              esID = esID_sel,
              userID = userID_sel,
              siteID = studyID,
              mappingR2_UID = paste0(userID_sel,"_",esID_sel,"_", studyID),
              area = as.integer(sum(st_area(poly_r1))),
              n_poly = as.integer(nrow(poly_r1)),
              blog = "test_blog",
              map_adjust = input$remap_poss,
              mapping_order = as.integer(mapping_round),
              extrap_RMSE = 0,
              extrap_accIMP = 0,
              extrap_lulcIMP = 0,
              extrap_natIMP = 0,
              edited = "No"
            )
          mapping_param<-as.data.frame(mapping_param)
          
          # write to bq
          insert_upload_job("rgee-381312", "data_base", "es_mappingR2", mapping_param)
          
        }#/else
        removeUI(
          selector = paste0("#",ns("remap_poss"))
        )
        removeUI(
          selector = paste0("#",ns("blog"))
        )
        removeUI(
          selector = paste0("#",ns("map_res_ind"))
        )
        removeUI(
          selector = paste0("#",ns("confirm1"))
        )
        removeUI(
          selector = paste0("#",ns("text0"))
        )
        
        return(edits)
      })#/observer
      
                
          edits<-callModule(
            module = editMod,
            leafmap = map_edit,
            id = "map_sel",
            targetLayerId = "poly_r1",
            record = T,
            sf = T,
            editor = c("leaflet.extras", "leafpm")) 

    ## test if edited polys intersect
          


#    
#            
#         # edits<-mapedit::editMap(map_edit, targetLayerId = "poly_r1", record = T,sf = T,editor = c("leaflet.extras", "leafpm"))
#       
      ## prepare the final edits, not changed, adjusted, new polys
      final_edits<-eventReactive(input$savepoly,{
        # copy old plyg
          final_edits<-poly_r1
          edits2<-edits()
          #filter polys that have not been edited
          ## attach values of R1 ES
          ## if final_edits geom == p_all(feature type NA) -> status = no_edit
          final_edits$status<-rep("old_geom",nrow(final_edits))

          # if sth has been changed, moved deleted edited
        if(!is.null(edits2$all)){

          for(i in 1: nrow(final_edits)){
            if(st_geometry(final_edits[i,]) %in% st_geometry(edits2$all)){
              final_edits[i,]$status<-"no_edit_R2"
            }else{
              final_edits[i,]$status<-"old_geom"
            }
          }
          if(!is_empty(edits2$drawn)){
            p_new<-edits2$drawn
            p_new$X_lflt_d<-p_new$`_leaflet_id`
            p_new$ftr_typ<-rep("rectangle",nrow(p_new))
            p_new$es_valu<-rep(NA,nrow(p_new))
            p_new$esID<-rep(esID_sel,nrow(p_new))
            p_new$userID<-rep(userID_sel,nrow(p_new))
            p_new$siteID<-rep(studyID,nrow(p_new))
            p_new$mppng_r<-rep(mapping_round,nrow(p_new))
            p_new$dlph_rn<-rep(2,nrow(p_new))
            p_new$drwng_r<-rep(NA,nrow(p_new))
            p_new$status<-rep("new_drawn",nrow(p_new))
            p_new<-p_new%>%select(X_lflt_d,ftr_typ,es_valu,esID,userID,siteID,mppng_r,dlph_rn,drwng_r,status)
            for(u in 1: nrow(p_new)){
              if(p_new$X_lflt_d[u] %in% final_edits$X_lflt_d){
              a<-p_new[u,]%>%dplyr::filter(!p_new$X_lflt_d[u] %in% final_edits$X_lflt_d)
              final_edits<-rbind(final_edits,a)
              }else{
              final_edits<-rbind(final_edits,p_new[u,])
              }
            }
          }

          ## old plyg edited
          if(!is_empty(edits2$edited)){
            p_edit<-edits2$edited
            p_edit$X_lflt_d<-p_edit$`_leaflet_id`
            p_edit$ftr_typ<-rep("rectangle",nrow(p_edit))
            p_edit$es_valu<-rep(NA,nrow(p_edit))
            p_edit$esID<-rep(esID_sel,nrow(p_edit))
            p_edit$userID<-rep(userID_sel,nrow(p_edit))
            p_edit$siteID<-rep(studyID,nrow(p_edit))
            p_edit$mppng_r<-rep(mapping_round,nrow(p_edit))
            p_edit$dlph_rn<-rep(2,nrow(p_edit))
            p_edit$drwng_r<-rep(NA,nrow(p_edit))
            p_edit$status<-rep("edited",nrow(p_edit))
            p_edit<-p_edit%>%select(X_lflt_d,ftr_typ,es_valu,esID,userID,siteID,mppng_r,dlph_rn,drwng_r,status)
            for(v in 1: nrow(p_edit)){
              if(p_edit$X_lflt_d[v] %in% final_edits$X_lflt_d){
                a<-p_edit[v,]%>%dplyr::filter(!p_edit$X_lflt_d[v] %in% final_edits$X_lflt_d)
                final_edits<-rbind(final_edits,a)
              }else{
                final_edits<-rbind(final_edits,p_edit[v,])
              }
            }
          }



          if(!is_empty(edits2$deleted)){
            p_del<-edits2$deleted
            p_del$X_lflt_d<-p_del$`_leaflet_id`
            p_del$ftr_typ<-rep("rectangle",nrow(p_del))
            p_del$es_valu<-rep(NA,nrow(p_del))
            p_del$esID<-rep(esID_sel,nrow(p_del))
            p_del$userID<-rep(userID_sel,nrow(p_del))
            p_del$siteID<-rep(studyID,nrow(p_del))
            p_del$mppng_r<-rep(mapping_round,nrow(p_del))
            p_del$dlph_rn<-rep(2,nrow(p_del))
            p_del$drwng_r<-rep(NA,nrow(p_del))
            p_del$status<-rep("deleted",nrow(p_del))
            p_del<-p_del%>%select(X_lflt_d,ftr_typ,es_valu,esID,userID,siteID,mppng_r,dlph_rn,drwng_r,status)
            for(w in 1: nrow(p_del)){
              if(p_del$X_lflt_d[w] %in% final_edits$X_lflt_d){
                final_edits<-final_edits%>%dplyr::filter(!final_edits$X_lflt_d %in% p_del$X_lflt_d[w])
              }
            }
          }

        }

          final_edits

        })
# 
      ## only for drawings and new drawn polys:
      observeEvent(input$savepoly, {

        final_edits<-final_edits()
        poly_new<-final_edits%>%dplyr::filter(status == "edited" | status == "new_drawn")
        #background map to display edited polys
        back_map1<-leaflet(sf_bound)%>%
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

        # if edits were done:
        if(nrow(poly_new)>0){
          cent_poly <- st_centroid(poly_new)
          tbl<-poly_new%>%st_drop_geometry()

          # final map with edited polys to adjust sliders
          output$map_res<-renderLeaflet(back_map1 %>%
                                          addPolygons(data=poly_new) %>%
                                          addLabelOnlyMarkers(data = cent_poly,
                                                              lng = ~st_coordinates(cent_poly)[,1], lat = ~st_coordinates(cent_poly)[,2], label = cent_poly$X_lflt_d,
                                                              labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE,
                                                                                          style = list(
                                                                                            "color" = "red",
                                                                                            "font-family" = "serif",
                                                                                            "font-style" = "bold",
                                                                                            "font-size" = "20px"
                                                                                          ))))


          output$slider <- renderUI({
            ns <- session$ns
            tagList(
              paste0("The Nr. of the slider refer to the number of the rectangle in the map"),
              br(),
              br(),
              lapply(1:nrow(tbl),function(n){
                polynr <- tbl[n,]$X_lflt_d
                id<-paste0("id_",polynr)
                lable<-paste0("Polygon Nr: ",polynr)
                sliderInput(ns(id),lable, min = 1, max = 5, step = 1, value = 3)
              })
            )

          })#/slider

          insertUI(
            selector = paste0("#",ns("savepoly")),
            where = "afterEnd",
            ui = tagList(
              uiOutput(ns("es_quest_how")),
              br(),
              leafletOutput(ns("map_res")),
              br(),
              uiOutput(ns("slider")),
              br(),
              actionButton(ns("confirm2"),"Confirm", class='btn-primary')

            )
          )
          #if no edits were done
        }else{

          cent_poly <- st_centroid(final_edits)
          # final map with edited polys to adjust sliders
          output$map_res<-renderLeaflet(back_map1 %>%
                                          addPolygons(data=final_edits) %>%
                                          addLabelOnlyMarkers(data = cent_poly,
                                                              lng = ~st_coordinates(cent_poly)[,1], lat = ~st_coordinates(cent_poly)[,2], label = paste0("Your valuation: ",cent_poly$es_valu),
                                                              labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE,
                                                                                          style = list(
                                                                                            "color" = "red",
                                                                                            "font-family" = "serif",
                                                                                            "font-style" = "bold",
                                                                                            "font-size" = "20px"
                                                                                          ))))
          insertUI(
            selector = paste0("#",ns("savepoly")),
            where = "afterEnd",
            ui = tagList(
              "You have not changed any areas, we will use your data from the previous round!",
              br(),
              leafletOutput(ns("map_res")),
              br(),
              actionButton(ns("confirm2"),"Confirm", class='btn-primary')

            )
          )

        }#/else no edits

        removeUI(
          selector = paste0("#",ns("map_sel"),"-map"))
        removeUI(
          selector = paste0("#",ns("blog2"))
        )
        removeUI(
          selector = paste0("#",ns("savepoly"))
        )
        removeUI(
          selector = paste0("#",ns("remap"))
        )

        })#/observer
#         
#       ## save the values of the polys in bq - recalculation of map will be postprocessing R2...
      observeEvent(input$confirm2,{
        withProgress(message = "save your data",value = 0.1,{
          poly_all<-final_edits()
          poly_edited<-poly_all%>%dplyr::filter(status == "edited" | status == "new_drawn")
          poly_not_edited<-poly_all%>%dplyr::filter(status == "no_edit_R2" | status == "old_geom")
          
          if(nrow(poly_edited)>0){
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
            polypath <- paste0( 'C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/poly_R2/',userID_sel,"_",esID_sel,"_",studyID,".shp")
            ## save poly
            st_write(poly_all,polypath)
            print(poly_all)
            incProgress(amount = 0.4,message = "save your data")
            ## write information to poly 2
            mapping_param <-
              list(
                esID = esID_sel,
                userID = userID_sel,
                siteID = studyID,
                mappingR2_UID = paste0(userID_sel,"_",esID_sel,"_", studyID),
                area = as.integer(sum(st_area(poly_all))),
                n_poly = as.integer(nrow(poly_all)),
                blog = "test_blog",
                map_adjust = input$remap_poss,
                mapping_order = as.integer(mapping_round),
                extrap_RMSE = 0,
                extrap_accIMP = 0,
                extrap_lulcIMP = 0,
                extrap_natIMP = 0,
                edited = "Yes"
              )
            incProgress(amount = 0.6,message = "save your data")
            
            mapping_param<-as.data.frame(mapping_param)
            
            # write to bq
            insert_upload_job("rgee-381312", "data_base", "es_mappingR2", mapping_param)
            incProgress(amount = 1,message = "save your data")
          }else{
            polypath <- paste0( 'C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/poly_R2/',userID_sel,"_",esID_sel,"_",studyID,".shp")
            ## save poly
            st_write(poly_all,polypath)
            print(poly_not_edited)
            
            ## write information to poly 2
            mapping_param <-
              list(
                esID = esID_sel,
                userID = userID_sel,
                siteID = studyID,
                mappingR2_UID = paste0(userID_sel,"_",esID_sel,"_", studyID),
                area = as.integer(sum(st_area(poly_not_edited))),
                n_poly = as.integer(nrow(poly_not_edited)),
                blog = "test_blog",
                map_adjust = input$remap_poss,
                mapping_order = as.integer(mapping_round),
                extrap_RMSE = 0,
                extrap_accIMP = 0,
                extrap_lulcIMP = 0,
                extrap_natIMP = 0,
                edited = "No"
              )
            mapping_param<-as.data.frame(mapping_param)
            incProgress(amount = 0.6,message = "save your data")
            # write to bq
            insert_upload_job("rgee-381312", "data_base", "es_mappingR2", mapping_param)
            incProgress(amount = 1,message = "save your data")
          }#/else
          
        })#/progress ini
        
        })#/observer

#         
        cond <- reactive({input$confirm2})

        return(cond)

    }#/function server session
  )#/module server
  
}#/server
