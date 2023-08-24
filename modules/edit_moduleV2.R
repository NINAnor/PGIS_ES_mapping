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

remapServer<-function(id, userID_sel, es_descr, userES, studyID, geometry, sf_bound, vis_ind, mapping_round, comb, bands){
  moduleServer(
    id,
    function(input,output,session){
      ns<-session$ns

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
      })#/output image
      
      ## all raster path (adjust!!! recr)
      imgpath1<-paste0(ee_get_assethome(), '/R_1/all_part/',"recr", "_", studyID)
      img_all<-ee$Image(imgpath1)$select("probability")
      
      if(userES_sel$mapping == "Yes"){
        imgpath2<-paste0(ee_get_assethome(), '/R_1/ind_maps/',"1_",userID_sel, "_", esID_sel, "_", studyID)
        img_ind_R1<-ee$Image(imgpath2)
        
        Map$setCenter(10.38649, 63.40271,10)
        m1<-Map$addLayer(
            eeObject = img_ind_R1,
            vis_ind,
            opacity = 0.4,
            name = "Your map from round 1"
          )| Map$addLayer(
            eeObject = img_all,
            vis_ind,
            opacity = 0.4,
            name = "all participants")+
          Map$addLegend(vis_ind, name =paste0("Probability to benefit from ",es_descr_sel$esNAME) , color_mapping = "character")
        
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
            vis_ind,
            opacity = 0.4,
            name = "all participants")+
          Map$addLegend(vis_ind, name =paste0("Probability to benefit from ",es_descr_sel$esNAME) , color_mapping = "character")

        
        output$map_res_all <- renderLeaflet({
          m1
        })
        output$text1<-renderText(paste0("In the previous mapping round you have not mapped ", es_descr_sel$esNAME))
        output$ui2<-renderUI({
          actionButton(ns("confirm2"),"Confirm", class='btn-primary')
        })
        map_edit<-leaflet()
      }
      
      #initialize reactive values
      rv<-reactiveValues(
        edits = reactive({})
      )
      
      observeEvent(input$confirm1,{
        if(input$remap_poss == "Yes"){
          
          rv$edits<-callModule(
            module = editMod,
            leafmap = map_edit,
            id = "map_sel",
            targetLayerId = "poly_r1",
            record = T,
            sf = T,
            editor = c("leaflet.extras", "leafpm")) 
          # edits<-mapedit::editMap(map_edit, targetLayerId = "poly_r1", record = T,sf = T,editor = c("leaflet.extras", "leafpm"))
          
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
              htmlOutput(ns("overlay_result")),
              uiOutput(ns("btn1"))
              # initial button to save the remap
              
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
        
        return(rv$edits)
      })#/observeEvent
      
      ## test if edited polys intersect with each other or study area -- inform user!
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
        
      })#/observer

      ## prepare the final edits, not changed, adjusted, new polys
      final_edits<-eventReactive(input$savepoly,{
        # copy old plyg
          final_edits<-poly_r1
          edits2<-rv$edits()
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
            for(w in 1: nrow(final_edits)){
              if(st_geometry(final_edits[w,])%in%st_geometry(p_del)){
                final_edits$status[w] <-"deleted"
              }
            }

          }

        }

          final_edits

        })#/eventReactive final_edits

      ## only for drawings and new drawn polys:
      observeEvent(input$savepoly, {
        
        final_edits<-final_edits()
        ## not consider the deleted polys
        poly_values<-final_edits%>%dplyr::filter(status == "edited" | status == "new_drawn" | status == "no_edit_R2")
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
        
        # create slider for edited, new and unchanged polys, but indicate that:
        cent_poly <- st_centroid(poly_values)
        tbl<-poly_values%>%st_drop_geometry()
        
        # final map with edited polys to adjust sliders
        output$map_res<-renderLeaflet(back_map1 %>%
                                        addPolygons(data=poly_values) %>%
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
            lapply(1:nrow(tbl),function(n){
              print(as.integer(tbl[n,]$es_valu))
              polynr <- tbl[n,]$X_lflt_d
              id<-paste0("id_",polynr)
              R1_value <-tbl[n,]$es_valu
              ## for new drawn poly (initial value)
              if(tbl[n,]$status == "edited"){
                lable<-paste0("Polygon Nr: ",polynr, " has been edited in R2 - please rate the area")
                sliderInput(ns(id),lable, min = 1, max = 5, step = 1, value = 1)

              }else if(tbl[n,]$status=="no_edit_R2"){
                lable<-paste0("Polygon Nr: ",polynr, " has NOT been edited in R2 - please adjust your old rating if you want")
                sliderInput(ns(id),lable, min = 1, max = 5, step = 1, value = as.integer(tbl[n,]$es_valu))

              }else{
                lable<-paste0("Polygon Nr: ",polynr, " was newly drawn in R2 - please rate the area")
                sliderInput(ns(id),lable, min = 1, max = 5, step = 1, value = 1)
              }
              
              
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
            actionButton(ns("save_val"),"save values")
            
          )
        )
        
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
        
      })#/observeEvent
      
    observeEvent(input$save_val, {
        
        insertUI(
          selector = paste0("#",ns("save_val")),
          where = "afterEnd",
          ui = tagList(
            fluidRow(
              column(5,
                     leafletOutput(ns("gee_map1"))
              ),
              column(1),
              column(5,
                     leafletOutput(ns("gee_map2"))
              )
            ),#/row
            br(),
            h4(paste0("Which of your maps do you think represents the probability to benefit from ", es_descr_sel$esNAME," better?")),
            selectizeInput(ns("select_map"),label="",choices = c("Map round 1","Map round 2", "I don`t know"),options = list(
              onInitialize = I('function() { this.setValue(""); }')
            )),
            uiOutput(ns("cond_final"))
            
          )#/taglist
        )#/inserUI
    

    output$cond_final<-renderUI({
        validate(
          need(input$select_map, 'Please select an option'),
        )
      actionButton(ns("confirm2"),"Confirm", class='btn-primary')
      })
    
    removeUI(
      selector = paste0("#",ns("map_res"))
    )
    removeUI(
      selector = paste0("#",ns("es_quest_how"))
    )
    removeUI(
      selector = paste0("#",ns("slider"))
    )
    removeUI(
      selector = paste0("#",ns("save_val"))
    )
    
    })#/observeEvent
    
      ## save the values of the polys in bq - recalculation of map will be postprocessing R2...
     img_ind_R2<-eventReactive(input$save_val,{
      
        withProgress(message = "save your data",value = 0.1,{
        poly_all<-final_edits()
        poly_train<-poly_all%>%dplyr::filter(status == "edited" | status == "new_drawn" | status == "no_edit_R2")
        # poly_del<-poly_all%>%dplyr::filter(status == "deleted")
        
        poly_train<-as.data.frame(poly_train)
        poly_train$es_val_new<-rep(NA,nrow(poly_train))
        sliderval<-list()
        
        # extract the values from the slider
        res<-lapply(1:nrow(poly_train),function(a){
          var<-paste0("id_",poly_train[a,]$`X_lflt_d`)
          sliderval[[a]]<-input[[var]]
          return(sliderval)
        })
        vecA <- unlist(res)
        
        # write attributes to geometry
        poly_train$es_val_new <- vecA
        
        # poly_del$es_val_new<-NA
        # poly_del$drawing_order<-NA
        
        # a drawing order index
        for(i in 1: nrow(poly_train)){
          poly_train$drawing_order[i] <- as.integer(i)
          
        }
        
        poly_train<-st_as_sf(poly_train)
        # poly_all<-rbind(poly_del,poly_train)
        polypath <- paste0( 'C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/poly_R2/',userID_sel,"_",esID_sel,"_",studyID,".shp")
        ## save poly
        st_write(poly_train,polypath)

        poly_area<-as.numeric(sum(st_area(poly_train)))
        # gee_poly<-rgee::sf_as_ee(poly_train, via = "getInfo")
        incProgress(amount = 0.2,message = "prepare training data")
        
        
        ## N background (outside poly points) according to area of extrapolation
        A_roi<-as.numeric(st_area(sf_bound))
        # area of smallest poly
        A_min<-as.numeric(min(st_area(poly_train)))
        # area of largest poly
        A_max<-as.numeric(max(st_area(poly_train)))
        
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
        pts_out <- st_difference(st_combine(pts_out), st_combine(poly_train)) %>% st_cast('POINT')
        pts_out<-st_as_sf(pts_out)
        pts_out$inside<-rep(0,nrow(pts_out))
        
        
        # inside pts are area + NEW es value weighted
        for (i in 1:nrow(poly_train)) {
          A_tmp <- as.numeric(st_area(poly_train[i,]))
          #tmp_ratio<-A_tmp/A_min
          tmp_ratio<-A_tmp/A_roi
          # npts in this poly must be max_pts*tmp_ratio*es_value
          #tmp_pts = st_sample(polygon[i,], round(tmp_ratio*pts_min,0)*polygon[i,]$es_value,type="random")
          tmp_pts = st_sample(poly_train[i,], round(max_pts*tmp_ratio,0)*poly_train[i,]$es_val_new, type="random")
          tmp_pts<-st_as_sf(tmp_pts)
          tmp_pts$inside<-rep(1,nrow(tmp_pts))
          if(i==1){
            pts_in<-tmp_pts
          }else{
            pts_in<-rbind(pts_in,tmp_pts)
          }
          
          
        }
        pts_ee<-rbind(pts_out,pts_in)
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
        
        
        incProgress(amount = 0.4,message = "save your data")
        ## write information to poly 2
        mapping_param <-
          list(
            esID = esID_sel,
            userID = userID_sel,
            siteID = studyID,
            mappingR2_UID = paste0(userID_sel,"_",esID_sel,"_", studyID),
            area = as.integer(sum(st_area(poly_train))),
            n_poly = as.integer(nrow(poly_train)),
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
        
        
        img_ind_R2<-imageClassified$select("probability")
        
        ############ save map
        incProgress(amount = 0.7,message = "store your map")
        img_assetid <- paste0(ee_get_assethome(), '/R_2/ind_maps/',userID_sel,"_",esID_sel,"_", studyID)
        #
        # #set features of img
        img_ind_R2 <- img_ind_R2$set('es_id', esID_sel,
                                     'userID', userID_sel,
                                     'siteID', studyID,
                                     'order_id', mapping_round,
                                     'delphi_round', 2)
        # # 
        # start_time<-Sys.time()
        # task_img <- ee_image_to_asset(
        #   image = img_ind_R2,
        #   assetId = img_assetid,
        #   overwrite = T,
        #   region = geometry
        # )
        # 
        # task_img$start()
        # 
        # ind_diff<-img_ind_R2$subtract(img_ind_R1)
        
        
      })#/progress ini
      return(img_ind_R2)
    })#/eventReactive prediction
    
      observe({
        req(img_ind_R2)
        img_ind_R2<-img_ind_R2()
        withProgress(message = "prepare interactive map 1",value = 0.8,{
          
          
          Map$setCenter(10.38649, 63.40271,10) 
          result<-Map$addLayer(
            eeObject = img_ind_R2,
            vis_ind,
            "Your new probability of ES",
            opacity = 0.4)|
            Map$addLayer(
              eeObject = img_ind_R1,
              vis_ind,
              "Your old probability of ES",
              opacity = 0.4)+
            Map$addLegend(vis_ind, name =paste0("Probability to benefit from ",es_descr_sel$esNAME) , color_mapping = "character",  position = "topright")
          
          incProgress(amount = 0.9,message = "prepare interactive map 2")
          
          ind_diff<-img_ind_R2$subtract(img_ind_R1)
          ind_diff<-ind_diff$divide(img_ind_R2)
          
          Map$setCenter(10.38649, 63.40271,10)
          result2<- Map$addLayer(
            eeObject = ind_diff,
            vis_ind,
            "Difference between R1 - R2",
            opacity = 0.4)+
            Map$addLegend(vis_ind, name ="Relative difference new and old map" , color_mapping = "character", position = "topright")
          
          output$gee_map1 <- renderLeaflet({
            result%>%
              addControl(h3("Your new probability of ES"), position = "bottomleft", className="map-title")%>%
              addControl(h3("Your old probability of ES"), position = "bottomright", className="map-title")
            
          })
          output$gee_map2 <- renderLeaflet({
            result2
          })
          incProgress(amount = 1,message = "done")
          
        })
      })
    
      observeEvent(input$select_map,{
      ### save input decision here
      

      })#/observeEvent
      
      cond <- reactive({input$confirm2})
      return(cond)
      
    }#/function server session
  )#/module server
  
}#/server
