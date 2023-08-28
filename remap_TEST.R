# edit moduleV2
########### UI of remapping module
library(shiny)
library(leaflet)
library(mapedit)
library(sf)
library(dplyr)
library(rgee)
library(DT)
library(shinycssloaders)
library(leafem)
library(tibble)
library(leafpop)
library(mapview)
library(shinyRadioMatrix)
library(shinylogs)
library(leaflet.extras)
library(leaflet.extras2)
library(stringi)
library(shinyWidgets)
library(tidyverse)
library(bigrquery)
library(DBI)


ee_Initialize()
bq_auth(path = "C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/rgee-381312-85272383f82d.json")
con <- dbConnect(
  bigrquery::bigquery(),
  project = "rgee-381312",
  dataset = "data_base",
  billing = "rgee-381312"
)

studyID<-"NOR-SNJ"

#Resolution ES mapping
res_on<-100
res_off<-1000

userES <- tbl(con, "es_mappingR1")
#dplyr sql
userES <- select(userES, userID, esID, mapping, siteID, blog) %>% filter(siteID == studyID)%>%
  collect()


geometry <- ee$Geometry$Rectangle(
  coords = c(10.30, 63.35, 10.50, 63.5),
  proj = "EPSG:4326",
  geodesic = FALSE
)

## here modify code according to siteID!

bound_reg<-ee$FeatureCollection("FAO/GAUL_SIMPLIFIED_500m/2015/level2")$
  filter(ee$Filter$eq("ADM2_CODE",23463))
# sf_bound<-ee$FeatureCollection("FAO/GAUL_SIMPLIFIED_500m/2015/level2")$
#   filter(ee$Filter$eq("ADM2_CODE",23463))
sf_bound <- ee_as_sf(x = bound_reg)


lulc <- ee$Image("COPERNICUS/CORINE/V20/100m/2018")
lulc<-lulc$resample("bilinear")$reproject(crs= "EPSG:4326",scale=res_on)
lulc<-lulc$clip(bound_reg)$rename("ON_LULC")


acc_pat<-paste0(ee_get_assethome(), '/acc_old')
acc<-ee$Image(acc_pat)
acc<-acc$resample("bilinear")$reproject(crs= "EPSG:4326",scale=res_on)$clip(bound_reg)$rename("ON_ACC")

nat_pat<-paste0(ee_get_assethome(), '/natu')
nat<-ee$Image(nat_pat)
nat<-nat$resample("bilinear")$reproject(crs= "EPSG:4326",scale=res_on)$clip(bound_reg)$rename("ON_INT")

comb<-ee$Image$cat(lulc,acc, nat)
bands <- list("ON_LULC","ON_ACC","ON_INT")


labels <- c("low", "moderate", "intermediate", "high","very high")
cols   <- c("#e80909", "#fc8803", "#d8e03f", "#c4f25a","#81ab1f")
vis_ind <- list(min = 0, max = 1, palette = cols, values = labels)




### download es_descr table from bq once
es_descr <- tbl(con, "es_descr")
es_descr <- select(es_descr, esID, esNAME, esDESCR) %>% collect()

### download blog with not null blog and siteID given pass this to mapping module R2 (need only blog col and esID col)
userES <- tbl(con, "es_mappingR1")
userES <- select(userES, userID, esID, mapping, siteID, blog) %>% filter(siteID == studyID)%>%
  collect()

userID_sel = "OGln2sNZS4"
mapping_round<-1



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
      fluidRow(
        column(6,leafletOutput(ns("map_res_ind"))),
        column(6, DTOutput(ns("blog")))
      ),
      br(),
      uiOutput(ns("remap_poss")),
      # conditionalPanel(
      #   condition = "input.remap_poss != ''",
      #  actionButton(ns("confirm1"),"confirm") 
      # )
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
    # conditionalPanel(
    #   condition = "input.slider>0", ns=ns,
    #   actionButton(ns("confirm2"),"Next task", class='btn-primary')
    # )#/cond final btn
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
      # 
      
      userES_sel<-userES%>%dplyr::filter(userID == userID_sel)%>%slice(mapping_round)
      esID_sel<-userES_sel$esID
      es_descr_sel<-es_descr%>%dplyr::filter(esID %in% esID_sel)
      output$title_es<-renderUI(h5(es_descr_sel$esNAME))
      blog_data_sel<-userES%>%dplyr::filter(esID %in% esID_sel & (blog !="NA"))%>%filter(blog!="")%>%select(blog)
      output$blog<-renderDT(blog_data_sel,rownames= FALSE, colnames="Why people choosed their sites")
      output$descr_es<-renderUI(es_descr_sel$esDESCR)
      
      output$remap<-renderUI(h6(paste0("Modify, add or delete areas that provide good ",es_descr_sel$esNAME)))
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
      
      if(userES_sel$mapping== "Yes"){
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
          Map$addLegend(vis_ind, name =paste0("Probability to benefit from ",es_descr_sel$esNAME) , color_mapping = "character", position = "topright")
          
        
        output$text0<-renderText(paste0("In the previous mapping round you have mapped ", es_descr_sel$esNAME, ". The maps below show the areas of high probability to benefit form ",es_descr_sel$esNAME,
                                        " based on your individual contribution and all other participants contribution."))
        
        output$map_res_ind <- renderLeaflet({
          m1
        })
        
        output$remap_poss<-renderUI({
          tagList(
            h6(paste0("Do you want to adjust your areas for good ", es_descr_sel$esNAME,"?")),
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
        # print(poly_path1)
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
          # actionButton(ns("confirm2"),"Confirm", class='btn-primary')
          h6(paste0("Based on the map you see now, do you feel able to map good spots for ", es_descr_sel$esNAME,"?"))
          selectizeInput(ns("map_ini"),label="",choices = c("Yes","No"),options = list(
            onInitialize = I('function() { this.setValue(""); }')
          ))
          validate(
            need(input$map_ini, 'Please select an option')
          )
          actionButton(ns("confirm3"), "confirm")
          
        })
        map_edit<-leaflet()
      }
      
      
      
      observeEvent(input$confirm3,{
        if(input$map_ini == "Yes"){
          #mapping light?

        }else{
          
        }
        
      })
      
      
      rv<-reactiveValues(
        edits = reactive({})
      )
      # edits<-mapedit::editMap(map_edit, targetLayerId = "poly_r1", record = T,sf = T,editor = c("leaflet.extras", "leafpm"))
      
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
          
          output$blog2<-renderDT(blog_data_sel,rownames= FALSE, colnames="Blog entries")
          insertUI(
            selector = paste0("#",ns("confirm1")),
            where = "afterEnd",
            ui = tagList(
              uiOutput(ns("remap")),
              fluidRow(" -please adjust inside the boundaries"),
              br(),
              fluidRow(" -please do not overlap polygons"),
              editModUI(ns("map_sel")),
              htmlOutput(ns("overlay_result")),
              uiOutput(ns("btn1")),
              column(5,DTOutput(ns("blog2")),
            )
          ))

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
        
        # return(edits)
      })#/observer
      
      observe({
        req(rv$edits)
        rectangles <- rv$edits()$all
        
        n_poly<-nrow(as.data.frame(rectangles))
        # print(n_poly)
        output$btn1<-renderUI(
          actionButton(ns("savepoly"),"save")
        )
        output$overlay_result <- renderText({
          "Save or draw further polygons"
        })
        
        if(n_poly==1){
          n_within<-nrow(as.data.frame(st_within(rectangles,sf_bound)))
          if(n_within<n_poly){
            output$overlay_result <- renderText({
              paste("<font color=\"#FF0000\"><b>","You can`t save the polygons:","</b> <li>Place your polygon completely into the the study area<li/></font>")
            })
            removeUI(
              selector = paste0("#",ns("savepoly")))
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
      
      ## prepare the final edits, not changed, adjusted, new polys
      final_edits<-eventReactive(input$savepoly,{
        final_edits<-poly_r1
        r2_edits<-rv$edits()
        
        
        ## indicate all R1 as old_geom
        final_edits$status<-rep("no_edit_R2",nrow(final_edits))
        # replace delphi round
        final_edits$dlph_rn<-rep(2,nrow(final_edits))
        
        if(!is_empty(r2_edits$all)){
          for(i in 1: nrow(final_edits)){
            if(st_geometry(final_edits[i,]) %in% st_geometry(r2_edits$all)){
              final_edits[i,]$status<-"no_edit_R2"
            }else{
              final_edits[i,]$status<-"old_geom_r1"
            }#/if else
          }#/for
          ##drawings
          if(!is.null(r2_edits$drawn)){
            for(k in 1:nrow(r2_edits$drawn)){
              if(st_geometry(r2_edits$drawn[k,])%in%st_geometry(r2_edits$all) & !st_geometry(r2_edits$drawn[k,])%in%st_geometry(final_edits)){
                p_new<-r2_edits$finished[k,]
                p_new<-r2_edits$drawn[k,]
                p_new$X_lflt_d<-p_new$`_leaflet_id`
                p_new$ftr_typ<-"rectangle"
                p_new$es_valu<-NA
                p_new$esID<-esID_sel
                p_new$userID<-userID_sel
                p_new$siteID<-studyID
                p_new$mppng_r<-mapping_round
                p_new$dlph_rn<-2
                p_new$drwng_r<-NA
                p_new$status<-"new_drawn"
                p_new<-p_new%>%select(X_lflt_d,ftr_typ,es_valu,esID,userID,siteID,mppng_r,dlph_rn,drwng_r,status)
                final_edits<-rbind(final_edits,p_new)
              }#/if geom is in final edits
            }#/for
          }#/not null edits
          if(!is.null(r2_edits$edited)){
            for(l in 1:nrow(r2_edits$edited)){
              if(st_geometry(r2_edits$edited[l,])%in%st_geometry(r2_edits$all)){
                p_edit<-r2_edits$edited[l,]
                p_edit<-r2_edits$edited[l,]
                p_edit$X_lflt_d<-p_edit$`_leaflet_id`
                p_edit$ftr_typ<-"rectangle"
                p_edit$es_valu<-NA
                p_edit$esID<-esID_sel
                p_edit$userID<-userID_sel
                p_edit$siteID<-studyID
                p_edit$mppng_r<-mapping_round
                p_edit$dlph_rn<-2
                p_edit$drwng_r<-NA
                p_edit$status<-"edited"
                p_edit<-p_edit%>%select(X_lflt_d,ftr_typ,es_valu,esID,userID,siteID,mppng_r,dlph_rn,drwng_r,status)
                final_edits<-rbind(final_edits,p_edit)
              }#/if geom is in final edits
            }#/for
          }#/not null edits
        }#/ not null any edits
        final_edits<-final_edits%>%filter(status!="old_geom_r1")
        final_edits
      })
      # 
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
              lapply(1:nrow(tbl),function(n){
                print(as.integer(tbl[n,]$es_valu))
              polynr <- tbl[n,]$X_lflt_d
              id<-paste0("id_",polynr)
              R1_value <-tbl[n,]$es_valu
              ## for new drawn poly (initial value)
              if(tbl[n,]$status == "edited"){
                lable<-paste0("Polygon Nr: ",polynr, " has been edited in R2 - please rate the area")
                sliderInput(ns(id),lable, min = 1, max = 5, step = 1, value = 1)
                # updateSliderInput(session, ns(id),value = as.integer(tbl[n,]$es_valu))
                
              }else if(tbl[n,]$status=="no_edit_R2"){
                lable<-paste0("Polygon Nr: ",polynr, " has NOT been edited in R2 - please adjust your old rating if you want")
                sliderInput(ns(id),lable, min = 1, max = 5, step = 1, value = as.integer(tbl[n,]$es_valu))
                # updateSliderInput(session, ns(id),value = as.integer(tbl[n,]$es_valu))
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
        
      })#/observer
      #       
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
            )#/row
            
            
          )#/taglist
        )#/inserUI
        
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

      })
      
      
      #       ## save the values of the polys in bq - recalculation of map will be postprocessing R2...
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
          # st_write(poly_all,polypath)
           # st_write(poly_train,polypath)
          ######################################################################
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
          # insert_upload_job("rgee-381312", "data_base", "es_mappingR2", mapping_param)
       
          
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
          # 
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

          

          ############ prepare map
          # incProgress(amount = 1,message = "done")
          

        })#/progress ini
        return(img_ind_R2)
      })#/eventReactive
      
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
      #         
      cond <- reactive({input$confirm2})
      
      return(cond)
      
    }#/function server session
  )#/module server
  
}#/server

################# test App
#
ui <- fluidPage(
  # theme = bslib::bs_theme(bootswatch = "cerulean"),
  titlePanel( title =  div(img(src="wendy_logo.png", width ='90'), 'POC remapping ecosystem services'), windowTitle = "ES remapping" ),
  tabsetPanel(id = "inTabset",
              tabPanel(title = "Your Task", value = "p1",
                       h5("This explains what you are asked to do in the following task"),
                       actionButton("test","next")
              ),
              tabPanel(title = "Mapping Task", value = "p2",
                       remapUI("remap1", "mapping1")
              ))
)

server <- function(input, output, session) {
  userID_sel = reactive("OGln2sNZS4")
  mapping_round<-1
  
  hideTab(inputId = "inTabset", target = "p2")


    observeEvent(input$test, {
      updateTabsetPanel(session, "inTabset",
                        selected = "p2")
    })
    observeEvent(input$test, {
      hideTab(inputId = "inTabset",
              target = "p1")
  })
    observeEvent(input$test, {
      userID_sel<-userID_sel()
      hideTab(inputId = "inTabset", target = "p2")
      hideTab(inputId = "inTabset",
              target = "p1")
      showTab(inputId = "inTabset", target = "p2")
      remapServer("remap1", userID_sel, es_descr, userES, studyID, geometry, sf_bound, vis_ind, mapping_round, comb, bands)

    })
  #


       # val<- remapServer("remap1", isolate(userID_sel), es_descr, userES, studyID, geometry, sf_bound, vis_qc, 1)
  #


    # observeEvent(input$test,{
    #   userID_sel<-userID_sel()
    #   remapServer("remap1", userID_sel, es_descr, userES, studyID, geometry, sf_bound, vis_qc, 1)
    # })




    # observeEvent(val(),{
    #   req(val)
    #
    #   updateTabsetPanel(session, "inTabset",
    #                     selected = "p1")
    #   hideTab(inputId = "inTabset",
    #           target = "p2")
    #   showTab(inputId = "inTabset", target = "p1")
    #
    # })
  #
  }

  shinyApp(ui, server)