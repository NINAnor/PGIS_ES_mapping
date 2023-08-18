## mapping module maxent
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


img_path<-"C:/Users/reto.spielhofer/git/PGIS_ES_mapping/R1_dev/data/images"
bq_auth(path = "C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/rgee-381312-85272383f82d.json")
# connection to bq
con <- dbConnect(
  bigrquery::bigquery(),
  project = "rgee-381312",
  dataset = "data_base",
  billing = "rgee-381312"
)


### study site:
siteID<-"NOR-SNJ"

#### number of ES part shoud see and pot. map:
num_es<-4
UID_part<-stri_rand_strings(1, 10, pattern = "[A-Za-z0-9]")
userID<-as.character(UID_part)
#######

ee_Initialize(user = 'r.spielhofer@bluewin.ch')

#this needs to be modified dep. on siteID! 
geometry <- ee$Geometry$Rectangle(
  coords = c(10.30, 63.35, 10.50, 63.5),
  proj = "EPSG:4326",
  geodesic = FALSE
)

## here modify code according to siteID!
bound_reg<-ee$FeatureCollection("FAO/GAUL_SIMPLIFIED_500m/2015/level2")$
  filter(ee$Filter$eq("ADM2_CODE",23463))


sf_bound <- ee_as_sf(x = bound_reg)


## make a grid over region
# grd<-st_make_grid(sf_bound, cellsize = c(diff(st_bbox(sf_bound)[c(1, 3)]), diff(st_bbox(sf_bound)[c(2,
#                                                                                                     4)]))/60, offset = st_bbox(sf_bound)[1:2],  what = "polygons")
lulc <- ee$Image("COPERNICUS/CORINE/V20/100m/2018")
lulc<-lulc$resample("bilinear")$reproject(crs= "EPSG:4326",scale=100)
lulc<-lulc$clip(bound_reg)


acc_pat<-paste0(ee_get_assethome(), '/acc')
acc<-ee$Image(acc_pat)
acc<-acc$resample("bilinear")$reproject(crs= "EPSG:4326",scale=100)

nat_pat<-paste0(ee_get_assethome(), '/natu')
nat<-ee$Image(nat_pat)
nat<-nat$clip(bound_reg)
nat<-nat$resample("bilinear")$reproject(crs= "EPSG:4326",scale=100)
nat<-nat$rename("nat")

# combine unique class count wdw and lulc
comb<-ee$Image$cat(lulc,acc, nat)
bands <- list("landcover","b1","nat")
# 
# ### es samples


### load ES description table from gbq
#es_all<-readRDS("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/es_description.rds")
es_all<-tbl(con, "es_descr")
# es_all<-select(es_all,esID,esNUM,esDESCR,esNAME,esSECTION)%>%filter(esID == "aes"| esID == "cult" | esID == "recr")%>%collect()
es_all<-select(es_all,esID,esNUM,esDESCR,esNAME,esSECTION)%>%collect()



APP_CRS <- 4326
# Need to parse out spatial objects if input data is spatial type <- c('sf', 'SpatVector') 
le = TRUE 

## set zoom to
user_crs <- APP_CRS
zoomto = "Trondheim"
zoomto_area <- tmaptools::geocode_OSM(zoomto) 
zoomto <- sf::st_as_sfc(zoomto_area$bbox) %>% sf::st_sf() %>%
  sf::st_set_crs(APP_CRS)

# create empty df for storing ES values while mapping
dat <- data.frame(ES_value = 0) 

dat%>% 
  mutate(leaf_id = 1)


dat <- dat %>% mutate(leaf_id = 1:nrow(dat))
data_copy <- sf::st_as_sf(
  dat,
  geometry = 
    sf::st_sfc(lapply(seq_len(nrow(dat)),function(i){sf::st_polygon()}))
) %>% sf::st_set_crs(APP_CRS)


### visualization parameter for img, mean
cols   <- c("#e80909", "#fc8803", "#d8e03f", "#c4f25a","#81ab1f")
maxentviz = list(bands= 'probability',min= 0, max= 1, palette= cols)



############ UI of maxent mapping module
mapselectUI<- function(id, label = "selector") {
  ns <- NS(id)
  tagList(
    br(),
    mainPanel(
        # title 
    uiOutput(ns("title_es")),
    br(),
    fluidRow(
      column(5,
             textOutput(ns("descr_es"))),
      column(2),
      column(5,
             uiOutput(ns("image_es")))
    ),
    br(),
    # questions of importance
    uiOutput(ns("imp_text")),
    sliderInput(ns("imp_own"), "... for you personally in this area?",
                min = 0, max = 5, value = 3),
    
    sliderInput(ns("imp_other"), "... for others and the society in this area?",
                min = 0, max = 5, value = 3),

    br(),
    # are you able to map the ES?
    uiOutput(ns("map_poss")),
    br(),
    # conditionalPanel(
    #   condition = "input.map_poss == 'Yes'", ns = ns ,
    #   # h5("where do you find good spots of ESx?"),
    #   uiOutput(ns("es_quest_where")),
    #   br(),
    #   editModUI(ns("map_sel")),
    #   br(),
    #   # initial button to save the map
    #   # actionButton(ns("check_poly"),"check polygons")
    #   
    #   
    # ),
    # if ES not mappable
    conditionalPanel(
      condition = "input.map_poss == 'No'", ns = ns ,
      selectizeInput(ns("expert_map"),label="Would you trust an expert map",choices = c("Yes","No"),options = list(
        placeholder = 'Please select an option below',
        onInitialize = I('function() { this.setValue(""); }')))
      # actionButton(ns("submit2"),"save")
      
    ),

    conditionalPanel(
      condition = "input.expert_map != '' || input.submit > 0", ns=ns,
      actionButton(ns("confirm"), "Next task", class='btn-primary')
    )
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
      output$title_es<-renderUI(h5(rand_es_sel[order,]$esNAME))
      output$descr_es<-renderText(rand_es_sel[order,]$esDESCR)
      # output$varimp_text<-renderText(paste0("How important are the following aspects to get a benefit of ",rand_es_sel[order,]$esNAME))
      output$res_text<-renderUI(h6(paste0("Your personal map of ",rand_es_sel[order,]$esNAME)))
      output$es_quest_where<-renderUI(h6(paste0("Where do you find good areas for ", rand_es_sel[order,]$esNAME,"?")))
      output$es_quest_how<-renderUI(h6(paste0("How do you rate the quality of ",rand_es_sel[order,]$esNAME, " for each of area")))
      output$imp_text<-renderUI(h6(paste0("How important is ", rand_es_sel[order,]$esNAME,"...")))
                                
      
      output$image_es<-renderUI({
        tags$figure(
          class = "centerFigure",
          tags$img(
            src = paste0(esID,".jpg"),
            width = 600,
            alt = "Picture of an astragalus (bone die)"
          ),
          tags$figcaption("Image of Astragalus by Yaan, 2007")
        )
      })
      
      
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
      # edits<-callModule(
      #   module = editMod,
      #   leafmap = map,
      #   id = "map_sel")
      
      rv<-reactiveValues(
        edits = reactive({})
     )
      # edits<-mapedit::editMap(map)

      ## call the edit map module from the mapedit package
     observeEvent(input$map_poss,{
      if(input$map_poss == "Yes"){

          rv$edits<-callModule(
            module = editMod,
            leafmap = map,
            id = "map_sel")
          # print(rv$edits())
        
          insertUI(selector =paste0("#",ns("map_poss")),
                   where = "afterEnd",
                   ui=tagList(
                    uiOutput(ns("es_quest_where")),
                     br(),
                     editModUI(ns("map_sel")),
                    
                    htmlOutput(ns("overlay_result")),
                    uiOutput(ns("btn1")),
                     
                   )
                   )
          
            }#/if yes
        })#/map_poss 
      
      
      
      ## remove mapping question as soon as decided
     observe({
       req(rv$edits)
       rectangles <- rv$edits()$finished
       n_poly<-nrow(as.data.frame(rectangles))
       
       if(n_poly==1){
         n_within<-nrow(as.data.frame(st_within(rectangles,sf_bound)))
         if(n_within<n_poly){
           output$overlay_result <- renderText({
             paste("<font color=\"#FF0000\"><b>","You can`t save the polygons:","</b> <li>Place your polygon completely into the the study area<li/></font>")
           })
           removeUI(
             "#save"
           )
         }else{
           output$btn1<-renderUI(
             actionButton("save","save")
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
             "#save"
           )
           output$overlay_result <- renderText({
             paste("<font color=\"#FF0000\"><b>","You can`t save the polygons:","</b><li>Place your polygon completely into the the study area<li/><li>Remove overlays<li/></font>")
             
           })
         }else if(q==0 & n_within<n_poly){
           removeUI(
             "#save"
           )
           output$overlay_result <- renderText({
             paste("<font color=\"#FF0000\"><b>","You can`t save the polygons:","</b> <li>Place your polygon completely into the the study area<li/></font>")
             
           })
         }else if(q!=0 & n_within==n_poly){
           removeUI(
             "#save"
           )
           output$overlay_result <- renderText({
             paste("<font color=\"#FF0000\"><b>","You can`t save the polygons:","</b> <li>Remove overlays<li/></font>")
             
           })
         }else if(q==0 & n_within==n_poly){
           output$btn1<-renderUI(
             actionButton("save","save")
           )
           output$overlay_result <- renderText({
             "Save or draw further polygons"
           })
         }
       }
       
     })
     
    
      
      observeEvent(input$map_poss,{
        if(input$map_poss !=""){
          removeUI(
            selector = paste0("#",ns("map_poss"))
          )
          removeUI(
            selector = paste0("#",ns("imp_own"),"-label"))
          removeUI(
            selector = paste0("#",ns("imp_own")))
          removeUI(
            selector = paste0("#",ns("imp_other"),"-label"))
          removeUI(
            selector = paste0("#",ns("imp_other")))
          removeUI(
            selector = paste0("#",ns("imp_text")))
        }
      })
      
      ### confirm the drawings and render the results table
      tbl_out<-eventReactive(input$savepoly,{
        tbl<-rv$edits()$finished
        
        # do not give possibility to submit map without polygons
        req(tbl, cancelOutput = FALSE)
        tbl<-tbl%>%st_drop_geometry()
        tbl$value_es<-rep(NA,(nrow(tbl)))
        tbl
      })
      
      ### confirm the drawings and render the leaflet map
      observeEvent(input$savepoly, {
        tbl<-tbl_out()
        polygon<-rv$edits()$finished
        
        # do not give possibility to submit map without polygons
        req(polygon, cancelOutput = FALSE)
        
        ## render new UI of polygon map and slider remove rest
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
            # a short expl. why this sites
            uiOutput(ns("blogdescr")),
            textInput(ns("blog"), label = ""),
            br(),
            conditionalPanel(
              condition = "input.blog != ''", ns=ns,
              actionButton(ns("submit"),"save values")
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
        
        ### create a slider for each of the polygons

      output$slider <- shiny::renderUI({
          ns <- session$ns
          tagList(
            paste0("The Nr. of the slider refer to the number of the rectangle in the map"),
            br(),
            br(),
            lapply(1:nrow(tbl),function(n){
              polynr <- tbl[n,]$`_leaflet_id`
              id<-paste0("id_",polynr)
              lable<-paste0("Polygon Nr in map: ",polynr)
              sliderInput(ns(id),lable, min = 1, max = 5, step = 1, value = 3)
            })
          )
          
        })
      })
      output$blogdescr<-renderUI({
        h6(paste0("Please provide us a short explanation why you choosed these areas of good quality to provide ",rand_es_sel[order,]$esNAME))
      })

      ## remove map UI and sliders show result
      observeEvent(input$submit, {
        
        insertUI(
          selector = paste0("#",ns("submit")),
          where = "afterEnd",
          ui = tagList(
            textOutput(ns("res_text")),
            br(),
            leafletOutput(ns("gee_map")),

          )
        )
        
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
          selector = paste0("#",ns("blogdescr"))
        )
        removeUI(
          selector = paste0("#",ns("blog"))
        )
        removeUI(
          selector = paste0("#",ns("submit"))
        )
        
      })
      
      
      ### predict probability of ES with maxent, save prediction, save poly and save drawing summaries on gee/bq

      ### gather poly
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
          # varImp$esID<-rep(esID,nrow(varImp))
          # varImp$siteID<-rep(siteID,nrow(varImp))
          # varImp$userID<-rep(userID,nrow(varImp))
          # varImp$spatial_delphi_round<-rep(as.integer(1),nrow(varImp))
          # varImp<-varImp%>%filter(name !="__unused__")
          # colnames(varImp)<-c("var_name","imp_val","esID","siteID","userID","spatial_delphi_round")
          # insert_upload_job("rgee-381312", "data_base", "var_imp", varImp)
          
          
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
              # imp_acc= as.integer(input$access),
              imp_acc= as.integer(0),
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
        return(prediction)
        
      })
      
     

      output$gee_map <- renderLeaflet({
        prediction()
             })
      # outputOptions(output, "gee_map", suspendWhenHidden = FALSE)
      

      ### store infos if mapping is not possible
      observeEvent(input$expert_map,{
        if(input$expert_map !=""){
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
          # insert_upload_job("rgee-381312", "data_base", "es_mappingR1", train_param)
          # removeUI(
          #   selector = paste0("#",ns("expert_map"))
          # )
        }


      })
      cond <- reactive({input$confirm})
      
      return(cond) 
    }
  )

}


### test
# # 
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "cerulean"),
                titlePanel( title =  div(img(src="wendy_logo.png", width ='90'), 'POC mapping ecosystem services'), windowTitle = "ES mapping" ),
                tabsetPanel(id = "inTabset",
                          tabPanel(title = "Your Task", value = "p1",
                          h5("This explains what you are asked to do in the following task"),
                          img(src="tutorial_rating.gif", align = "left",height='620px',width='836px'),
                          br(),
                          checkboxInput('check', 'I have read and understood the instructions',value = FALSE),
                          actionButton("test","hit me")
                ),
                tabPanel(title = "Mapping Task", value = "p2",
                  mapselectUI("map1", "mapping1")
           ))
)

server <- function(input, output, session) {
  
  rv<-reactiveValues(
    v = reactive({})
  )
  
  
  userID<-eventReactive(input$test, {
    # create random large string
    UID_part<-stri_rand_strings(1, 10, pattern = "[A-Za-z0-9]")
    userID<-as.character(UID_part)
    return(userID)
  })

    rand_es_sel<-es_all%>%slice_sample(n=1, replace = F)
 
  
  #
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
        showTab(inputId = "inTabset", target = "p2")
    rv$v<-mapselectServer("map1",sf_bound, comb,bands, rand_es_sel, 1, isolate(userID()),"ABC", geometry, maxentviz)
  })


   # v<- mapselectServer("map1",sf_bound, comb,bands, rand_es_sel, 1, isolate(userID()),"ABC", geometry, maxentviz)


  
  observeEvent(rv$v(),{

            updateTabsetPanel(session, "inTabset",
                        selected = "p1")
      hideTab(inputId = "inTabset",
              target = "p2")
      showTab(inputId = "inTabset", target = "p1")

  })
  
}

shinyApp(ui, server)
