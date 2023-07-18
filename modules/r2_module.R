### edit module v2

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(plotly)
library(sf)
library(leaflet)
library(rgee)
library(DT)
library(mapedit)
library(leaflet.extras2)
library(leaflet.extras)
library(bigrquery)
library(DBI)
library(dplyr)

ee_Initialize()
bq_auth(path = "C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/rgee-381312-85272383f82d.json")
con <- dbConnect(
  bigrquery::bigquery(),
  project = "rgee-381312",
  dataset = "data_base",
  billing = "rgee-381312"
)

userID <-"AcfePm2lep"
esID2<-"aes"
siteID<-"NOR-SNJ"


geometry <- ee$Geometry$Rectangle(
  coords = c(10.30, 63.35, 10.50, 63.5),
  proj = "EPSG:4326",
  geodesic = FALSE
)

## here modify code according to siteID!
bound_reg<-ee$FeatureCollection("FAO/GAUL_SIMPLIFIED_500m/2015/level2")$
  filter(ee$Filter$eq("ADM2_CODE",23463))


sf_bound <- ee_as_sf(x = bound_reg)

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

# mapping
### vis parameter for img, mean
labels <- c("low", "moderate", "intermediate", "high","very high")
cols   <- c("#e80909", "#fc8803", "#d8e03f", "#c4f25a","#81ab1f")
vis_qc <- list(min = 1, max = 5, palette = cols, values = labels)


cols2   <- c("red", "red")
vis_diff<- list(min = -4, max = 4, palette = cols2)

cols3   <- c("orange", "yellow","#81ab1f")
vis_all<- list(min = 3, max = 5, palette = cols3)


### download es_descr table from bq once
es_descr <- tbl(con, "es_descr")
es_descr <- select(es_descr, esID, esNAME, esDESCR) %>% collect()

### download blog with not null blog and siteID given pass this to mapping module R2 (need only blog col and esID col)
blog_data <- tbl(con, "es_mappingR1")
blog_data <- select(blog_data, esID, blog) %>%filter(siteID == siteID & !is.na(blog))%>% collect()

### download ahp_all and ahp_ind


r2ui<- function(id, label = "r2") {
  ns <- NS(id)
  tagList(
    mainPanel(
      textOutput(ns("es_title")),
      br(),
      textOutput(ns("es_description")),
      br(),
      column(3,
        h4("How important is this ES?"),
        plotlyOutput(ns("imp_all"))
      ),
      column(6,
        ## mapping
        # editModUI(ns("map_edit")),
        leafletOutput(ns("map_ini")),
        selectizeInput(ns("map_change"),label="do you want to change your polygons?",choices = c("Yes","No"),options = list(
          placeholder = 'Please select an option below',
          onInitialize = I('function() { this.setValue(""); }')
        )),
        conditionalPanel(condition = "input.map_change == 'Yes'", ns = ns ,
          editModUI(ns("map_sel")),
          actionButton(ns("sub_poly"),"store changes"),
          leafletOutput(ns("map_res")),
          uiOutput(ns("slider")),
          actionButton(ns("submit"),"save values")          
                         ),

      ),
      column(3,
        # blog
        h2("test3"),
        DTOutput(ns("blog"))
        
      ),
      br()
    )
    
  )
  
  
}


r2server<-function(id, esID, userID, siteID, vis_qc, vis_diff, vis_all, es_descr, blog_data, ahp_all, comb, bands, order, geometry, sf_bound){
  moduleServer(
    id,
    function(input, output, session){
      ns<-session$ns
     
      # filter es_all with current esID
      cur_es<-es_descr%>%filter(esID %in% esID)
      
      output$es_title <- renderText({
        cur_es$esNAME
      }
      )
      
      output$es_description <- renderText({
        cur_es$esDESCR
      }
      )
      
      #blog

      blog_data<-blog_data%>%filter(esID == esID)%>%select(blog)
      #create fake user
      blog_data$username<-rep("USER_",nrow(blog_data))
      blog_data$username<-paste0(blog_data$username,as.numeric(rownames(blog_data)), ": ",blog_data$blog)
      blog_data<-blog_data%>%select(username)
      output$blog<-renderDT(blog_data,rownames= FALSE, colnames="Blog entries")
        
      # get poly of user and esID from R1
      ee_asset_path<-paste0(ee_get_assethome(), '/train_polys_1/',userID,"_",esID,"_",siteID)
      ee_poly<-ee$FeatureCollection(ee_asset_path)
      poly_r1 <- ee_as_sf(ee_poly)
      
      #centroid for later mapping
      cent_poly <- st_centroid(poly_r1)
        
      ##get the individual raster
      imgpath1<-paste0(ee_get_assethome(), '/R_1/ind_maps/',"1_",userID,"_",esID,"_",siteID)
      img_ind1<-ee$Image(imgpath1)
        
      ## get the postprocessed consensus raster 
      allpath<-paste0(ee_get_assethome(), '/R_1/all_part/',"1_",esID,"_",siteID)
      all<-ee$Image(allpath)
        
      ##get the leave-one-out raster of participant
      loo<-paste0(ee_get_assethome(), '/R_1/leave_one_out/',"1_",userID,"_",esID,"_",siteID)
      loo<-ee$Image(loo)
        
      diff<-loo$select('predicted')$subtract(all$select('predicted'))
      diff = diff$updateMask(diff$eq(0))
        
      ## filter to just show areas of high values
      all = all$updateMask(all$gte(3))
        
      Map$setCenter(10.38649, 63.40271,10)
      m1<-Map$addLayer(
          eeObject = diff,
          vis_diff,
          opacity = 0.5, 'Your contribution map'
      ) |Map$addLayer(
        eeObject = img_ind1,
        vis_qc,
        opacity = 0.5, 'Your individual map'
      ) |Map$addLayer(
        eeObject = all,
        vis_all,
        opacity = 0.5, 'The map from all participants'
      )
          
      # +Map$addLegend(vis_qc,name = "prediction", color_mapping = "character")  
        
      # create and render the initial map for the user  
      output$map_ini<-renderLeaflet({
          leaflet(poly_r1)%>%
            addPolygons(color = "blue", weight = 3, smoothFactor = 0.5,
                        opacity = 1.0, fillOpacity = 0)%>%
            addLabelOnlyMarkers(data = cent_poly,
                                lng = ~st_coordinates(cent_poly)[,1], lat = ~st_coordinates(cent_poly)[,2], label = cent_poly$es_value,
                                labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE,
                                                            style = list(
                                                              "color" = "red",
                                                              "font-family" = "serif",
                                                              "font-style" = "bold",
                                                              "font-size" = "20px"
                                                            ))) %>%
            addProviderTiles(providers$CartoDB.Positron,options = tileOptions(minZoom = 10, maxZoom = 15))%>%
            leaflet.extras::addDrawToolbar(targetGroup='editable',
                                           polylineOptions = F,
                                           polygonOptions = F,
                                           circleOptions = F,
                                           markerOptions = F,
                                           circleMarkerOptions = F,
                                           rectangleOptions = F,
                                           
                                           singleFeature = FALSE) + m1
          
        })
      
      # create the map with the polys from R1 of participant  
      map<-leaflet(poly_r1)%>%
          addPolygons(color = "blue", weight = 3, smoothFactor = 0.5,
                      opacity = 1.0, fillOpacity = 0)%>%
          addLabelOnlyMarkers(data = cent_poly,
                              lng = ~st_coordinates(cent_poly)[,1], lat = ~st_coordinates(cent_poly)[,2], label = cent_poly$es_value,
                              labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE,
                                                          style = list(
                                                            "color" = "red",
                                                            "font-family" = "serif",
                                                            "font-style" = "bold",
                                                            "font-size" = "20px"
                                                          ))) %>%
          addProviderTiles(providers$CartoDB.Positron,options = tileOptions(minZoom = 10, maxZoom = 15))%>%
          leaflet.extras::addDrawToolbar(targetGroup='editable',
                                         polylineOptions = F,
                                         polygonOptions = F,
                                         circleOptions = F,
                                         markerOptions = F,
                                         circleMarkerOptions = F,
                                         rectangleOptions = T,
                                         
                                         singleFeature = FALSE) + m1
        
      #create result map
      map_res<-leaflet()%>%
          addProviderTiles(providers$CartoDB.Positron,options = tileOptions(minZoom = 10, maxZoom = 15))%>%
          addDrawToolbar(targetGroup='drawPoly',
                         polylineOptions = F,
                         polygonOptions = F,
                         circleOptions = F,
                         markerOptions = F,
                         circleMarkerOptions = F,
                         rectangleOptions = F,
                         singleFeature = FALSE,
                         editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))+m1
        
      
      edits<-callModule(
          module = editMod,
          leafmap = map,
          id = "map_sel")
      
      #edits<-mapedit::editMap(map)
        
      
      ### as soon as sub_poly button pressed...
      
      tbl_out<-eventReactive(input$sub_poly,{
          tbl<-edits()$finished
          tbl<-tbl%>%st_drop_geometry()
          tbl$value_es<-rep(NA,(nrow(tbl)))
          tbl
        })
        
      observeEvent(input$sub_poly, {
         tbl<-tbl_out()
         polygon<-edits()$all
          
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
                                                                                          )))
          )
          
        })
        
      #render slider to re-evaluate 
        observeEvent(input$sub_poly,{
          tbl<-tbl_out()

          output$slider <- shiny::renderUI({
            ns <- session$ns
            tagList(
              h5("please rate ESx for each area"),
              lapply(1:nrow(tbl),function(n){
                polynr <- tbl[n,]$`_leaflet_id`
                id<-paste0("id_",polynr)
                lable<-paste0("Polygon Nr: ",polynr)
                sliderInput(ns(id),lable, min = 1, max = 5, step = 1, value = 3)
                
              })
            )
            
          })
        })
        

        ### same procedure as R 1 but no need to show resulting map of R2
        
      observeEvent(input$submit,{
          withProgress(message = "save your drawings",value = 0.1,{
            polygon<-edits()$finished
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
            polygon$delphi_round<-rep(2, nrow(polygon))
            polygon$drawing_order<-rep(NA,nrow(polygon))
            
            n_polys <-nrow(polygon)
            polygon<-st_as_sf(polygon)
            poly_area<-as.numeric(sum(st_area(polygon)))
            
            # make ee object and save
            gee_poly<-rgee::sf_as_ee(polygon, via = "getInfo")
            #set features
            gee_poly <- gee_poly$set('es_id', esID,
                                     'userID', userID,
                                     'siteID', siteID,
                                     'order_id', order,
                                     'delphi_round', 2)
            
            # #save poly
            poly_load<-ee_table_to_asset(gee_poly,
                                         description = "upload poly",
                                         assetId = paste0(ee_get_assethome(),"/train_polys_2/",userID,"_",esID,"_",siteID),
                                         overwrite = T
            )
            poly_load$start()
            
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
                area = as.integer(poly_area),
                n_poly = as.integer(n_polys),
                blog = input$blog,
                mapping_order = as.integer(order),
                extrap_RMSE = 0,
                extrap_accIMP = 0,
                extrap_lulcIMP = 0,
                extrap_natIMP = 0
                
              )
            train_param<-as.data.frame(train_param)
            
            ############ upload R2 mapping table
            incProgress(amount = 0.1,message = "update data base")
            # write to bq
            insert_upload_job("rgee-381312", "data_base", "es_mappingR2", train_param)
            
            prediction<-imageClassified$select("probability")
            
            ############ save map
            incProgress(amount = 0.2,message = "save geodata")
            img_assetid <- paste0(ee_get_assethome(), '/R_2/ind_maps/',"2_",userID,"_",esID,"_", siteID)
            
            #set features of img
            prediction <- prediction$set('es_id', esID,
                                         'userID', userID,
                                         'siteID', siteID,
                                         'order_id', order,
                                         'delphi_round', 2)
            
            start_time<-Sys.time()
            task_img <- ee_image_to_asset(
              image = prediction,
              assetId = img_assetid,
              overwrite = T,
              region = geometry
            )
            
            task_img$start()
            incProgress(amount = 0.2,message = "map successfully uploaded")
            
            
          })

        })
        
 
        #ranking of esID compared to all other ES

        imp_all<-plot_ly(ahp_all, x = ~es, y=~pref_all, color = ~es == esID, type = 'bar') %>% 
          layout(xaxis = list(categoryorder = "total ascending"))
        output$imp_all<-renderPlotly(imp_all)
        
        

        
      
    }#close server function
  )#module server
}#server close      
    
  



ui <- fluidPage(
  fluidRow(
    column(width = 12,
           r2ui("es", "Counter #1")
           )
  )


)

server <- function(input, output, session) {


  r2server("es", esID, userID, siteID, vis_qc, vis_diff, vis_all, es_descr, blog_data, ahp_all, comb, bands, 1, geometry, sf_bound)

}

shinyApp(ui, server)
