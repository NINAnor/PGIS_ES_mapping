# call module gee extrapolation

geeUI = function(id) {
  ns <- NS(id)
  
  tagList(
    mainPanel(
      leafletOutput(outputId = ns("gee_map"))
      # %>% withSpinner(color="#0dc5c1")
    )
  )
}

gee_Server = function(input, output, session, train_poly, sel_es_ab) {
  gee_poly<-train_poly()
  
  lulc <- ee$Image("COPERNICUS/CORINE/V20/100m/2018")
  lulc<-lulc$resample("bilinear")$reproject(crs= "EPSG:4326",scale=30)
  lulc<-lulc$clip(bound_reg)
  
  dem <- ee$Image('USGS/GMTED2010')$select("be75")
  dem<-dem$clip(bound_reg)
  Map$addLayer(dem)
  
  
  ## besides single value at point loc from 3 raster, calc zonal stats with moving window for each pts test:
  count_lc<-lulc$select("landcover")$reduceNeighborhood(
    reducer = ee$Reducer$countDistinct(),
    kernel = ee$Kernel$circle(3)
  )
  
  #mean elev
  elev_mean<-dem$select("be75")$reduceNeighborhood(
    reducer = ee$Reducer$mean(),
    kernel = ee$Kernel$circle(3)
  )
  
  # slope
  slope <- ee$Terrain$slope(dem)
  
  #mean slope
  mean_slope<-slope$select("slope")$reduceNeighborhood(
    reducer = ee$Reducer$mean(),
    kernel = ee$Kernel$circle(3)
  )
  
  #aspect
  asp<-ee$Terrain$aspect(dem)
  
  # combine unique class count wdw and lulc
  comb<-ee$Image$cat(lulc,dem,count_lc,slope,mean_slope,elev_mean,asp)
  
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
  
  # ## save in stack
  # assetid <- paste0(ee_get_assethome(), '/rgee/individual_R1_',sel_es_ab,"/","USER_abc")
  # task_img <- ee_image_to_asset(
  #   image = regression,
  #   assetId = assetid,
  #   overwrite = T,
  #   region = geometry
  # )
  # 
  # task_img$start()
  
  ## map
  map<-Map$setCenter(10.38649, 63.40271,10)
  Map$addLayer(
    eeObject = prediction,
    list(min = 1, max=3),
    name = "prediction ee"
  )+Map$addLayer(gee_poly, list(color = "red"), "colored")
  
  output$gee_map <- renderLeaflet({
    map()
  })
  
}

