### Module 3, compute regression on rgee

library(rgee)
library(googledrive)
library(mapedit)
library(geojsonio)
library(leaflet)
library(mapview)
library(sf)
library(raster)



## input:
#-step 2: sf object passed with sf_as_gee() (training data)

## output extrapolated rasters based on training per ES

## functions
#load ee credentials

## bounding area sørtrøndelag
bound_reg<-ee$FeatureCollection("FAO/GAUL_SIMPLIFIED_500m/2015/level1")$
  filter(ee$Filter$eq("ADM1_CODE",2257))

ee_print(bound_reg)



## lulc
lulc <- ee$Image("COPERNICUS/CORINE/V20/100m/2018")
lulc<-lulc$resample("bilinear")$reproject(crs= "EPSG:4326",scale=30)
ee_print(lulc)
lulc<-lulc$clip(bound_reg)
# vizParams <-list(bands =list("landcover"), min = 1, max = 10, palette = list("00FFFF","0000FF"))
# Map$addLayer(lulc$select("landcover"))

## dem
dem <- ee$Image('USGS/GMTED2010')$select("be75")
dem<-dem$clip(bound_reg)
ee_print(dem)
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
comb<-ee$Image$cat(lulc,dem,count_lc,slope,mean_slope,elev_mean,asp,acc_gee)
ee_print(comb)

bands <- c("landcover","be75","landcover_count","slope","slope_mean","aspect","b1")

poly_pts = comb$select(bands)$sampleRegions(
  collection= gee_poly,
  geometries = T
)

classifier <- ee$Classifier$smileRandomForest(100, NULL, 1,0.5,NULL,0)$setOutputMode("REGRESSION")$train(
  features=poly_pts,
  classProperty= "rec_val",
  inputProperties = list("landcover","be75","landcover_count","slope","slope_mean","aspect","b1")
)

regression <- comb$select("landcover","be75","landcover_count","slope","slope_mean","aspect","b1")$classify(classifier, "predicted")


Map$addLayer(regression, list(min = 1, max=3))

importance = ee$Dictionary(
  classifier$explain()$get('importance')
)

sumimp <- importance$values()$reduce(ee$Reducer$sum())

relImportance <- importance$map(function(key, val) {
  return (ee$Number(val)$multiply(100))$divide(sum)
})
# pred_single<-ee_as_raster(regression)
a<-as.data.frame(importance$getInfo())

### reduce extent
new_roi<-mapedit::editMap(map)
new_roi<-as.data.frame(new_roi$finished)
new_roi<-st_as_sf(new_roi)

new_roi<-rgee::sf_as_ee(new_roi, via = "getInfo")
new_regr<-regression$clip(new_roi)

new_roi<-new_roi$geometry()

Map$addLayer(new_regr, list(min = 1, max=3))


# Move results from Earth Engine to Drive save
task_img <- ee_image_to_drive(
  image = new_regr,
  folder = "rgee_geoprospective",
  fileFormat = "GEO_TIFF",
  region = new_roi,
  fileNamePrefix = "TRD_test_regression"
)


task_img$start()
ee_monitoring(task_img)
# save on gcs and display
out_raster<-ee_as_raster(new_regr,
                         region = new_roi,
                         dsn = "test_regr",
                         via = "gcs",
                         container = "rgee_geoprospective",
                         maxPixels = 1e+09,
                         lazy = FALSE,
                         public = TRUE,
                         add_metadata = TRUE,
                         timePrefix = TRUE,
                         quiet = FALSE)


plot(out_raster)