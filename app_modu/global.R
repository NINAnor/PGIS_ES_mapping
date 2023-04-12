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

source("C:/Users/reto.spielhofer/git/PGIS_ES_mapping/app_modu/questionnaire_module.R")
source("C:/Users/reto.spielhofer/git/PGIS_ES_mapping/app_modu/trainingmap_module.R")
source("C:/Users/reto.spielhofer/git/PGIS_ES_mapping/app_modu/selection_module.R")
# 
# 
# render_dt = function(data, editable = 'cell', server = TRUE, ...) {
#   renderDT(data, selection = 'none', server = server, editable = editable, ...)
# }
# 
# dat = data.frame(value = 'CHANGE ME', comments = 'ADD COMMENTS...') %>% mutate(leaf_id = 1)
# APP_CRS <- 4326
# 
# # accept list of sf data.frames with multiple geom types
# original_sf <- NULL
# 
# dat <- dat %>% mutate(leaf_id = 1:nrow(dat))
# data_copy <- sf::st_as_sf(
#   dat,
#   geometry = sf::st_sfc(lapply(seq_len(nrow(dat)),function(i){sf::st_point()}))) %>% sf::st_set_crs(APP_CRS)
# 
# map <- leaflet() %>% 
#   addProviderTiles(provider= "CartoDB.Positron")%>%setView(10.42,63.44,10)
# 
ee_Initialize(user = 'r.spielhofer@bluewin.ch')
# 
# geometry <- ee$Geometry$Rectangle(
#   coords = c(10.32, 63.40, 10.46, 63.45),
#   proj = "EPSG:4326",
#   geodesic = FALSE
# )
# 

bound_reg<-ee$FeatureCollection("FAO/GAUL_SIMPLIFIED_500m/2015/level2")$
  filter(ee$Filter$eq("ADM2_CODE",23463))


sf_bound <- ee_as_sf(x = bound_reg)


## make a grid over region
grd<-st_make_grid(sf_bound, cellsize = c(diff(st_bbox(sf_bound)[c(1, 3)]), diff(st_bbox(sf_bound)[c(2,
                                                                                                    4)]))/60, offset = st_bbox(sf_bound)[1:2],  what = "polygons")
# 
# 

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
# 
# ### es samples
es<-c("recreation in nature","provision of food (plants)","water resources")
es_ab<-c("recr","food_prov","water_stor")
es_ind<-c(1,2,3)
sel_es<-sample(es_ind,1)
sel_es_full<-es[sel_es]
sel_es_ab<-es_ab[sel_es]
# 
# 

APP_CRS <- 4326
# Need to parse out spatial objects if input data is spatial type <- c('sf', 'SpatVector') 
le = TRUE 


user_crs <- APP_CRS
zoomto = "Trondheim"
zoomto_area <- tmaptools::geocode_OSM(zoomto) 
zoomto <- sf::st_as_sfc(zoomto_area$bbox) %>% sf::st_sf() %>%
  sf::st_set_crs(APP_CRS)


dat <- data.frame(ES_value = 0) 
# dat <- data.frame(ES_value = 0, imp_access = 0, imp_naturalness = 0, imp_landcover = 0, further_comments = 'CHANGE ME',stringsAsFactors=FALSE) 

dat%>% 
  mutate(leaf_id = 1)


dat <- dat %>% mutate(leaf_id = 1:nrow(dat))
data_copy <- sf::st_as_sf(
  dat,
  geometry = 
    sf::st_sfc(lapply(seq_len(nrow(dat)),function(i){sf::st_polygon()}))
) %>% sf::st_set_crs(APP_CRS)




