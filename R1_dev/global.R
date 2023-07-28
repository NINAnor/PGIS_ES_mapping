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
library(shinyjs)
library(shinyBS)

source("C:/Users/reto.spielhofer/git/PGIS_ES_mapping/modules/mapping_maxent_mod.R")
source("C:/Users/reto.spielhofer/git/PGIS_ES_mapping/modules/ahp_section_module.R")
source("C:/Users/reto.spielhofer/git/PGIS_ES_mapping/modules/ahp_es_module.R")
#gbq auth
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

plz<-sf::st_read("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/postnummeromrade_wgs.shp")
plz<-st_as_sfc(plz)
map_liv<- leaflet() %>%
  addProviderTiles(provider= "CartoDB.Positron")%>%setView(10.42,63.44,10)%>%
  addFeatures(st_sf(plz), layerId = ~seq_len(length(plz)))


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

### load ES description table from gbq
es_all<-tbl(con, "es_descr")
es_all<-select(es_all,esID,esNUM,esDESCR,esNAME,esSECTION)%>%collect()

### load user conf to check if e-mail is already there...
conf<-tbl(con, "user_conf")
conf<-select(conf,userMAIL)%>%collect()



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



