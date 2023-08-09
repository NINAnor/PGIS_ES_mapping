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


####### parameters
#study site:
studyID<-"NOR-SNJ"

#Resolution ES mapping
res_on<-100
res_off<-1000

# if offshore how long should the study site edge length be [m]?
study_size_edge_length<-10000




### load study site
site<-tbl(con, "study_site")
site<-select(site, siteID, siteLNG, siteLAT, siteADM2, siteNAME, siteWIND, siteAREA, sitePOP, siteHIGH, siteLOW, siteLINK, siteECONOMY, siteECOLOGY, siteCUL)%>%filter(siteID == studyID)%>%collect()



#### number of ES part shoud see and pot. map:
num_es<-4
#######

ee_Initialize(user = 'r.spielhofer@bluewin.ch')

# test without geometry upload...???
geometry <- ee$Geometry$Rectangle(
  coords = c(10.30, 63.35, 10.50, 63.5),
  proj = "EPSG:4326",
  geodesic = FALSE
)

## according to onshore vs. offshore use different variables for MaxEnt and different study area geometries

if(site$siteWIND == "on"){
  # ADM2 code
  bound_reg<-ee$FeatureCollection("FAO/GAUL_SIMPLIFIED_500m/2015/level2")$
    filter(ee$Filter$eq("ADM2_CODE", as.integer(site$siteADM2)))
  
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
  
}else{
  ## create a rectangle of x dim around center point in DB
  p1 <- st_point(c(site$siteLNG,site$siteLAT))
  pts_cen <- st_sfc(p1, crs = 'WGS84')
  bound_reg<-st_buffer(pts_cen,study_size_edge_length/2)%>%st_bbox()
  bound_reg<-st_as_sfc(bound_reg)
  #make ee object
  bound_reg<-sf_as_ee(bound_reg)

  bath <- ee$Image("NOAA/NGDC/ETOPO1")$select("bedrock")
  bath<-bath$resample("bilinear")$reproject(crs= "EPSG:4326",scale=res_off)
  bath<-bath$clip(bound_reg)$rename("OFF_BAT")
  
  
  off_dist<-paste0(ee_get_assethome(), '/descriptor_var/OFF_DIST')
  off_dist<-ee$Image(off_dist)
  off_dist<-off_dist$resample("bilinear")$reproject(crs= "EPSG:4326",scale=res_off)$clip(bound_reg)$rename("OFF_DIST")
  
  off_nat<-paste0(ee_get_assethome(), '/descriptor_var/OFF_NAT')
  off_nat<-ee$Image(off_nat)
  off_nat<-off_nat$resample("bilinear")$reproject(crs= "EPSG:4326",scale=res_off)$clip(bound_reg)$rename("OFF_NAT")
  
  comb<-ee$Image$cat(bath,off_dist, off_nat)
  bands <- list("OFF_BAT","OFF_DIST","OFF_NAT")
  
}

sf_bound <- ee_as_sf(x = bound_reg)


## make a grid over region
# grd<-st_make_grid(sf_bound, cellsize = c(diff(st_bbox(sf_bound)[c(1, 3)]), diff(st_bbox(sf_bound)[c(2,
#                                                                                                     4)]))/60, offset = st_bbox(sf_bound)[1:2],  what = "polygons")

grd<-sf::st_read("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/postnummeromrade_wgs.shp")
grd<-st_as_sfc(plz)
map_liv<- leaflet() %>%
  addProviderTiles(provider= "CartoDB.Positron")%>%setView(10.42,63.44,10)%>%
  addFeatures(st_sf(grd), layerId = ~seq_len(length(grd)))

map_area<-leaflet(sf_bound) %>%
  addPolygons(color = "orange", weight = 3, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0)%>%
  addProviderTiles(providers$CartoDB.Positron,options = tileOptions(minZoom = 10, maxZoom = 13))


### load ES description table from gbq
es_all<-tbl(con, "es_descr")
es_all<-select(es_all,esID,esNUM,esDESCR,esNAME,esSECTION)%>%collect()

### load user conf to check if e-mail is already there...
conf<-tbl(con, "user_conf")
conf<-select(conf,userMAIL)%>%collect()


### visualization parameter for img, mean
cols   <- c("#e80909", "#fc8803", "#d8e03f", "#c4f25a","#81ab1f")
maxentviz = list(bands= 'probability',min= 0, max= 1, palette= cols)



