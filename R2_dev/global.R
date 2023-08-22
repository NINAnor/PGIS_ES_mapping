library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(plotly)
library(sf)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(rgee)
library(DT)
library(mapedit)
library(tidyverse)
library(bigrquery)
library(DBI)
library(shinyWidgets)

## if participant decides to map although he/she has not mapped in R1:
source("C:/Users/reto.spielhofer/git/PGIS_ES_mapping/modules/mapping_maxent_mod.R")

source("C:/Users/reto.spielhofer/git/PGIS_ES_mapping/modules/edit_moduleV2.R")

ee_Initialize()
bq_auth(path = "C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/rgee-381312-85272383f82d.json")
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

if(site$siteWIND == "on"){
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
  
  off_lulc<-paste0(ee_get_assethome(), '/descriptor_var/OFF_LULC')
  off_lulc<-ee$Image(off_lulc)
  off_lulc<-off_lulc$resample("bilinear")$reproject(crs= "EPSG:4326",scale=res_off)$clip(bound_reg)$rename("OFF_LULC")
  
  comb<-ee$Image$cat(bath,off_dist, off_nat, off_lulc)
  bands <- list("OFF_BAT","OFF_DIST","OFF_NAT", "OFF_LULC")
  
  
  
}

sf_bound <- ee_as_sf(x = bound_reg)


geometry <- ee$Geometry$Rectangle(
  coords = c(10.30, 63.35, 10.50, 63.5),
  proj = "EPSG:4326",
  geodesic = FALSE
)


# # mapping
# ### vis parameter for img, mean
labels <- c("low", "moderate", "intermediate", "high","very high")
cols   <- c("#e80909", "#fc8803", "#d8e03f", "#c4f25a","#81ab1f")
vis_ind <- list(min = 0, max = 1, palette = cols, values = labels)
# 
# 
# cols2   <- c("red", "red")
# vis_diff<- list(min = -4, max = 4, palette = cols2)
# 
# cols3   <- c("orange", "yellow","#81ab1f")
# vis_all<- list(min = 3, max = 5, palette = cols3)

## download user confidential
user_conf <- tbl(con, "user_conf")
user_conf <- select(user_conf, userMAIL, userID) %>% 
  distinct() %>%
  collect()


### download es_descr table from bq once
es_descr <- tbl(con, "es_descr")
es_descr <- select(es_descr, esID, esNAME, esDESCR) %>% collect()



### download general statistics for site ID

user_all <- tbl(con, "user_all")
user_all <- select(user_all, userLAT, userLNG, siteID, edu, fam, liv, gen, age)%>%filter(siteID == studyID)%>%
  collect()
### map user location
user_pts<-user_all%>%filter(!is.na(userLAT)&!is.na(userLNG))
user_pts<-st_as_sf(user_pts, coords = c("userLNG","userLAT"))

userES <- tbl(con, "es_mappingR1")
userES <- select(userES, userID, esID, mapping, siteID, blog) %>% filter(siteID == studyID)%>%
  collect()



