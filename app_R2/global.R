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
library(bigrquery)
library(DBI)

source("C:/Users/reto.spielhofer/git/PGIS_ES_mapping/modules/edit_module.R")

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



## download user confidential
user_conf <- tbl(con, "user_conf")
user_conf <- select(user_conf, userMAIL, userID) %>% filter(siteID == siteID)%>%
  distinct() %>%
  collect()


## make user pts
user_all <- tbl(con, "user_all")
user_all <- select(user_all, userLAT, userLNG, age, gen, liv)%>%
  collect()
user_pts<-user_all%>%filter(!is.na(userLAT)&!is.na(userLNG))
user_pts<-st_as_sf(user_pts, coords = c("userLNG","userLAT"))


