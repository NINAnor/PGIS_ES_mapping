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


### read data sets
user_conf<-readRDS("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/user_conf.rds")
quest<-readRDS("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/questionnaire.rds")
es_user_data<-readRDS("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/es_user_data.rds")

## make user pts
user_pts<-quest%>%filter(!is.na(user_lng)&!is.na(user_lat))
user_pts<-st_as_sf(user_pts, coords = c("user_lng","user_lat"))

## initialiye gee
ee_Initialize()

### vis parameter for img, mean
labels <- c("low", "moderate", "intermediate", "high","very high")
cols   <- c("#e80909", "#fc8803", "#d8e03f", "#c4f25a","#81ab1f")
vis_qc <- list(min = 1, max = 5, palette = cols, values = labels)

### vis param for diff map
labels_diff <- c("low", "moderate", "intermediate", "high","very high")
cols_diff<-c("#81ab1f","#c4f25a", "#d8e03f", "#fc8803","#e80909")
vis_diff <- list(min = 0, max = -4, palette = cols_diff,  values = labels_diff)