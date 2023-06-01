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

source("C:/Users/reto.spielhofer/git/PGIS_ES_mapping/modules/edit_module.R")
### read data sets

main_path<-"C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base"
ahp_ind<-readRDS(paste0(main_path,"/es_ranking_ind.RDS"))
ahp_all<-readRDS(paste0(main_path,"/es_ranking_all.RDS"))
es_user<-readRDS(paste0(main_path,"/es_user_data.RDS"))
rmse<-readRDS(paste0(main_path,"/rmse.RDS"))
varimp<-readRDS(paste0(main_path,"/varImp.RDS"))
quest<-readRDS(paste0(main_path,"/questionnaire.RDS"))
es<-readRDS(paste0(main_path,"/es_description.RDS"))
user_conf<-readRDS(paste0(main_path,"/user_conf.RDS"))

geom_path<-paste0(main_path,"/poly_R1")

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
