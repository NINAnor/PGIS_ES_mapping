library(shiny)
library(shinydashboard)
library(shinyjs)
library(plotly)
library(sf)
library(leaflet)
library(rgee)
library(DT)


### read data sets
user_conf<-readRDS("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/user_conf.rds")
quest<-readRDS("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/questionnaire.rds")
es_user_data<-readRDS("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/es_user_data.rds")

## make user pts
user_pts<-st_as_sf(quest, coords = c("user_lng","user_lat"))

## initialiye gee
ee_Initialize()
