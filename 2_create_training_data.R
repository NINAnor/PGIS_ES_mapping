### Module 2, create training data on map

## input: -
## output: sf object poly as training objects

## functions
#load ee credentials



library(rgee)
library(googledrive)
library(mapedit)
library(geojsonio)
library(leaflet)
library(mapview)
library(sf)
library(raster)

###draw two polygons, send it to gee
map<-leaflet()%>%addProviderTiles(provider= "CartoDB.Positron")%>%setView(10.42,63.44,10)
a<-mapedit::editMap(map)
a<-as.data.frame(a$finished)
a<-st_as_sf(a)

## add a recreational value to each geom -- this needs to be done in the app by the user
a$rec_val<-c(3,3,2,2,1,1)


### ROIs

gee_poly<-rgee::sf_as_ee(a, via = "getInfo")