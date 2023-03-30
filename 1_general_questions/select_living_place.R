### general questions (e.g. select area of living)

## besides a set of questions to be answered, ech participant select their place of residence with a square of a map in order to 
# secure the privacy...



library(mapedit)
library(dplyr)
library(leaflet)
library(rgee)
library(sf)
ee_Initialize()



map <- leaflet()%>% 
  addProviderTiles(provider= "CartoDB.Positron")%>%
  setView(10.42,63.44,10)

# create regular poly grid 200x200m
bound_reg<-ee$FeatureCollection("FAO/GAUL_SIMPLIFIED_500m/2015/level2")$
  filter(ee$Filter$eq("ADM2_CODE",23463))


sf_bound <- ee_as_sf(x = bound_reg)
a<-st_make_grid(sf_bound, cellsize = c(diff(st_bbox(sf_bound)[c(1, 3)]), diff(st_bbox(sf_bound)[c(2,
                                                                             4)]))/100, offset = st_bbox(sf_bound)[1:2],  what = "polygons")
a<-st_sf(a)

sel_poly<-mapedit::selectFeatures(x=a,map=map)

## calc centroid of selected poly
cent<-st_centroid(sel_poly)

leaflet(cent)%>% 
  addProviderTiles(provider= "CartoDB.Positron")%>%
  setView(10.42,63.44,10)%>%addCircles()
