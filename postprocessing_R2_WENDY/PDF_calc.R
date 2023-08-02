## compute IAR / PDF per WT
library(rgee)
library(sf)
library(raster)
library(mapedit)
library(leaflet)
library(leaflet.extras)
library(terra)
library(spdep)
library(tmap)
library(dplyr)
library(ggplot2)

## load ee con
ee_Initialize()

## single wind turbine (=wt) or wind farm (=wf)

pdf_meth<-"wt"
case_study<-"NOR-SNJ"
main_path<-"C:/Users/reto.spielhofer/git/PGIS_ES_mapping/7_PDF"
in_path<-paste0(main_path,"/from_gee")
out_path<-paste0(main_path,"/output")

geometry <- ee$Geometry$Rectangle(
  coords = c(10.30, 63.35, 10.50, 63.5),
  proj = "EPSG:4326",
  geodesic = FALSE
)

sf_bound <- ee_as_sf(x = geometry)


# load IAR pixel raster
# IAR<-rast(paste0(in_path,"/IAR.tif"))
IAR<-raster(paste0(in_path,"/IAR.tif"))
# 
# IAR<-project(IAR,"EPSG:27391")
# plot(IAR)
IAR_poly<-rasterToPolygons(IAR)
IAR_poly<-st_as_sf(IAR_poly)
#res(IAR)



## if wt - select a single spot on map to calc pdf, 
# if wf draw a polygon on map to clac pdf.

if(pdf_meth == "wt"){
  map<-leaflet(sf_bound)%>%
    addPolygons(color = "orange", weight = 3, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0)%>%
    addProviderTiles(providers$CartoDB.Positron,options = tileOptions(minZoom = 10, maxZoom = 14))%>%
    addDrawToolbar(targetGroup='drawPoly',
                   polylineOptions = F,
                   polygonOptions = F,
                   circleOptions = F,
                   markerOptions = T,
                   circleMarkerOptions = F,
                   rectangleOptions = F,
                   singleFeature = TRUE,
                   editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))
  
}else{
  map<-leaflet(sf_bound)%>%
    addPolygons(color = "orange", weight = 3, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0)%>%
    addProviderTiles(providers$CartoDB.Positron,options = tileOptions(minZoom = 10, maxZoom = 14))%>%
    addDrawToolbar(targetGroup='drawPoly',
                   polylineOptions = F,
                   polygonOptions = T,
                   circleOptions = F,
                   markerOptions = F,
                   circleMarkerOptions = F,
                   rectangleOptions = F,
                   singleFeature = TRUE,
                   editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))
  
}

edits<-mapedit::editMap(map)
site<-edits$drawn[1,] 
site<-st_transform(site,crs(IAR_poly))

z_list<-list()
dist_vec<-c(1,100,1000,1500,2000,3000,10000)
for(a in 1:nrow(IAR_poly)){
  print(a)
  iar_list<-list()
  area_list<-list()
  tmp_center<-IAR_poly[a,]
  for(m in 1: length(dist_vec)){
  
  # tmp_poly<-st_join(IAR_poly, site, st_nn, k = 1,maxdist = dist_vec[m])
  tmp_poly<-st_join(IAR_poly, tmp_center, st_is_within_distance, dist = dist_vec[m])
  tmp_poly<-tmp_poly%>%filter(!is.na(constant.y))
  #plot(tmp_poly)
  area_list[m]<-sum(st_area(tmp_poly))
  iar_list[m]<-mean(tmp_poly$constant.x,na.rm=T)

}
  IAR<-as.data.frame(cbind(unlist(area_list),unlist(iar_list)))
  colnames(IAR)<-c("area","I")
  IAR$logA<-log(IAR$area)
  IAR$logI<-log(IAR$I)

# plot(IAR$logA, IAR$logI)
# 
# ggplot(IAR, aes(logA, logI)) +
#   geom_point() +
#   stat_smooth(method = lm)
# regression
  model <- lm(logI ~ logA, data = IAR)
  z_list[a]<-model$coefficients[2]
  rm(model, IAR)
  
}

IAR_poly$z_val<-unlist(z_list)

# poly that contains site
# 
# int <- sf::st_intersects(IAR_poly , site)
# targ_poly<-st_join(IAR_poly, site, st_intersects)
# targ_poly<-targ_poly%>%filter(!is.na(feature_type))
# targ_poly<-st_as_sf(targ_poly)
# A1<-st_area(targ_poly)
# I1<-targ_poly$constant
# 
# ### increase area and recalc A and I
# 
# nn_poly<-st_join(IAR_poly, targ_poly, st_nn, k = 1,maxdist = 300)
# nn_poly<-nn_poly%>%filter(!is.na(constant.y))
# A2<-sum(st_area(nn_poly))
# I2<-mean(nn_poly$constant.x,na.rm=T)
# plot(nn_poly)
# 
# 
# ### increase area and recalc A and I
# 
# nn_poly2<-st_join(IAR_poly, targ_poly, st_nn, k = 1,maxdist = 1000)
# nn_poly2<-nn_poly2%>%filter(!is.na(constant.y))
# A3<-sum(st_area(nn_poly2))
# I3<-mean(nn_poly2$constant.x,na.rm=T)
# plot(nn_poly2)
# 
# 
# ### increase area and recalc A and I
# 
# nn_poly3<-st_join(IAR_poly, targ_poly, st_nn, k = 1,maxdist = 4000)
# nn_poly3<-nn_poly3%>%filter(!is.na(constant.y))
# A4<-sum(st_area(nn_poly3))
# I4<-mean(nn_poly3$constant.x,na.rm=T)
# plot(nn_poly3)

## morans I of nn_poly spatial autocorrelation of dist = 700
# nb <- poly2nb(nn_poly, queen=TRUE)
# lw <- nb2listw(nb, style="W", zero.policy=TRUE)
# I <- moran(nn_poly$constant.x, lw, length(nb), Szero(lw))[1]
# I
# value at site 

# site_val<-extract(IAR, site)
# theta<-90
# dist<-c(300, 0.05, 0.1)
# dist_list<-list()
# val_list<-list()
# 
# for(n in 1: length(dist)){
#   
#   x_vec<-list()
#   y_vec<-list()
#   for(i in 1:365){
#     x_vec[i] = as.numeric(st_coordinates(site)[1,c(1)]) + dist[n] * cos(i)
#     y_vec[i] = as.numeric(st_coordinates(site)[1,c(2)]) + dist[n] * sin(i)
#     
#   }
#   
#   coords<-as.data.frame(cbind(x_vec,y_vec))
# 
#   pts<-st_as_sf(coords,coords = c("x_vec","y_vec"), crs = st_crs(IAR))
#   plot(pts)
# 
#   # ggplot() +
#   #   # geom_raster(data = IAR) +
#   #   geom_sf(data = pts, fill = NA)
#   # 
#   #pts<-st_crs(4326)
#   a<-extract(IAR, pts)
#   a = a$constant
#   corr_list<-list()
#   for(m in 1:1000){
#     b<-sample(a)
#     c<-sample(a)
#     corr_list[m]<-cor(c,b)
#     
#   }
#   corr_list<-as.data.frame(unlist(corr_list))
#   boxplot(corr_list$`unlist(corr_list)`)
# }


## local correlation (window around site 3x3 pixel of IAR raster) d=1
# mean of eight values based on ini index


## local correlation (window with hole around site 9x9 pixel of IAR raster) d=4


## local correlation (window with hole around site 17x17 pixel of IAR raster) d=8




