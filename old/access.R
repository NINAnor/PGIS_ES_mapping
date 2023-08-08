library(osrm)
library(sf)
library(dplyr)
library(rgee)
library(openrouteservice)
library(sp)
library(raster)


#nc = st_read(system.file("shape/nc.shp", package="sf"))

ee_Initialize(user = 'r.spielhofer@bluewin.ch')
# 

bound_reg<-ee$FeatureCollection("FAO/GAUL_SIMPLIFIED_500m/2015/level2")$
  filter(ee$Filter$eq("ADM2_CODE",23463))


sf_bound <- ee_as_sf(x = bound_reg)
sf_bound_sp <- as_Spatial(sf_bound)
## make a grid with app. 250m grid cells
n_cells<-round(area(sf_bound_sp)/62500,0)

idw_grid <- spsample(sf_bound_sp, type = "regular", n = n_cells)


samp_size = 100
N=round(n_cells/samp_size,0)*2

df_ini<-NULL

for (i in 1:N) {
p1 = st_sample(sf_bound,size = samp_size, type= "random")
p1<-st_set_crs(p1,st_crs(sf_bound))

travel_time <- osrmTable(p1)

## add mean values to each pts
mean_travel_time<-colMeans(travel_time$duration)

p2<-st_as_sf(p1)
p2$duration_ind<-(mean_travel_time-min(mean_travel_time))/(max(mean_travel_time)-min(mean_travel_time))

df_ini<-rbind(df_ini, p2 )  
print(paste0(i/N*100," %"))
}


#### for individual acc raster:
M=40
src<-rand_dst[1]
### raster from one place
for (i in 1:M) {
rand_dst = st_sample(sf_bound,size = 150, type= "random")
rand_dst<-st_set_crs(rand_dst,st_crs(sf_bound))

dst<-rand_dst[c(2:length(rand_dst))]

travel_time <- osrmTable(dst = dst, src=src)
#travel_time$durations[1:5,1:5]

## add mean values to each pts
# mean_travel_time<-colMeans(travel_time$duration)

dst<-st_as_sf(dst)
dur<-travel_time$durations[1,]
dst$duration<-dur

df_ini<-rbind(df_ini, dst %>% dplyr::select(duration, x))  
print(i)
}

###### test ors
p1 = st_sample(sf_bound,size = 55, type= "random")
p2<-st_coordinates(p1)
res <- ors_matrix(p2, metrics = c("duration", "distance"), units = "km",
                  profile = "foot-walking",api_key = "5b3ce3597851110001cf624852453c6fed1d40c5812a70e4d71c8fe6")

mean_travel_time<-colMeans(res$durations)
mean_travel_time<-(mean_travel_time-min(mean_travel_time))/(max(mean_travel_time)-min(mean_travel_time))

p1<-st_as_sf(p1)
p1$duration_ind<-mean_travel_time
p_end<-rbind(p1,p_end)

n_cells<-round(area(sf_bound_sp)/62500,0)

idw_grid <- spsample(sf_bound_sp, type = "regular", n = n_cells)

df_ini_sp <- as_Spatial(p_end)



for (i in 1:N) {
  p1 = st_sample(sf_bound,size = 55, type= "random")
  p2<-st_coordinates(p1)
  res <- ors_matrix(p2, metrics = c("duration", "distance"), units = "km",
                    profile = "driving-car",api_key = "5b3ce3597851110001cf624852453c6fed1d40c5812a70e4d71c8fe6")
  ## add mean values to each pts
  mean_travel_time<-colMeans(res$duration)/60
  
  p2<-st_as_sf(p1)
  p2$duration_ind<-(mean_travel_time-max(mean_travel_time))/(min(mean_travel_time)-max(mean_travel_time))

  p_end<-rbind(p2,p_end)
  
  print(paste0(i/N*100," %"))
}









### interpolate with idw

df_ini_sp <- as_Spatial(p_end)


## idw
rslt_pt <- gstat::idw(duration_ind~1, df_ini_sp, newdata = idw_grid, idp = 2)
rslt_df2 <- as.data.frame(rslt_pt)
rslt_grd_df <- rslt_df2 %>% filter(!is.na(var1.pred))

rslt_df2_rst <- rslt_df2 %>% 
  dplyr::select(-var1.var) %>% 
  raster::rasterFromXYZ()

# r2<-(rslt_df2_rst-min(rslt_df2_rst))/()

plot(rslt_df2_rst)
raster::crs(rslt_df2_rst) <- "EPSG:4326"
writeRaster(rslt_df2_rst,"C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/acc_rast_ors_car_5000.tif",overwrite = T)




############ osm rasterization and walking speed

## osm
library(osmdata)

roads <- opq(bbox = st_bbox(sf_bound)) %>%
  add_osm_feature(key = 'highway', value = c("motorway","trunk","primary","secondary","tertiary","residential","unclassified"))%>%
  osmdata_sf ()
  #add_osm_feature(key = 'highway', value = c("motorway","trunk","primary"))%>% 

roads <- osm_poly2line(roads)
roads <- roads$osm_lines
roads<-roads%>%dplyr::select(osm_id)

roads<-st_transform(roads,"ESRI:102103")

plot(roads)


all <- opq(bbox = st_bbox(sf_bound)) %>%
  add_osm_feature(key = 'highway', value = c("path","motorway","trunk","primary","secondary","tertiary","residential","unclassified"))%>%
  osmdata_sf ()
#add_osm_feature(key = 'highway', value = c("motorway","trunk","primary"))%>%

all <- osm_poly2line(all)
all <- all$osm_lines
all<-all%>%dplyr::select(osm_id)

all<-st_transform(all,"ESRI:102103")

plot(all)



## 30M RASTER roads
roads_rst <- raster(extent(roads), crs=projection(roads), resolution  = 30)
t1<-Sys.time()
roads_rst<-rasterize(roads,roads_rst)
t2<-Sys.time()
print(t2-t1)
plot(roads_rst)

## 50M RASTER paths
all_rst <- raster(extent(all), crs=projection(all), resolution  = 30)
t1<-Sys.time()
all_rst<-rasterize(all,all_rst)
t2<-Sys.time()
print(t2-t1)
plot(all_rst)

#reclass to 0.85 where path is present and 1 where no path


all_rst_recl<-reclassify(all_rst,
                           cbind(NA, 1))

all_rst_recl<-reclassify(all_rst_recl,
                           cbind(1,14000, 0.85))

plot(all_rst_recl)
## upload to gee
writeRaster(all_rst_recl,"C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/6_PGIS_TOOL_variables/tests_accessibility_raster/all_recl_30_test.tif",overwrite = T)
writeRaster(roads_rst,"C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/6_PGIS_TOOL_variables/tests_accessibility_raster/road_30.tif",overwrite = T)


## reproject raster for gee upload




###### compare the nina computed raster and the EI (Burak et al, 2021)

raster_path<-"C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/6_PGIS_TOOL_variables/tests_accessibility_raster"

diff_acc<-raster(paste0(raster_path,"/diff.tif"))
plot(diff_acc)
hist(diff_acc)
