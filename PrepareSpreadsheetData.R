library(sf)
library(tidyverse)

Pemba<-read_sf("~/Pemba_Project/PembaShapeFile.shp")
#d<-Pemba%>%st_union()
grid<-st_make_grid(Pemba,cellsize = 0.004516827922662469,square=TRUE) #500m at -5degrees
grid_clip<-grid[Pemba]


grid_df<-st_as_sf(grid_clip)
grid_df$Value<-1:nrow(grid_df)
ggplot(data=Pemba)+geom_sf()+geom_sf(data=grid_df,aes(fill=Value))



Pem_shehia<-Pemba%>%filter(NAME_3=="Msuka Magharibi")
#Pem_shehia<- st_crop(Pem_shehia, xmin = 39.685, xmax = 39.77,
                      #       ymin = -5.175139, ymax = -5.069029)
grid_df_shehia<-grid_df[Pem_shehia,]
#grid_df_shehia<- st_crop(grid_df_shehia, xmin = 39.685, xmax = 39.77,
                    # ymin = -5.175139, ymax = -5.069029)

ggplot(data=Pem_shehia)+geom_sf()+geom_sf(data=grid_df_shehia,alpha=0.2)+
  geom_sf_label(data=grid_df_shehia,aes(label=Value),size=4)

writeClipboard(rep(as.character(grid_df_shehia$Value),10))

writeClipboard(rep(as.character(c("sh43_1","sh43_2","sh43_3","sh43_4","sh43_5",
                     "sh43_6","sh43_7","sh43_8","sh43_9","sh43_10")),each=length(grid_df_shehia$Value)))
                


