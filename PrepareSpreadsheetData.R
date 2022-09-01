library(sf)
library(tidyverse)

Pemba<-read_sf("~/Pemba_Project/PembaShapeFile.shp")
#d<-Pemba%>%st_union()
grid<-st_make_grid(Pemba,cellsize = 0.004516827922662469,square=TRUE) #500m at -5degrees
grid_clip<-grid[Pemba]


grid_df<-st_as_sf(grid_clip)
grid_df$Value<-1:nrow(grid_df)
ggplot(data=Pemba)+geom_sf()+geom_sf(data=grid_df,aes(fill=Value))



Pem_shehia<-Pemba%>%filter(NAME_3=="Mtambwe Kaskazini")
grid_df_shehia<-grid_df[Pem_shehia,]

ggplot(data=Pem_shehia)+geom_sf()+geom_sf(data=grid_df_shehia,alpha=0.2)+
  geom_sf_label(data=grid_df_shehia,aes(label=Value))

writeClipboard(rep(as.character(grid_df_shehia$Value),10))

writeClipboard(rep(as.character(c("sh1_1","sh1_2","sh1_3","sh1_4","sh1_5",
                     "sh1_6","sh1_7","sh1_8","sh1_9","sh1_10")),each=length(grid_df_shehia$Value)))
                


