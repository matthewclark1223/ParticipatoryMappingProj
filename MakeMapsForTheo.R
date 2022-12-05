library(sf)
library(tidyverse)

Pemba<-read_sf("~/Pemba_Project/PembaShapeFile.shp")
df<-read_csv("../FullSample.csv")

StudyShehia<-unique(df$Shehia314)

#d<-Pemba%>%st_union()
grid<-st_make_grid(Pemba,cellsize = 0.004516827922662469,square=TRUE) #500m at -5degrees
grid_clip<-grid[Pemba]


grid_df<-st_as_sf(grid_clip)
grid_df$CellNumber<-1:nrow(grid_df)

files<-list.files("../EnteredPMData", full.names = TRUE)
data<-vroom::vroom(files)


for(i in StudyShehia){
Pem_shehia<-Pemba%>%filter(NAME_3%in%c(i))

grid_df_shehia<-grid_df[Pem_shehia,]
#ggplot(data=Pem_shehia)+geom_sf()+geom_sf(data=grid_df_shehia,fill=NA,alpha=0.5)+
 # geom_sf_label(data=grid_df_shehia,aes(label=CellNumber))

data2<-data%>%group_by(CellNumber)%>%
  filter(Shehia ==i)%>%
  summarise(Mangrove=as.logical(median(Mangrove)))

grid_df_shehia<-base::merge(x=grid_df_shehia,y=data2,by="CellNumber", all.x=TRUE )

p<-ggplot(data=Pem_shehia)+geom_sf(color="black")+geom_sf(data=grid_df_shehia,aes(fill=Mangrove),alpha=0.5)+
  scale_fill_manual(values =c("white","green"))+
  geom_sf_label(data=grid_df_shehia,aes(label=CellNumber))+theme_void()+
  ggtitle(Pem_shehia$NAME_3)+
  theme(panel.background = element_rect(fill="white",color="white"),
        plot.background = element_rect(fill="white",color="white"))

filename<-paste0("../MapsForTheo/",i,".png")
ggsave(filename,p,width=15,height=11,units="in")
}
