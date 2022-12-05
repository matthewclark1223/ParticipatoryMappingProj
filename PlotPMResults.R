library(sf)
library(tidyverse)

Pemba<-read_sf("~/Pemba_Project/PembaShapeFile.shp")
#d<-Pemba%>%st_union()
grid<-st_make_grid(Pemba,cellsize = 0.004516827922662469,square=TRUE) #500m at -5degrees
grid_clip<-grid[Pemba]


grid_df<-st_as_sf(grid_clip)
grid_df$CellNumber<-1:nrow(grid_df)
#ggplot(data=Pemba)+geom_sf()+geom_sf(data=grid_df,aes(fill=Value))



#Pem_shehia<-Pemba%>%filter(NAME_3%in%c("Junguni"))
#grid_df_shehia<-grid_df[Pem_shehia,]
#ggplot(data=Pem_shehia)+geom_sf()+geom_sf(data=grid_df_shehia,fill=NA,alpha=0.5)+
#  geom_sf_label(data=grid_df_shehia,aes(label=CellNumber))

files<-list.files("~/Pemba_Project/ParticipatoryMapping/EnteredPMData/WithChanges", full.names = TRUE)
data<-vroom::vroom(files)

data$CellValue<-ifelse(data$CellValue==-99,NA,data$CellValue)

data<-data%>%group_by(CellNumber)%>%
  summarise(Mangrove=as.logical(median(Mangrove)),
            SumValue=sum(CellValue,na.rm=TRUE),
            MeanValue = mean(CellValue,na.rm=TRUE),
            MedValue=median(CellValue,na.rm=TRUE))

grid_df2<-base::merge(x=grid_df,y=data,by="CellNumber", all.x=TRUE )

#st_write(grid_df2,"~/Pemba_Project/ParticipatoryMapping/ParticipatoryMangroveChange.shp")

grid_df2$SumValue<-ifelse(grid_df2$Mangrove==FALSE,NA,grid_df2$SumValue)
grid_df2$MeanValue<-ifelse(grid_df2$Mangrove==FALSE,NA,grid_df2$MeanValue)

ggplot(data=Pemba)+
  geom_sf(color="black",fill="white",alpha=0.01)+
  geom_sf(data=grid_df2,aes(fill=MeanValue),alpha=0.99)+
  scale_fill_viridis_c( na.value=alpha("white",alpha=0.01) )+
  #scale_fill_manual(values =c("white","green"))+
  theme_bw()+theme(panel.grid = element_blank(),axis.text.x = element_text(angle = 45))
