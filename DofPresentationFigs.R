library(tidyverse)
source("DataCleaning.R")
library(sf)
library(ggspatial)


Pemba <- read_sf("~/Pemba_Project/PembaShapeFile.shp")


d<-read.csv("~/Pemba_Project/ParticipatoryMapping/Community Sampling Datasheet.csv")
Sampled<-d[d$Sampled==1,]$Shehia
NeedToSample<-d[d$Sampled==0,]$Shehia
which(Sampled %in% Pemba$NAME_3==F)
which(NeedToSample %in% Pemba$NAME_3==F)

Pemba$Sample<-ifelse(Pemba$NAME_3 %in% Sampled, "Study Shehia",NA)


cols <- c("Study Shehia" = "#1f78b4" )


ggplot(Pemba)+
  geom_sf(aes(fill=Sample))+
  scale_fill_manual(name="",values=cols,na.value="#f0f0f0" )+
  scale_size(range = c(5,15)) +
  annotation_scale(location = "br", width_hint = 0.5) +ylab("")+xlab("")+
  theme_void()





#demographic stats

dfDirt<-read_csv("../FullSample.csv")


mytheme<-theme(axis.title = element_text(color="black",size=20),
               axis.text = element_text(color="black",size=16),
               axis.ticks  = element_line(color="black"))

ggplot(dfDirt)+
  geom_histogram(aes(x=Age317),fill="#525252",color="#d9d9d9")+theme_classic()+mytheme+
  xlab("Age")+ylab("Participants")


dfDirt%>%select(Occupation3111)%>%
  gather(key="Job",value="val")%>%
  filter(is.na(val)==FALSE)%>%group_by(val)%>%count()%>%ungroup()%>%
  mutate(perc = n/sum(n))%>%mutate(question=rep("Job",nrow(.)))%>%mutate(Job=fct_reorder(val,perc,mean))%>%
  
  
  ggplot(., aes(x=question,y=perc,fill=Job)) + geom_bar(stat="identity",position='stack',width = 0.3)+
  geom_text(aes(label=paste0(round(perc*100),"%"),size=perc),
            position=position_fill(0.5),show.legend = FALSE,color="white",fontface="bold")+
  scale_fill_viridis_d( name ="Occupation",
                    labels = c("Mfugaji (Livestock)","Serikali (Government)","Fundi (Craftsman)",
                               "Biashara (Business)","Uvuvi (Fishing)","Kilimo (Farming)"),
                    guide = guide_legend(reverse = TRUE),begin=0,end=0.9,option="B" )+
  coord_flip()+theme_classic()+guides(y="none",x="none")+scale_y_continuous(labels=scales::percent)+ylab("")+xlab("")+
  scale_radius(range = c(3,15))+mytheme#+ggtitle("Percent of responses")

#Same thing for sex
dfDirt%>%select(Sex318)%>%
  gather(key="Job",value="val")%>%
  filter(is.na(val)==FALSE)%>%group_by(val)%>%count()%>%ungroup()%>%
  mutate(perc = n/sum(n))%>%mutate(question=rep("Gender",nrow(.)))%>%mutate(Gender=fct_reorder(val,perc,mean))%>%
  
  
  ggplot(., aes(x=question,y=perc,fill=Gender)) + geom_bar(stat="identity",position='stack',width = 0.3)+
  geom_text(aes(label=paste0(round(perc*100),"%"),size=20),
            position=position_fill(0.5),show.legend = FALSE,color="white",fontface="bold")+
  scale_fill_viridis_d( labels = c("Wanawake (F)","Wanaume (M)"),
                        guide = guide_legend(reverse = TRUE),begin=0,end=0.7,option="C" )+
  coord_flip()+theme_classic()+guides(y="none",x="none")+scale_y_continuous(labels=scales::percent)+ylab("")+xlab("")+
  scale_radius(range = c(3,15))+mytheme#+ggtitle("Percent of responses")

#######


x<-df%>%group_by(Shehia314)%>%
  summarize(stealingMean = mean(NumberOutsidersCutting221),
            stealingMedian = median(NumberOutsidersCutting221))%>%
  mutate(NAME_3=Shehia314)



x<-merge(Pemba,x,by="NAME_3",all.x=TRUE)
y<-x%>%filter(stealingMean>11)

ggplot(x)+
  geom_sf(aes(fill=stealingMean))+
  geom_sf_label(data=y,aes(label=NAME_3),nudge_y = 0.025,nudge_x = -0.005,size=2)+
  scale_fill_viridis_c(name="Mean reported stealing\nper week",option="F", direction=-1 )+theme_void()





#Mang area declining

x<-df%>%group_by(Shehia314)%>%
  summarize(percDecMean = mean(MangPercDecl),
            percDecMedian = median(MangPercDecl))%>%
  mutate(NAME_3=Shehia314)



x<-merge(Pemba,x,by="NAME_3",all.x=TRUE)
y<-x%>%filter(percDecMedian>0.4)

ggplot(x)+
  geom_sf(aes(fill=percDecMedian))+
  geom_sf_label(data=y,aes(label=NAME_3),nudge_y = 0.025,nudge_x = -0.005,size=2)+
  scale_fill_viridis_c(name="Median percent of mangrove\narea declining",option="B",labels = scales::percent ,
                       direction=-1,begin =0,end=1,)+
  
  theme_void()

