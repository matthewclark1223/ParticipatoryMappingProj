library(tidyverse)
library(magrittr)
#setwd("C:/Users/jeffrey_andrews/OneDrive/Documents/Matt/Models")
df<-read_csv("../FullSample.csv")

CofmaWards<-c("Changaweni", "Fundo", "Gando", "Kambini","Kangani","Kifundi","Kisiwa Panza",
              "Mgelema","Mgogoni","Michenzani","Mjimbini","Mjini Wingwi","Msuka Magharibi","Mtambwe Kaskazini",
              "Mtambwe Kusini","Shumba Mjini","Tondooni","Tumbe Magharibi")


df%<>%
  #Make some new variables
  
  #percent of mangrove cover that is decreasing
  mutate(MangPercDecl = MangroveSqDecrease115/MangroveSq112)%>%
  
  #Percent of landcover that contains mangroves
  mutate(MangPercCover = MangroveSq112/TotalGridSq111)%>%
  
  #Net change in mangrove cover (squares)
  mutate(NetMangChngSquares = MangroveSqIncrease114-MangroveSqDecrease115)%>%

  #Net change in mangrove cover (perc)
  mutate(NetMangChngPerc = (MangroveSqIncrease114-MangroveSqDecrease115)/MangroveSq112)%>%
  
  #Percent of stealers resulting from leakage
  mutate(LeakageStealers = NumberOutsideShehiaConservation225/NumberOutsidersShehia223) %>%
  
  #Remove some bad data
  
  #Did they understand what they were doing
  filter(UnderstoodMapActivity=="Yes")%>%
  
  #Some insane outliers
  filter(NumberOutsidersCutting221<500)%>%
  filter(TimesPatrolled244<100)%>%
  filter(BundlesAbleToCollect230<50)%>%
  
  #Round up the bundles to an integer
  mutate(BundlesAbleToCollect230 = ceiling(BundlesAbleToCollect230))%>%
  mutate(Shehia314=as.factor(Shehia314)) %>%
  
  #Make variable for member of any conservation committee
  mutate(CommmitteeMem = ifelse(MemberSCC2411=="Yes" | VillageConsCom2412 =="Yes",TRUE,FALSE))%>%
  mutate(CommmitteeMem = ifelse(is.na(CommmitteeMem)==TRUE,FALSE,CommmitteeMem))%>%
  
  #add in institutional history
  mutate(REDD=ifelse(Shehia314 %in% CofmaWards,TRUE,FALSE))

stdize<-function(x){
  (x-mean(x,na.rm=T))/(2*sd(x,na.rm=T))}

df<-df%>%mutate(
  stdYearsTo2M=stdize(Years2Meters212),
  stdYearsTo5M=stdize(Years5Meters213),
  stdYearsCantSee=stdize(YearsUntilCantSee210),
  stdMangPercDecl=stdize(MangPercDecl),
  stdYearsFlowers=stdize(YearsFlowersFruit211))# %>%
    # select(PlantingEventsAttended248,stdYearsTo2M,stdYearsTo5M,stdYearsCantSee,
    #      stdYearsFlowers,Shehia314,REDD,PlantingHost249,stdMangPercDecl,NumberPlantingEvents247)%>%
     #filter(NumberPlantingEvents247<100)%>%
    ## filter(NumberPlantingEvents247!=0))

df<-df%>%mutate(
  stdOutsidersCutting=stdize(NumberOutsidersCutting221),
  stdMangPercDecl=stdize(MangPercDecl),
  stdMangArea=stdize(MangroveSq112))%>%
  mutate(REDDP=ifelse(REDD==TRUE,"REDD","Not REDD"),
         MemberSCC = ifelse(MemberSCC2411%in%c("No",NA),0,1L))


