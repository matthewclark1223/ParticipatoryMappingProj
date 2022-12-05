library(tidyverse)



source("DataCleaning.R")


df<-df%>%mutate(
  stdOutsidersCutting=stdize(NumberOutsidersCutting221),
  stdMangPercDecl=stdize(MangPercDecl),
  stdMangArea=stdize(MangroveSq112))%>%
  mutate(CommmitteeMem = as.integer(CommmitteeMem))


df2 <-data.frame(Decl = df$MangPercDecl,
                 Y = df$TimesPatrolled244,
                 REDD = as.integer(as.factor(df$REDD)),
                 S = as.integer(as.factor(df$Shehia314)),
                 Theft = df$stdOutsidersCutting,
                 Gen = ifelse(df$Sex318=="M",1,0),
                 Memb = df$CommmitteeMem,
                 Area = df$stdMangArea)

df2 <- df2[complete.cases(df2), ]

postdf<-read.csv("HurdlPposterior.csv")

##
df<-postdf[,882:1659]
df<-df%>%mutate(Draw=1:nrow(df))%>%
  pivot_longer(!Draw,names_to = "Parameter",values_to = "Estimate")

dfZ<-df[grep("Z", df$Parameter), ]
dfZ$Observation<-1:nrow(dfZ)
names(dfZ)[3]<-"Z"
dfZ<-dfZ[,c(-2)]
dfy<-df[grep("y", df$Parameter), ]
dfy$Observation<-1:nrow(dfy)
names(dfy)[3]<-"y"
dfy<-dfy[,c(-2)]
df<-cbind(dfZ,dfy)
df<-df[-c(3:4)]

df$ymod<-ifelse(df$Z ==0,0,df$y)

ggplot(df2)+
  geom_density(aes(x=Y),size=1.5,color="blue",stat="count",show.legend = TRUE)+
  geom_density(data=df[df$Draw%in%sample(unique(df$Draw),500),],aes(x=ymod,group=Draw),size=0.5,color=alpha("lightblue", 0.1),stat="count")+
  theme_classic()+xlim(0,90)+xlab("Patrols")+ylab("Number of Observations")+
  theme(axis.title = element_text(color="black",size=18),
        axis.text = element_text(color="black",size=15))




##

df<-as.data.frame(apply(postdf,2,median))
df$par<-row.names(df)
df<-df[-c(1:880,1659),]
names(df)[1]<-"Value"
row.names(df)<-NULL
df<-data.frame(n=1:389,zPred=df[1:389,]$Value,yPred=df[390:778,]$Value)
plot(df$yPred)
points(dat$Y,add=TRUE,col="red")


hist(dat$Y,breaks=100)
