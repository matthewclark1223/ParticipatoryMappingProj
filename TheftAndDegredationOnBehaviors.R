source("DataCleaning.R")
library(brms)
library(tidybayes)
#Based on our DAG, patrol effort should be determined by neighborhood (random effect), Occupation, Percieved theft, 
# Percieved env change, and institutional history. From this we can interpret percieved theft and environmental change,
# and Institutional history
options(mc.cores=4)

df<-df%>%mutate(
  stdOutsidersCutting=stdize(NumberOutsidersCutting221),
  stdMangPercDecl=stdize(MangPercDecl),
  stdMangArea=stdize(MangroveSq112))%>%
  mutate(REDDP=ifelse(REDD==TRUE,"REDD","Not REDD"),
         CommmitteeMem = as.integer(CommmitteeMem))


mytheme=theme(legend.position = "bottom", plot.background = element_rect(fill = "#073763ff", colour = NA),
              axis.text=element_text(color="white",size=15),panel.background = element_rect(fill = "#073763ff"),
              strip.background = element_rect(fill = "#073763ff", color = "#073763ff", size = 1),
              strip.text = element_text(colour = "white",face="bold",size=20),axis.title = element_text(color="white",size=20),
              axis.line = element_line(colour = "white", size = 1),
              legend.background = element_rect(fill="#073763ff"),legend.text = element_text(color="white",size=12),
              legend.title = element_text(color="white",size=15))

df<-df%>%select(TimesPatrolled244,stdOutsidersCutting,stdMangPercDecl,Occupation3111,
                stdMangArea,REDDP,Shehia314,BundlesAbleToCollect230,stdMangArea,CommmitteeMem,MangPercDecl)%>%
  na.omit()

(prior<-get_prior(BundlesAbleToCollect230~stdOutsidersCutting*stdMangPercDecl+Occupation3111+
                                         stdMangArea+CommmitteeMem+
                                         REDDP+(1|Shehia314),family="poisson",data=df))

#prior$prior[1]<-"normal(0,1)" #coefficient default
prior$prior[2]<-"normal(0,0.5)" #Member SCC
prior$prior[3]<-"normal(0,0.5)" #craftsman
prior$prior[4]<-"normal(0,0.5)" #farmer
prior$prior[5]<-"normal(0,0.5)" #fisherman
prior$prior[6]<-"normal(0,0.5)" #government
prior$prior[7]<-"normal(0,0.5)" #livestock
prior$prior[8]<-"normal(0,0.5)" #redd =T
prior$prior[9]<-"normal(0,0.5)"#mangrove area
prior$prior[10]<-"normal(1,0.5)" #perc mang area declining
prior$prior[11]<-"normal(1,0.5)" #outsiders cuting
prior$prior[12]<-"normal(1,0.5)" #INTERACTION
prior$prior[13]<-"normal(0,1)" #Intercept default
prior$prior[14]<-"exponential(1)" #sd default
prior$prior[15]<-"exponential(1)" #ran int sd
prior$prior[16]<-"normal(0,1)" #ran int


fit2Int<-brm(BundlesAbleToCollect230~stdOutsidersCutting*stdMangPercDecl+Occupation3111+
                                stdMangArea+CommmitteeMem+
                         REDDP+(1|Shehia314),family="poisson",data=df,prior = prior )

bayesplot::color_scheme_set("darkgray")
mcmc_plot(fit2Int, variable = c("Out","Mang","REDD"), regex = TRUE)+theme_bw()+mytheme+
  geom_vline(xintercept=0,linetype=1,color="white")+
  geom_vline(xintercept=0,linetype=2,color="black")+
  scale_y_discrete(labels=c("Theft","Theft:Mangroves\ndeclining","Mangroves\ndeclining",
                            "Mangrove size","REDD"))+xlab("Standardized coefficient estimate")+
  theme(text=element_text(  family="Comic Sans MS", face= "plain"),
        axis.text.y  = element_text(face= "plain"),
        axis.title = element_text(color="black",size=20),
        axis.text=element_text(color="black",size=15))+ylab("Predictor")


#get probability that effects are +/-

extObj<-extract_draws(fit2Int)
extObj<-as.data.frame(extObj$dpars$mu$fe$b)
length(which(extObj$b_REDDPREDD>0))/nrow(extObj)#81% chance that Redd+ inreases the bundles ppl want to collect
length(which(extObj$b_stdMangArea<0))/nrow(extObj)
length(which(extObj$`b_stdOutsidersCutting:stdMangPercDecl`>0))/nrow(extObj)

#Split by theft level
mytheme=theme(legend.position = "bottom", plot.background = element_rect(fill = "white", colour = NA),
              axis.text=element_text(color="black",size=15),panel.background = element_rect(fill = "white"),
              strip.background = element_rect(fill = "white", color = "white", size = 1),
              strip.text = element_text(colour = "black",face="bold",size=20),axis.title = element_text(color="black",size=20),
              axis.line = element_line(colour = "black", size = 1),
              legend.background = element_rect(fill="white"),legend.text = element_text(color="black",size=12),
              legend.title = element_text(color="black",size=15))
fit2Int%>%
  epred_draws(newdata = expand_grid(stdMangPercDecl = seq(min(df$stdMangPercDecl),max(df$stdMangPercDecl),by=0.05),
                                    stdOutsidersCutting = c(-0.34,3.37),
                                    stdMangArea=0,
                                    REDDP = c("Not REDD"),
                                    CommmitteeMem =c(0),
                                    Shehia314=unique(df$Shehia314),
                                    Occupation3111="Farmer")
  )%>%mutate(Theft=factor(ifelse(stdOutsidersCutting==3.37,"High theft","Low theft"),
                          levels=c("Low theft","High theft")) )%>%
  ggplot(.,aes(x=stdMangPercDecl,y=.epred))+
  tidybayes::stat_lineribbon(aes(fill_ramp = stat(.width)), .width = ppoints(20), fill = "#4d004b") +
  ggdist::scale_fill_ramp_continuous(range = c(0.95, 0),name="Credibility Interval",from="white")+
  #geom_jitter(data=df,aes(x=stdMangPercDecl,y=BundlesAbleToCollect230),size=1,stroke=1,alpha=0.3,fill="darkgrey",color="darkgrey",shape=21)+
  ggthemes::theme_clean() +
  scale_x_continuous(breaks=c(min(df$stdMangPercDecl),(max(df$stdMangPercDecl)/2),max(df$stdMangPercDecl)),
                     labels = c("0%","50%","100%") )+
  facet_wrap(~Theft)+ylab("Bundles Insiders Can Collect")+xlab("Percent of Mangrove Area Declining")+
  theme(legend.position = "bottom")+mytheme






preddat<-fit2Int%>%
  epred_draws(newdata = expand_grid(stdMangPercDecl = seq(min(df$stdMangPercDecl),max(df$stdMangPercDecl),by=0.05),
                                    stdOutsidersCutting = c(-0.34,3.37),
                                    stdMangArea=0,
                                    REDDP = c("Not REDD"),
                                    CommmitteeMem =c(0),
                                    Shehia314=unique(df$Shehia314),
                                    Occupation3111="Farmer")
  )%>%mutate(Theft=factor(ifelse(stdOutsidersCutting==3.37,"High theft","Low theft"),
                          levels=c("Low theft","High theft")) )



#View(preddat)



preddat%>%group_by(stdMangPercDecl,Theft)%>%
  summarise(meddy=median(.epred))%>%ungroup()%>%
  filter(stdMangPercDecl==min(stdMangPercDecl)|
           stdMangPercDecl==max(stdMangPercDecl)|
           stdMangPercDecl==quantile(stdMangPercDecl)[3])
