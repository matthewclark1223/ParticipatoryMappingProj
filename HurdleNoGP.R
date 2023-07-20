source("DataCleaning.R")
library(rstan)
# ML HURDLE MODEL WITH NO GP


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

dat <- as.list(df2)
dat$N = nrow(df2)
dat$Ns = length(unique(dat$S))

out=stan("HurdleEditedMC.stan", cores = 4, chains = 4,iter=6000, data = dat)
#shinystan::launch_shinystan(out)

post <- extract(out)
postdf<-as.data.frame(post)
#write.csv(postdf,"HurdlPposterior.csv")
#postdf<-read.csv("HurdlPposterior.csv")


mytheme=theme(legend.position = "right", plot.background = element_rect(fill = "white", colour = NA),
              axis.text=element_text(color="black",size=15),panel.background = element_rect(fill = "white"),
              strip.background = element_rect(fill = "white", color = "white", size = 1),
              strip.text = element_text(colour = "black",face="bold",size=20),axis.title = element_text(color="black",size=20),
              axis.line = element_line(colour = "black", size = 1),
              legend.background = element_rect(fill="white"),legend.text = element_text(color="black",size=12),
              legend.title = element_text(color="black",size=15))



postdfBERN<-postdf%>%select(c(92:93,95:97))%>%mutate(Draw=1:nrow(postdf))%>%
  pivot_longer(1:5,names_to = "Effect",values_to = "Estimate" )

#number of draws where effect of REDD <0
length(which(postdfBERN[postdfBERN$Effect=="EffREDDBern",]$Estimate<0))/
  nrow(postdfBERN[postdfBERN$Effect=="EffREDDBern",])


#number of draws where effect of theft >0
length(which(postdfBERN[postdfBERN$Effect=="EffTheftBern",]$Estimate>0))/
  nrow(postdfBERN[postdfBERN$Effect=="EffTheftBern",])

#number of draws where effect of committee >0
length(which(postdfBERN[postdfBERN$Effect=="EffMembBern",]$Estimate>0))/
  nrow(postdfBERN[postdfBERN$Effect=="EffMembBern",])

#number of draws where effect of Male >0
length(which(postdfBERN[postdfBERN$Effect=="EffGenBern",]$Estimate>0))/
  nrow(postdfBERN[postdfBERN$Effect=="EffGenBern",])



ggplot(postdfBERN,aes(x=Estimate,y=Effect))+
  ggridges::geom_density_ridges()+geom_vline(xintercept  =0,linetype=2)+
  theme_bw()+xlab("Estimated effect")+ylab("Predictor")+
  ggtitle("Predictors of participating in patrols (dorias)")+
  scale_y_discrete(breaks=c(names(postdf)[c(92:93,95:97)]),
    labels=c("Mangroves\ndeclining","Theft","REDD","Committee\nmember","Male"))+
  mytheme
  

postdfNB<-postdf%>%select(c(98:99,101:103))%>%mutate(Draw=1:nrow(postdf))%>%
  pivot_longer(1:5,names_to = "Effect",values_to = "Estimate" )


#number of draws where effect of committee >0
length(which(postdfNB[postdfNB$Effect=="EffMembNB",]$Estimate>0))/
  nrow(postdfNB[postdfNB$Effect=="EffMembNB",])

length(which(postdfNB[postdfNB$Effect=="EffGenNB",]$Estimate>0))/
  nrow(postdfNB[postdfNB$Effect=="EffGenNB",])

ggplot(postdfNB,aes(x=Estimate,y=Effect))+
  ggridges::geom_density_ridges()+geom_vline(xintercept  =0,linetype=2 )+
  theme_bw()+xlab("Estimated effect")+ylab("Predictor")+
  ggtitle("Predictors of the number of patrols (dorias)")+
  scale_y_discrete(breaks=c(names(postdf)[97:102]),
                   labels=c("Mangroves\ndeclining","Theft","Mangrove size","REDD","Committee\nmember","Male"))+
  mytheme

###get crazy
ggplot(postdfBERN,aes(x=Estimate,y=Effect,fill = 0.5 - abs(0.5 - stat(ecdf))))+
  ggridges::stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE,alpha=0.5)+
  scale_fill_viridis_c(name = "Tail probability", direction = -1, option="G")+
  geom_vline(xintercept  =0,linetype=2 )+
  theme_bw()+xlab("Standardized coefficient estimate")+ylab("Predictor")+
  scale_x_continuous(breaks=c(-2,0,2))+
  #ggtitle("Predictors of participating in patrols (dorias)")+
  scale_y_discrete(breaks=c(names(postdf)[c(92:93,95:97)]),
                   labels=c("Mangroves\ndeclining","Theft","REDD","Committee\nmember","Male"))+
  mytheme

ggplot(postdfNB,aes(x=Estimate,y=Effect,fill = 0.5 - abs(0.5 - stat(ecdf))))+
  ggridges::stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE)+
  scale_fill_viridis_c(name = "Tail probability", direction = -1, option="G")+
  geom_vline(xintercept  =0,linetype=2 )+
  theme_bw()+xlab("Standardized coefficient estimate")+ylab("Predictor")+
  scale_x_continuous(breaks=c(-2,0,2),limits=c(-3,3))+
  #ggtitle("Predictors of the number of patrols (dorias)")+
  scale_y_discrete(breaks=c(names(postdf)[c(97:99,101:102)]),
                   labels=c("Male","Mangroves\ndeclining","Theft","REDD","Committee\nmember"))+
  mytheme



#######


#regular parameter plots

#BERN Pars
postdfBERN%>%
  group_by(Effect)%>%
  summarise(lower=quantile(Estimate,.25),
            upper=quantile(Estimate,.75),
            top = quantile(Estimate,.95),
            bottom = quantile(Estimate,.05),
            mid=quantile(Estimate,.5))%>%
  ggplot(., aes(y = mid ,x=Effect,ymin=lower,ymax=upper))+ggtitle("")+#maybe put A/B here for panel lable
  geom_linerange( mapping=aes(x=Effect, ymin=bottom, ymax=top), size=0.5,position = position_dodge(width = 0.5),alpha=0.8) +##change deets
  geom_pointrange(position = position_dodge(width = 0.5),size=1.2,alpha=0.99)+
  scale_y_continuous(breaks=c(-2,0,2),limits=c(-2.2,2.2))+
  scale_x_discrete(breaks=c(names(postdf)[c(92:93,95:97)]),
                   labels=c("Mangroves\ndeclining","Theft","REDD","Committee\nmember","Male"))+
  coord_flip(clip = "off")+geom_hline(yintercept=0,linetype="dashed",color="darkgrey" )+
  ggthemes::theme_clean()+theme(text=element_text(  family="Comic Sans MS", face= "plain"),
                                axis.text.y  = element_text(face= "plain"),
                                axis.title = element_text(color="black",size=20),
                                axis.text=element_text(color="black",size=15))+
  ylab("Standardized coefficient estimate")+xlab("Predictor")

#NB Pars
postdfNB%>%
  group_by(Effect)%>%
  summarise(lower=quantile(Estimate,.25),
            upper=quantile(Estimate,.75),
            top = quantile(Estimate,.95),
            bottom = quantile(Estimate,.05),
            mid=quantile(Estimate,.5))%>%
  ggplot(., aes(y = mid ,x=Effect,ymin=lower,ymax=upper))+ggtitle("")+#maybe put A/B here for panel lable
  geom_linerange( mapping=aes(x=Effect, ymin=bottom, ymax=top), size=0.5,position = position_dodge(width = 0.5),alpha=0.8) +##change deets
  geom_pointrange(position = position_dodge(width = 0.5),size=1.2,alpha=0.99)+
  scale_y_continuous(breaks=c(-2,0,2),limits=c(-2.2,2.2))+
  scale_x_discrete(breaks=c(names(postdf)[c(98:99,101:103)]),
                   labels=c("Mangroves\ndeclining","Theft","REDD","Committee\nmember","Male"))+
  coord_flip(clip = "off")+geom_hline(yintercept=0,linetype="dashed",color="darkgrey" )+
  ggthemes::theme_clean()+theme(text=element_text(  family="Comic Sans MS", face= "plain"),
                        axis.text.y  = element_text(face= "plain"),
                        axis.title = element_text(color="black",size=20),
                        axis.text=element_text(color="black",size=15))+
  ylab("Standardized coefficient estimate")+xlab("Predictor")


###

#Make more standard coefficient plots



#######################################################
############## Model output simulations ###############

#
x.seq <-seq(0:100)*.01
N = 1000
out <- list()
for(REDD in 1:2){
  link <- function(x.seq){
    rethinking::inv_logit(post$IntBern+rowMeans(post$ranIntBern)+post$Eff1Bern[,REDD]*x.seq)
  }
  p<-sapply(x.seq, link)
  link2 <- function(x.seq){
    exp(post$IntNB+rowMeans(post$ranIntNB)+post$Eff1NB[,REDD]*x.seq+post$Eff2NB[,REDD]*x.seq^2)
  }
  mu <- sapply(x.seq, link2)
  y <- rnbinom(N, size = post$phi, mu = mu)*rbernoulli(N, p)
  out[[REDD]] <- y
 }

plot(dat$Y~ dat$IND)
lines(colMeans(out[[1]]) ~ c(x.seq))
lines(colMeans(out[[2]]) ~ c(x.seq))
#####
rowMeans(postdf[,5:47])


#Median likelihood that womwn engage in patrols. Change 0 to 1 for men
median(rethinking::inv_logit(postdf$IntBern+rowMeans(postdf[,5:47])+
                        postdf$EffDeclBern *mean(df2$Decl)+postdf$EffTheftBern*mean(df2$Theft)+
                        postdf$EffAreaBern *mean(df2$Area)+postdf$EffREDDBern*mean(df2$REDD)+
                        postdf$EffMembBern *mean(df2$Memb)+postdf$EffGenBern*0))

#quantile. Change 0.9 to 0.1 for lower bound. And remember to change gender
quantile(rethinking::inv_logit(postdf$IntBern+rowMeans(postdf[,5:47])+
                               postdf$EffDeclBern *mean(df2$Decl)+postdf$EffTheftBern*mean(df2$Theft)+
                               postdf$EffAreaBern *mean(df2$Area)+postdf$EffREDDBern*mean(df2$REDD)+
                               postdf$EffMembBern *mean(df2$Memb)+postdf$EffGenBern*1),0.9)


#now do REDD: 1 is not REDD, 2 is REDD
median(rethinking::inv_logit(postdf$IntBern+rowMeans(postdf[,5:47])+
                               postdf$EffDeclBern *mean(df2$Decl)+postdf$EffTheftBern*mean(df2$Theft)+
                               postdf$EffAreaBern *mean(df2$Area)+postdf$EffREDDBern*1+
                               postdf$EffMembBern *mean(df2$Memb)+postdf$EffGenBern*mean(df2$Gen)))

#quantiles
quantile(rethinking::inv_logit(postdf$IntBern+rowMeans(postdf[,5:47])+
                               postdf$EffDeclBern *mean(df2$Decl)+postdf$EffTheftBern*mean(df2$Theft)+
                               postdf$EffAreaBern *mean(df2$Area)+postdf$EffREDDBern*2+
                               postdf$EffMembBern *mean(df2$Memb)+postdf$EffGenBern*mean(df2$Gen)),0.1)


#this doesnt work below!! I tdont think..
mu<-exp(postdf$IntNB+rowMeans(postdf[,49:90])+
  postdf$EffDeclNB *mean(df2$Decl)+postdf$EffTheftNB*mean(df2$Theft)+
  postdf$EffAreaNB *mean(df2$Area)+postdf$EffREDDNB*mean(df2$REDD)+
  postdf$EffMembNB *mean(df2$Memb)+postdf$EffGenNB*1)


estty<-rnbinom(n=length(mu),mu=mu,size=postdf$phi)
estty<-ifelse(estty>0,estty,NA)
mean(estty,na.rm=T)


