source("DataCleaning.R")

#we need to bin the responses in perceptions of mangrove regrowth.
df<-df%>%filter(Years5Meters213<100)
boxplot(df$YearsUntilCantSee210,df$YearsFlowersFruit211,df$Years2Meters212,df$Years5Meters213)

growth.pca<-prcomp(na.omit(df[,c("YearsUntilCantSee210","YearsFlowersFruit211","Years2Meters212","Years5Meters213")]),center=TRUE,scale.=TRUE)
summary(growth.pca)

library(brms)
#Based on our DAG, patrol effort should be determined by neighborhood (random effect), Occupation, Percieved theft, 
# Percieved env change, and institutional history. From this we can interpret percieved theft and environmental change,
# and Institutional history
options(mc.cores=4)

df<-na.omit(df%>%mutate(
  stdYearsTo2M=stdize(Years2Meters212),
  stdYearsTo5M=stdize(Years5Meters213),
  stdYearsCantSee=stdize(YearsUntilCantSee210),
  stdMangPercDecl=stdize(MangPercDecl),
  stdYearsFlowers=stdize(YearsFlowersFruit211))%>%
    select(PlantingEventsAttended248,stdYearsTo2M,stdYearsTo5M,stdYearsCantSee,
           stdYearsFlowers,Shehia314,REDD,PlantingHost249,stdMangPercDecl,NumberPlantingEvents247)%>%
    filter(NumberPlantingEvents247<100)%>%
    filter(NumberPlantingEvents247!=0))




fit<-brms::brm(PlantingEventsAttended248~stdYearsTo5M+#stdYearsTo2M+
                 #stdYearsCantSee+
                 #stdYearsFlowers+
                 PlantingHost249+stdMangPercDecl+
                 offset(log(NumberPlantingEvents247))+
                 REDD+(1|Shehia314),family="poisson",data=df)

mcmc_plot(fit)
brms::conditional_effects(fit)

















dat<-df%>%select(BundlesAbleToCollect230,Years2Meters212,Years5Meters213)%>%na.omit()
#data for modeling
data_list <- list(
  N = nrow(dat),
  KorjaCollect = dat$BundlesAbleToCollect230,
  Growth2yr = dat$Years2Meters212,
  Growth5yr = dat$Years5Meters213
  
)







options(mc.cores=1)#10

rstan::rstan_options(autowrite=TRUE)

mod<-rstan::stan( file="PerceptionsofFunctionMixtureModel.stan" , 
                  data=data_list,chains=1,iter=2000)


print( mod , probs=c( 0.05 , 0.95  ))

dat<-na.omit(df%>%select(BundlesAbleToCollect230,Years2Meters212))
data_list <- list(
  N1 = nrow(dat),
  y = dat$BundlesAbleToCollect230,
  x1 = dat$Years2Meters212,
  N2 = nrow(dat),
  x2 = dat$Years2Meters212
  
)
mod<-rstan::stan( file="gausianProcessModel.stan" , 
                  data=data_list,chains=1,iter=2000)


z<-rstan::extract(mod)

z<-as.data.frame(z$y2)
x<-as.data.frame(apply(z,2,median))
names(x)[1]<-"PredictedKorjaCollect"
x$Years2Meter<-dat$Years2Meters212





dat%>%
ggplot(.,aes(x=Years2Meters212,y=BundlesAbleToCollect230))+geom_point()+
  geom_line(data=x,aes(x=Years2Meter,y=PredictedKorjaCollect))



library(mgcv)
modd<-gam(BundlesAbleToCollect230~s(Years2Meters212),
         data=dat,method="REML")
plot(modd,residuals=TRUE,pch=1,cex=1,shade=T,
     shade.col="lightblue")

modd2<-gam(BundlesAbleToCollect230~s(Years2Meters212,bs="fs")+s(Years5Meters213,bs="fs")+s(YearsFlowersFruit211,bs="fs"),
          data=df,method="REML")





