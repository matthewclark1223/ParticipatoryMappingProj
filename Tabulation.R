source("DataCleaning.R")

head(df)

df2<-df
df2$MemberSCC <- 
  factor(df2$MemberSCC, 
         levels=c(0,1),
         labels=c("Non-Committee member", # Reference
                  "Committee member"))

df2<-df2%>%select(MangPercDecl,TimesPatrolled244,BundlesAbleToCollect230,Age317,
                 NumberOutsidersCutting221,MemberSCC,Sex318,REDDP)
df2<-na.omit(df2)


table1::label(df2$MangPercDecl)       <- "Mangrove area decline"
table1::label(df2$TimesPatrolled244)       <- "Patrols past month"
table1::label(df2$BundlesAbleToCollect230)       <- "Allowable wood bundles"
table1::label(df2$Age317)       <- "Age"
table1::label(df2$NumberOutsidersCutting221)       <- "Weekly outsiders"
table1::label(df2$MemberSCC)       <- "Committee member"
table1::label(df2$Sex318)       <- "Sex"

table1::units(df2$MangPercDecl)       <- "%"
table1::units(df2$Age317) <- "Years"



table1::table1(~  MangPercDecl+ TimesPatrolled244+BundlesAbleToCollect230+Age317  + 
                 NumberOutsidersCutting221 | MemberSCC+factor(Sex318), data=df2,
               overall=c(left="Total"))
                 


