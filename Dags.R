install.packages("dagitty")
install.packages("ggdag")
library(dagitty)
library(ggdag)
library(ggplot2)

Dag2<-dagitty('dag {
  bb="-0.5,-0.5,0.5,0.5"
  "Community mangrove size" [pos="-0.311,-0.375"]
  "Conservation committee member" [exposure,pos="0.268,-0.388"]
  "Distance to other communities" [pos="-0.331,0.243"]
  "Institutional history (REDD+)" [exposure,pos="-0.134,-0.466"]
  "Mangrove status (degradation)" [latent,pos="-0.025,0.254"]
  "Patrolling behavior" [outcome,pos="-0.014,-0.221"]
  "Perceived theft" [exposure,pos="0.100,0.213"]
  "Perception of mangrove change" [exposure,pos="0.054,-0.464"]
  Age [pos="0.221,0.326"]
  Gender [exposure,pos="0.333,-0.150"]
  Occupation [pos="0.306,0.080"]
  Theft [latent,pos="-0.172,0.056"]
  "Community mangrove size" -> "Institutional history (REDD+)"
  "Community mangrove size" -> "Mangrove status (degradation)"
  "Community mangrove size" -> "Patrolling behavior"
  "Community mangrove size" -> Theft
  "Conservation committee member" -> "Patrolling behavior"
  "Conservation committee member" -> "Perceived theft"
  "Conservation committee member" -> "Perception of mangrove change"
  "Distance to other communities" -> "Community mangrove size"
  "Distance to other communities" -> Theft
  "Institutional history (REDD+)" -> "Mangrove status (degradation)"
  "Institutional history (REDD+)" -> "Patrolling behavior"
  "Institutional history (REDD+)" -> Theft
  "Mangrove status (degradation)" -> "Perception of mangrove change"
  "Mangrove status (degradation)" <-> Theft
  "Perceived theft" -> "Patrolling behavior"
  "Perception of mangrove change" -> "Patrolling behavior"
  Age -> "Conservation committee member"
  Age -> Occupation
  Gender -> "Conservation committee member"
  Gender -> "Patrolling behavior"
  Gender -> "Perception of mangrove change"
  Gender -> Occupation
  Occupation -> "Conservation committee member"
  Occupation -> "Perceived theft"
  Theft -> "Perceived theft"
}')


tidy_dagitty(g)
ggdag(Dag2,text = FALSE, use_labels = "name")+theme_void()




#Model 1
Dag1<-dagitty('dag {
  bb="-0.5,-0.5,0.5,0.5"
  "Community mangrove size" [pos="-0.297,-0.372"]
  "Conservation committee member" [exposure,pos="0.237,-0.435"]
  "Distance to other communities" [pos="-0.360,-0.001"]
  "Institutional history (REDD+)" [exposure,pos="-0.142,-0.460"]
  "Mangrove status (degradation)" [latent,pos="0.011,0.236"]
  "Perceived theft" [exposure,pos="0.105,-0.043"]
  "Perception of mangrove change" [exposure,pos="0.066,-0.456"]
  "Preferred fuelwood harvest limit" [outcome,pos="0.142,-0.228"]
  Age [pos="0.289,0.194"]
  Gender [pos="0.302,-0.354"]
  Occupation [pos="0.295,-0.085"]
  Theft [latent,pos="-0.114,-0.134"]
  "Community mangrove size" -> "Institutional history (REDD+)"
  "Community mangrove size" -> "Mangrove status (degradation)"
  "Community mangrove size" -> "Preferred fuelwood harvest limit"
  "Community mangrove size" -> Theft
  "Conservation committee member" -> "Preferred fuelwood harvest limit"
  "Distance to other communities" -> "Community mangrove size"
  "Distance to other communities" -> Theft
  "Institutional history (REDD+)" -> "Mangrove status (degradation)"
  "Institutional history (REDD+)" -> "Perception of mangrove change"
  "Institutional history (REDD+)" -> "Preferred fuelwood harvest limit"
  "Institutional history (REDD+)" -> Theft
  "Mangrove status (degradation)" -> "Perception of mangrove change"
  "Perceived theft" -> "Preferred fuelwood harvest limit"
  "Perception of mangrove change" -> "Preferred fuelwood harvest limit"
  Age -> "Conservation committee member"
  Age -> Occupation
  Gender -> "Conservation committee member"
  Gender -> Occupation
  Occupation -> "Preferred fuelwood harvest limit"
  Theft -> "Mangrove status (degradation)"
  Theft -> "Perceived theft"
}')

ggdag(Dag1,text = FALSE, use_labels = "name")+theme_void()
