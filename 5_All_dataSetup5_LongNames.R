# Multiple stressors on coral reefs
# goal: Make Pretty Names for final models

############################
remove(list=ls())

loc=("C:/wd/Resilience/results/RS_only")
setwd(loc)

# load data 
d2<-read.csv("./results/RS_only/IndpVar_Ptsd_RS.csv",header = T)
d2$State<-as.factor(d2$Id_resil)
d2$Reef.state<-as.factor(d2$Id_resil)
d2$MPA<-as.factor(d2$Id_MPAb)

d2<-filter(d2,CID!=4215) #outlier

##############
# new names
names(d2)
########################################
#make var have better names
########################################
d2$Fishing.legacy.1980.2000<-d2$z.LfYrLag30A
d2$Isolation.from.seagrass<-d2$z.Lsg_minDist100
d2$Seagrass.isolation<-d2$z.Lsg_minDist100
d2$Depth<-d2$z.Ldepth
d2$Patch.edge.to.area<-d2$z.LPARA
d2$Patch.shape<-d2$z.LSHAPE
d2$Isolation.from.rivers<-d2$z.LdistRiver.km 
d2$Isolation.from.corals<-d2$z.Lco_minDist100
d2$Population.density<-d2$z.LPopRsk.Nrm

d2$Isolation.from.rivers.x.Fishing.Legacy.1980.2010<-d2$z.LdistRiver.km*d2$z.LfYrLag30A 
d2$Fishing.legacy.1980.2010.x.Isolation.from.rivers<-d2$z.LdistRiver.km*d2$z.LfYrLag30A 
d2$Fishing.legacy.1980.2000.x.Population.density<-d2$z.LfYrLag30A+d2$z.LPopRsk.Nrm
d2$Population.density.x.Isolation.from.rivers<-d2$z.LPopRsk.Nrm*d2$z.LdistRiver.km 

d2$Market.proximity<-d2$z.LdistMarket.km
d2$Fishing.pressure.1980.2010<-d2$z.LfYr30A
d2$PatchEdgeLength<-d2$z.LCoRuLngth
d2$PatchNearNeighborDistance<-d2$z.LENN
d2$Distance.from.towns<-d2$z.LdistTown.km
d2$Distance.from.communities<-d2$z.LdistBgy.km
d2$Distance.from.communities.x.Distance.from.towns<-d2$z.LdistTown.km*d2$z.LdistBgy.km
d2$Market.proximity.x.Isolation.from.rivers<-d2$z.LdistMarket.km*d2$z.LdistRiver.km 

############
# Save data with names
names(d2)

d3<-dplyr::select(d2,x:CID,EcoZone2:Market.proximity.x.Isolation.from.rivers); names(d3)

# all data
write.table(d2,"./results/RS_only/IndpVar_Ptse_RS.csv", col.names = T,row.names = F,sep=",")

# just fancy names
write.table(d3,"./results/RS_only/IndpVar_Ptsf_RS.csv", col.names = T,row.names = F,sep=",")

############################
setwd("C:/wd/Resilience/bin/")
