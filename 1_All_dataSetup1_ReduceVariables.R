# Multiple stressors on coral reefs
# goal: Reduce number of variables, keep variables using in analysis
###############################


##############################################################################
remove(list=ls())

loc=("C:/wd/Resilience/")
setwd(loc)

list.files(loc)

# load data #
d1<-read.csv("./results/RS_only/IndpVar_Pts_RS.csv",header = T); names(d1)
names(d1)

d2<-dplyr::select(d1, x,y,CID,Id_resil,Nm_Resil,Nm_Resil3, Geomorphic,Id_geom,EcoZone,Id_Ezone, Depth_m, Depth1,Id_MPAb,Id_MPAc,yearEst,divSpec2010:sg_minDist,CoRuLngth,CoRuArea,PopRskDecay,PopRsk.Nrm,distBgy.km,distTown.km,distMarket.km,distRiver.km,fYr00A:dfYrLag50A,therm_98_07:destDif_90_80,cum_blast00:lag_poison50)%>% 
	#sg_uDist, sgProx:mg_uDist,mgProx, Id_dMg,Id_dSg,RskCstDev,RskMrPlDmg,RskWtrShdPl,
  arrange(CID)
names(d2)

range(d2$Depth_m) # some depths are outliers...
# set to -15 max 
# confirmed in GIS - this is a problem with the bathy map - all deep areas are around reef slope
d3<-d2
d3$Depth1<-as.character(d3$Depth1)
d3$Depth1[d3$Depth_m <= -15] <- "Less than 15m"
d3$Depth_m[d3$Depth_m <= -15]<- -15

head(d3)
d3<-arrange(d3,CID)


# Save Table
write.table(d3,file= "./results/RS_only/IndpVar_Ptsa_RS.csv",sep=",", row.names=F, col.names=T)

# set wd to bin
setwd("C:/wd/Resilience/bin/")
