# Multiple stressors on coral reefs
# goal: This code removes Olango Island (not part of study area) and combines 2 EcoZone categories that are not significantly different (coastal, terrestrial island)

##########################

remove(list=ls())

# set working directory
loc=("C:/wd/Resilience/")
setwd(loc)


# load data 
d1<-read.csv("./results/RS_only/IndpVar_Ptsc_RS.csv",header = T) 
head(d1)
names(d1)
d1<-na.omit(d1)

#######################
# remove Olango Island - not focus of data collection so trust it less 
d2<-filter(d1,d1$EcoZone!="Olango") 
str(d2)

options(na.action = "na.fail")

# remove enormous outlier for earlier run of model PtID = 4272
d2<-filter(d2,d2$CID!="4272") 


##############################
# NOTES:
# leave out mgProx & sgProx? -  model throws errors with mgProx
# # z.LCoRuEdg2Area #not better with edge/area composite
# using LPopRsk.nrm instead of reefs at risk data because RaR data was non-sensical
# dist to river and mangroves are negatively correlated


# combine terrestrial island and coastal ecological zones
d2$EcoZone2<-as.character(d2$EcoZone)
d2$EcoZone2[d2$EcoZone2=="Terrestrial Island"]<-"Coastal"

# remove coastal - test
d3<-filter(d2,d2$EcoZone2!="Coastal") 
d3$EcoZone2<-as.factor(d3$EcoZone2)

###########
names(d2)

# select relevant variables
d4<-dplyr::select(d2,x:yearEst,z.Ldepth:EcoZone2)
names(d4)

#############
# write data to table
# d2 = all ecological zones, d3 = no coastal


write.table(d4,file="./results/RS_only/IndpVar_Ptsd_RS.csv",col.names = T,row.names = F, sep=",")
write.table(d3,file="./results/RS_only/IndpVar_Ptsd_RS_NoCoastalTerr_RS.csv",col.names = T,row.names = F, sep=",")

#############################################
#reset wd
setwd("C:/wd/Resilience/bin/")
