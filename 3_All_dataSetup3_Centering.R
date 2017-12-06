# Multiple stressors on coral reefs
# goal: centering variables for models
#####################################
remove(list=ls())

loc=("C:/wd/Resilience")
setwd(loc)


# load data 
d1<-read.csv("./results/RS_only/IndpVar_Ptsb_RS.csv",header = T) 
head(d1)
names(d1)
# str(d1)

         
######################################################

################
# transforming
###############
###############################
# Make codes into factors
d1$Id_geom<-as.factor(d1$Id_geom)
d1$EcoZone<-as.factor(d1$EcoZone)
d1$Id_MPAc<-as.factor(d1$Id_MPAc)
d1$Id_MPAb<-as.factor(d1$Id_MPAb)

#############################
# centering variables based on mean
# from gelman and hill p 55
# https://www.r-bloggers.com/a-faster-scale-function/
# or substract by mean and divide by max (0-1)
##############################

# subset numeric variables only
d2<-dplyr::select(d1,Ldepth:LdestDif_10_90)
head(d2)
names(d2)
nm<- names(d2)

# center
d2.c<-data.frame(scale(d2, scale = F))
str(d2.c)

nm.c<-paste("c.",nm,sep="")
names(d2.c)<-nm.c
names(d2.c)

# standardized using z-scale transformation
d2.z<-data.frame(scale(d2))
str(d2.z)
nm.z<-paste("z.",nm,sep="")
names(d2.z)<-nm.z
names(d2.z)

# recombine just with z.transformed
d3<-cbind(d1,d2.z)
head(d3)

d4<- gather(d2,vari,val,Ldepth:LdestDif_10_90)%>%
	ddply("vari", summarise,
				u.Lvar=mean(val),
				sd.Lvar=sd(val))%>%
	arrange(vari)
head(d4)
	
########################
# Save
write.table(d3,file="./results/RS_only/IndpVar_Ptsc_RS.csv",col.names = T,row.names = F, sep=",")
write.table(d4,file="./results/RS_only/IndpVar_Ptsc_RS_MeanSD.csv",col.names = T,row.names = F, sep=",")

#############################################
#reset wd
setwd("C:/wd/Resilience/bin/")

