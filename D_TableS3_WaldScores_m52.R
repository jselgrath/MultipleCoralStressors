# Multiple stressors on coral reefs
# goal:  Extract Wald test outputs from glmer logit model 

##########################
# Info ####################
# Wald scores can be used to rank relative significance of variables in a model
# http://www.talkstats.com/showthread.php/56992-Comparing-coefficients-in-Logistic-Regression
#http://stats.stackexchange.com/questions/60074/wald-test-for-logistic-regression
#http://sf.oxfordjournals.org/content/89/4/1409.abstract
# https://www.r-bloggers.com/logistic-regression-in-r-part-two/
#https://stat.ethz.ch/pipermail/r-sig-mixed-models/2011q4/014095.html

#http://www.ats.ucla.edu/stat/sas/output/sas_ologit_output.htm
#w. Wald Chi-Square & Pr > ChiSq - These are the test statistics and p-values, respectively, for the hypothesis test that an individual predictor's regression coefficient is zero given the rest of the predictors are in the model. The Wald Chi-Square test statistic is the squared ratio of the Estimate to the Standard Error of the respective predictor. The probability that a particular Wald Chi-Square test statistic is as extreme as, or more so, than what has been observed under the null hypothesis is given by Pr > ChiSq.
# The Wald Chi-Square test statistic for the predictor science (0.030/0.016)2  is 3.584 with an associated p-value of 0.0583. If we set our alpha level to 0.05, we would fail to reject the null hypothesis and conclude that the regression coefficient for science has not been found to be statistically different from zero in estimating ses given socst and female are in the model.
# The Wald Chi-Square test statistic for the predictor socst (0.053/0.015)2 is 12.78 with an associated p-value of 0.0004. If we again set our alpha level to 0.05, this time we would reject the null hypothesis and conclude that the regression coefficient for socst has been found to be statistically different from zero in estimating ses given science and female are in the model. The interpretation for a dichotomous variable parallels the continuous variable.

############################################
# Export final model tabels################
remove(list=ls())
dateToday=Sys.Date()

loc=("C:/wd/Resilience/")
setwd(loc)

# load final models
load("./results/RS_only/Q41_model_52.R")
summary(m.me_52)

# Wald test from car package
w1<-data.frame(Anova(m.me_52))
w1
w1$Variable<-rownames(w1)
row.names(w1)<-c() # remove row names
w1
str(w1)


# improve variable names
w1$Variable<-gsub("MPA601","MPA (present)",w1$Variable)
w1$Variable<-gsub("Population.density","Population density",w1$Variable)
w1$Variable<-gsub("Patch.shape","Patch shape",w1$Variable)
w1$Variable<-gsub("Seagrass.isolation","Seagrass isolation",w1$Variable)
w1$Variable<-gsub("Fishing.legacy.1980.2000","Fishing legacy, 1980-2000",w1$Variable)
w1$Variable<-gsub("Fishing.legacy.1980.2000:Population.density","Fishing legacy, 1980-2000:Population density",w1$Variable) 
w1$Variable<-gsub("z.Lcum_blast10","Blast fishing, 2000-2010",w1$Variable)
w1$labl<-gsub("Population density:Blast fishing, 2000-2010","Blast fishing, 2000-2010:Population density", w1$labl)


w1

# rename columns
names(w1)<-c("Wald.Chi.sq","df","Pr","Variable")
str(w1)
w1$Wald.Chi.sq<-round(w1$Wald.Chi.sq,2)
w1$Pr<-round(w1$Pr,5)


# arrange by influence
Wald.Chi.sq2<-desc(w1$Wald.Chi.sq)
w1$Wald.Chi.sq2<-Wald.Chi.sq2
w1
str(w1)
w2<-dplyr::arrange(w1,Wald.Chi.sq2) # to order by p-val
w3<-dplyr::select(w2,Variable,Wald.Chi.sq,df,Pr)
w3

# change format of p-val for table
w3$p.value<-round(w3$Pr,2)
w3$p.value[w3$Pr<0.001]<-"<0.001"
w3$p.value[w3$Pr<0.01&w3$Pr>=0.001]<-"<0.01"
w3

# select and order columns
# w3$ordr<-c(2,3,1,5,7,4,8,6) # to order as per other tables
# w4<-arrange(w3,ordr)%>%
w4<-dplyr::select(w3,Variable,Wald.Chi.sq,df,p.value)
w4

######
#save
write.table(w4,file="./doc/TableS3_m52.csv",col.names = T,row.names = F,sep=",")

