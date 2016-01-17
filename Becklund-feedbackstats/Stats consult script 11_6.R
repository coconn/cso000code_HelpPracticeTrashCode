###STATS CONSULT 11/6

rm(list=ls())

#rm only clears global env.
#if you have a .Rdata file in the folder where your workspace is, R will load that everytime you reopen R with a file in that folder, even after you quit.  sometimes the .Rdata files can be difficult to see..

#searches other places R puts things (besides global env)
search()

#will give you list of all variables and type of each
str(data2)

library(nlme)
library(car)
library(multcomp)
library(xls) #??

setwd("/Users/kristenbecklund/Desktop/Chapter3/Seedling data")
data <- read.csv("FB_MasterR_Nov1.csv")

#droplevels makes it so that it forgets about the things you are excluding forever
data1 <- droplevels(data[!data$SEEDLING %in%  c("am"),])
data2 <- droplevels(data1[!data1$SOIL %in%  c("C","AM","TG"),])

##but that meanas it doesn't calculate PCs for aa!! redo

simplelm <- lme((logBiomassH)~0+SOIL:SEEDLING,random=~1|TREE, data2)
summary(simplelm)
#focus on first block of output when model includes random effect. ignore the correlations below

##"^2" means it does all the 2way interactions
simplelm2 <- lme((logBiomassH)~(SOIL+SEEDLING+Biomass0)^2,random=~1|TREE, data2)
Anova(simplelm2)

#gives you means for each combo (but i think the SEs are wrong here...)

## lsmeans package?

library(multcomp)
## matrix with column giving coefficient for each parameter in model
## can have multiple rows for multiple contrasts

simplelm <- lme((logBiomassH)~0+SOIL:SEEDLING,random=~1|TREE, data2)
summary(simplelm)

## say I wanted mean om in others
#if you want spaces in the label, must use back quotes (i think that's what they meant)
K1 <- rbind(`om in om`=c(1,0,0,0,0,0,0,0,0))
K2 <- rbind(`mean om in other`=c(0,1,1,0,0,0,0,0,0)/2)
K3 <- rbind(`mean other in om`=c(0,0,0,1,0,0,1,0,0)/2)
K4 <- rbind(`mean other in other`=c(0,0,0,0,1,1,0,1,1)/4)
K <- rbind(K1, K2, K3, K4)
g <- glht(simplelm, linfct=K)

summary(g)
#this gives the estimates and standard error for each of the components in fb calculation for om

Kx <- (K1-K2)-(K3-K4)
rownames(Kx) <- "om feedback"
gx <- glht(simplelm, linfct=Kx)
summary(gx)
##gives you om fb!  se here is correct

##calculate fb for VIROLA
## say I wanted mean vs in others
#if you want spaces in the label, must use back quotes (i think that's what they meant)
K1_vs <- rbind(`vs in vs`=c(0,0,0,0,0,0,0,0,1))
K2_vs <- rbind(`mean vs in other`=c(0,0,0,0,0,0,1,1,0)/2)
K3_vs <- rbind(`mean other in vs`=c(0,0,1,0,0,1,0,0,0)/2)
K4_vs <- rbind(`mean other in other`=c(1,1,0,1,1,0,0,0,0)/4)
K_vs <- rbind(K1_vs, K2_vs, K3_vs, K4_vs)
g_vs <- glht(simplelm, linfct=K_vs)

summary(g_vs)
#this gives the estimates and standard error for each of the components in fb calculation for om

Kx_vs <- (K1_vs-K2_vs)-(K3_vs-K4_vs)
rownames(Kx_vs) <- "vs feedback"
gx_vs <- glht(simplelm, linfct=Kx_vs)
summary(gx_vs)
##gives you vs fb!  se here is correct


##calculate fb for PROTIUM

K1_pt <- rbind(`pt in pt`=c(0,0,0,0,1,0,0,0,0))
K2_pt <- rbind(`mean pt in other`=c(0,0,0,1,0,1,0,0,0)/2)
K3_pt <- rbind(`mean other in pt`=c(0,1,0,0,0,0,0,1,0)/2)
K4_pt <- rbind(`mean other in other`=c(1,0,1,0,0,0,1,0,1)/4)
K_pt <- rbind(K1_pt, K2_pt, K3_pt, K4_pt)
g_pt <- glht(simplelm, linfct=K_pt)

summary(g_pt)
#this gives the estimates and standard error for each of the components in fb calculation for pt

Kx_pt <- (K1_pt-K2_pt)-(K3_pt-K4_pt)
rownames(Kx_pt) <- "pt feedback"
gx_pt <- glht(simplelm, linfct=Kx_pt)
summary(gx_pt)
##gives you pt fb!  se here is correct




##now try PAIRWISE fbs and/or interaction plots




##adding in initial biomass as covariate

summary(data2$Biomass0)
#gives you summary stats for initial biomass

##as additive covariate (this should only shift the values, but not change result)

simplelm2 <- lme((logBiomassH)~0+SOIL:SEEDLING+Biomass0,random=~1|TREE, data2)
summary(simplelm2)

simplelm2 <- lme((logBiomassH)~0+SOIL*SEEDLING+Biomass0,random=~1|TREE, data2)

#check fbs with Biomass0 covariate (NOT THE SAME)
#had to add in extra 0 at front of c() to account for biomass0 adding a row to output in summary(simplelm2)
#but how to look at the model stats?  above doesn't include all terms when run Anova(simplelm2)

#om fb...must run simplelm2 and summary lines above FIRST
K1 <- rbind(`om in om`=c(0,1,0,0,0,0,0,0,0,0))
K2 <- rbind(`mean om in other`=c(0,0,1,1,0,0,0,0,0,0)/2)
K3 <- rbind(`mean other in om`=c(0,0,0,0,1,0,0,1,0,0)/2)
K4 <- rbind(`mean other in other`=c(0,0,0,0,0,1,1,0,1,1)/4)
K <- rbind(K1, K2, K3, K4)
g <- glht(simplelm2, linfct=K)

summary(g)
#this gives the estimates and standard error for each of the components in fb calculation for om

Kx <- (K1-K2)-(K3-K4)
rownames(Kx) <- "om feedback"
gx <- glht(simplelm2, linfct=Kx)
summary(gx)
##gives you om fb!  se here is correct


##calculate fb for VIROLA

K1_vs <- rbind(`vs in vs`=c(0,0,0,0,0,0,0,0,0,1))
K2_vs <- rbind(`mean vs in other`=c(0,0,0,0,0,0,0,1,1,0)/2)
K3_vs <- rbind(`mean other in vs`=c(0,0,0,1,0,0,1,0,0,0)/2)
K4_vs <- rbind(`mean other in other`=c(0,1,1,0,1,1,0,0,0,0)/4)
K_vs <- rbind(K1_vs, K2_vs, K3_vs, K4_vs)
g_vs <- glht(simplelm2, linfct=K_vs)

summary(g_vs)
#this gives the estimates and standard error for each of the components in fb calculation for om

Kx_vs <- (K1_vs-K2_vs)-(K3_vs-K4_vs)
rownames(Kx_vs) <- "vs feedback"
gx_vs <- glht(simplelm2, linfct=Kx_vs)
summary(gx_vs)
##gives you vs fb!  se here is correct

##calculate fb for PROTIUM

K1_pt <- rbind(`pt in pt`=c(0,0,0,0,0,1,0,0,0,0))
K2_pt <- rbind(`mean pt in other`=c(0,0,0,0,1,0,1,0,0,0)/2)
K3_pt <- rbind(`mean other in pt`=c(0,0,1,0,0,0,0,0,1,0)/2)
K4_pt <- rbind(`mean other in other`=c(0,1,0,1,0,0,0,1,0,1)/4)
K_pt <- rbind(K1_pt, K2_pt, K3_pt, K4_pt)
g_pt <- glht(simplelm2, linfct=K_pt)

summary(g_pt)
#this gives the estimates and standard error for each of the components in fb calculation for pt

Kx_pt <- (K1_pt-K2_pt)-(K3_pt-K4_pt)
rownames(Kx_pt) <- "pt feedback"
gx_pt <- glht(simplelm2, linfct=Kx_pt)
summary(gx_pt)
##gives you pt fb!  se here is correct




#as interactive covariate (will change result)
simplelm2 <- lme((logBiomassH)~0+SOIL:SEEDLING+Biomass0*SOIL,random=~1|TREE, data2)
summary(simplelm2)
Anova(simplelm2)

#check fbs with Biomass0 covariate with INTERACTIONS

#om fb...must run simplelm2 and summary lines above FIRST
K1 <- rbind(`om in om`=c(0,1,0,0,0,0,0,0,0,0))
K2 <- rbind(`mean om in other`=c(0,0,1,1,0,0,0,0,0,0)/2)
K3 <- rbind(`mean other in om`=c(0,0,0,0,1,0,0,1,0,0)/2)
K4 <- rbind(`mean other in other`=c(0,0,0,0,0,1,1,0,1,1)/4)
K <- rbind(K1, K2, K3, K4)
g <- glht(simplelm2, linfct=K)

summary(g)
#this gives the estimates and standard error for each of the components in fb calculation for om

Kx <- (K1-K2)-(K3-K4)
rownames(Kx) <- "om feedback"
gx <- glht(simplelm2, linfct=Kx)
summary(gx)
##gives you om fb!  se here is correct
##same result as additive covariate (maybe bc there isn't actually a significant interaction...see "Anova(simplelm2))


##2way vs 3way interactions with covariate (##SHOW THESE RESULTS TO ADVISORS!  CODE AND OUTPUT!)
> simplelm2 <- lme((logBiomassH)~SOIL*SEEDLING+Biomass0*SOIL,random=~1|TREE, data2)
> Anova(simplelm2)

#all 2way interactions (that's what ^2 does)
> simplelm2 <- lme((logBiomassH)~(SOIL+SEEDLING+Biomass0)^2,random=~1|TREE, data2)
> Anova(simplelm2)

#3-way interaction
> simplelm2 <- lme((logBiomassH)~SOIL*SEEDLING*Biomass0,random=~1|TREE, data2)
> Anova(simplelm2)
#stats ppl didn't think the marginally significant Biomass0*SEEDLING interaction is real...something about too many interactions in model or something...it goees away when you just look at all 2way interactions


#look at relationship between initial and final biomass by soil or seedling
library(ggplot2)
ggplot(data2) + aes(Biomass0, logBiomassH, color=SEEDLING) + geom_point() + geom_smooth(method="lm")


#PCAs
#Strep vars

Strep<-subset(data2, select = Strep:ZoneB)
Strep_scaled<-scale(Strep)

pca_Strep<-prcomp(Strep_scaled, scale=T) #gives error bc of NAs

pca_Strep<-prcomp(na.omit(Strep_scaled), scale=T) 
#should center = TRUE?  #i think default for center is true...check!
 summary(pca_Strep) #first two axes explain 89% variation
 pca_Strep$rotation
 screeplot(pca_Strep, type="lines")
 biplot(pca_Strep)
 
#try with na.exclude so not to omit rows with missing data for antB
pca_Strep<-prcomp(na.exclude(Strep_scaled), scale=T)
 
 axes_Strep<-predict(pca_Strep, newdata=data2)
 head(axes_Strep,)
 data2<-cbind(data2, axes_Strep)
 #but what we really want is a dataset with both RT and Strep axes...
 
 axes_Strep<-predict(pca_Strep, newdata=dat_pca)
 head(axes_Strep,)
 dat_pcaS<-cbind(dat_pca, axes_Strep)
 #now dataset dat_pca has both but they're labeled the same (second grouping is from Herr's data)
 
 #relabel Strep pca col headings
 data2<-rename(data2,c("PC1"="PC1_Strep", "PC2"="PC2_Strep", "PC3"="PC3_Strep"))
 
 #RT vars
RT<-subset(data2, select = severity_AA:index_OP)
RT_scaled<-scale(RT)
pca_RT<-prcomp(RT_scaled, scale=T)
summary(pca_RT)
pca_RT$rotation
screeplot(pca_RT, type="lines")
biplot(pca_RT)


#ADD RT PCA axes to datset
axes<-predict(pca_RT, newdata=data2)
head(axes,)
data2<-cbind(data2,axes)

##export dataset to excel
write.xlsx(data2, "FB_withPCs.xls", col.names=TRUE)
#then it just shows up in your workspace folder


#add PCs to models

##PC_Strep

simplelm2 <- lme((logBiomassH)~SOIL*SEEDLING+PC1_Strep,random=~1|TREE, data2, na.action=na.exclude)
#PC1_Strep is sig main effect (same result if you use na.omit)
simplelm3 <- lme((logBiomassH)~SOIL*SEEDLING+PC2_Strep,random=~1|TREE, data2, na.action=na.exclude)
#PC2_Strep ns
Anova(simplelm2)

simplelm4 <- lme((logBiomassH)~SOIL*SEEDLING+PC2+PC1,random=~1|TREE, dat_pca)
simplelm5 <- lme((logBiomassH)~SOIL*SEEDLING*PC1,random=~1|TREE, dat_pca) #reduces SEEDLING by a lot
#also reduces soil*seedling (no longer sig!)

simplelm2 <- lme((logBiomassH)~SOIL*SEEDLING*PC2,random=~1|TREE, dat_pca)
#reduces SEEDLING by a lot, also reduces soil*seedling

simplelm2 <- lme((logBiomassH)~(SOIL+SEEDLING+PC1)^2,random=~1|TREE, dat_pca)
Anova(simplelm2)


#Fbs with PCA vars added (to base model)
simplelm2 <- lme((logBiomassH)~0+SOIL:SEEDLING+PC1_Strep,random=~1|TREE, data2, na.action=na.exclude)
summary(simplelm2)
Anova(simplelm2)

#om fb...must run simplelm2 and summary lines above FIRST
K1 <- rbind(`om in om`=c(0,1,0,0,0,0,0,0,0,0))
K2 <- rbind(`mean om in other`=c(0,0,1,1,0,0,0,0,0,0)/2)
K3 <- rbind(`mean other in om`=c(0,0,0,0,1,0,0,1,0,0)/2)
K4 <- rbind(`mean other in other`=c(0,0,0,0,0,1,1,0,1,1)/4)
K <- rbind(K1, K2, K3, K4)
g <- glht(simplelm2, linfct=K)

summary(g)
#this gives the estimates and standard error for each of the components in fb calculation for om

Kx <- (K1-K2)-(K3-K4)
rownames(Kx) <- "om feedback"
gx <- glht(simplelm2, linfct=Kx)
summary(gx)
##gives you om fb!  se here is correct

or for RT

simplelm2 <- lme((logBiomassH)~0+SOIL:SEEDLING+PC1,random=~1|TREE, data2, na.action=na.omit)
summary(simplelm2)

#om fb...must run simplelm2 and summary lines above FIRST
K1 <- rbind(`om in om`=c(0,1,0,0,0,0,0,0,0,0))
K2 <- rbind(`mean om in other`=c(0,0,1,1,0,0,0,0,0,0)/2)
K3 <- rbind(`mean other in om`=c(0,0,0,0,1,0,0,1,0,0)/2)
K4 <- rbind(`mean other in other`=c(0,0,0,0,0,1,1,0,1,1)/4)
K <- rbind(K1, K2, K3, K4)
g <- glht(simplelm2, linfct=K)

summary(g)
#this gives the estimates and standard error for each of the components in fb calculation for om

Kx <- (K1-K2)-(K3-K4)
rownames(Kx) <- "om feedback"
gx <- glht(simplelm2, linfct=Kx)
summary(gx)
##gives you om fb!  se here is correct


##calculate fb for VIROLA

K1_vs <- rbind(`vs in vs`=c(0,0,0,0,0,0,0,0,0,1))
K2_vs <- rbind(`mean vs in other`=c(0,0,0,0,0,0,0,1,1,0)/2)
K3_vs <- rbind(`mean other in vs`=c(0,0,0,1,0,0,1,0,0,0)/2)
K4_vs <- rbind(`mean other in other`=c(0,1,1,0,1,1,0,0,0,0)/4)
K_vs <- rbind(K1_vs, K2_vs, K3_vs, K4_vs)
g_vs <- glht(simplelm2, linfct=K_vs)

summary(g_vs)
#this gives the estimates and standard error for each of the components in fb calculation for om

Kx_vs <- (K1_vs-K2_vs)-(K3_vs-K4_vs)
rownames(Kx_vs) <- "vs feedback"
gx_vs <- glht(simplelm2, linfct=Kx_vs)
summary(gx_vs)
##gives you vs fb!  se here is correct

##calculate fb for PROTIUM

K1_pt <- rbind(`pt in pt`=c(0,0,0,0,0,1,0,0,0,0))
K2_pt <- rbind(`mean pt in other`=c(0,0,0,0,1,0,1,0,0,0)/2)
K3_pt <- rbind(`mean other in pt`=c(0,0,1,0,0,0,0,0,1,0)/2)
K4_pt <- rbind(`mean other in other`=c(0,1,0,1,0,0,0,1,0,1)/4)
K_pt <- rbind(K1_pt, K2_pt, K3_pt, K4_pt)
g_pt <- glht(simplelm2, linfct=K_pt)

summary(g_pt)
#this gives the estimates and standard error for each of the components in fb calculation for pt

Kx_pt <- (K1_pt-K2_pt)-(K3_pt-K4_pt)
rownames(Kx_pt) <- "pt feedback"
gx_pt <- glht(simplelm2, linfct=Kx_pt)
summary(gx_pt)
##gives you pt fb!  se here is correct


#now test for two RT PCs added

simplelm2 <- lme((logBiomassH)~0+SOIL:SEEDLING+PC1+PC2,random=~1|TREE, data2, na.action=na.omit)
summary(simplelm2)

#om fb...must run simplelm2 and summary lines above FIRST
K1 <- rbind(`om in om`=c(0,0,1,0,0,0,0,0,0,0,0))
K2 <- rbind(`mean om in other`=c(0,0,0,1,1,0,0,0,0,0,0)/2)
K3 <- rbind(`mean other in om`=c(0,0,0,0,0,1,0,0,1,0,0)/2)
K4 <- rbind(`mean other in other`=c(0,0,0,0,0,0,1,1,0,1,1)/4)
K <- rbind(K1, K2, K3, K4)
g <- glht(simplelm2, linfct=K)

summary(g)
#this gives the estimates and standard error for each of the components in fb calculation for om

Kx <- (K1-K2)-(K3-K4)
rownames(Kx) <- "om feedback"
gx <- glht(simplelm2, linfct=Kx)
summary(gx)


##NOW test for fbs when including AA and aa

data3 <- droplevels(data[!data$SOIL %in%  c("C","TG"),])
str(data3)

#change model and dataset below!
simplelm3 <- lme((logBiomassH)~0+SOIL:SEEDLING,random=~1|TREE, data3)
summary(simplelm3)

#change number strings and data/model names below

## say I wanted mean om in others
#if you want spaces in the label, must use back quotes (i think that's what they meant)
K1 <- rbind(`om in om`=c(0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0))
K2 <- rbind(`mean om in other`=c(0,0,0,0,1,0,1,1,0,0,0,0,0,0,0,0)/3)
K3 <- rbind(`mean other in om`=c(0,1,0,0,0,0,0,0,0,1,0,0,0,1,0,0)/3)
K4 <- rbind(`mean other in other`=c(1,0,1,1,0,0,0,0,1,0,1,1,1,0,1,1)/9)
K <- rbind(K1, K2, K3, K4)
g <- glht(simplelm3, linfct=K)

summary(g)
#this gives the estimates and standard error for each of the components in fb calculation for om

Kx <- (K1-K2)-(K3-K4)
rownames(Kx) <- "om feedback"
gx <- glht(simplelm3, linfct=Kx)
summary(gx)
##gives you om fb!  se here is correct

##calculate fb for VIROLA
## say I wanted mean vs in others
K1_vs <- rbind(`vs in vs`=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1))
K2_vs <- rbind(`mean vs in other`=c(0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0)/3)
K3_vs <- rbind(`mean other in vs`=c(0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0)/3)
K4_vs <- rbind(`mean other in other`=c(1,1,1,0,1,1,1,0,1,1,1,0,0,0,0,0)/9)
K_vs <- rbind(K1_vs, K2_vs, K3_vs, K4_vs)
g_vs <- glht(simplelm3, linfct=K_vs)

summary(g_vs)
#this gives the estimates and standard error for each of the components in fb calculation for vs

Kx_vs <- (K1_vs-K2_vs)-(K3_vs-K4_vs)
rownames(Kx_vs) <- "vs feedback"
gx_vs <- glht(simplelm3, linfct=Kx_vs)
summary(gx_vs)
##gives you vs fb!  se here is correct

##calculate fb for PROTIUM

K1_pt <- rbind(`pt in pt`=c(0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0))
K2_pt <- rbind(`mean pt in other`=c(0,0,0,0,0,0,0,0,1,1,0,1,0,0,0,0)/3)
K3_pt <- rbind(`mean other in pt`=c(0,0,1,0,0,0,1,0,0,0,0,0,0,0,1,0)/3)
K4_pt <- rbind(`mean other in other`=c(1,1,0,1,1,1,0,1,0,0,0,0,1,1,0,1)/9)
K_pt <- rbind(K1_pt, K2_pt, K3_pt, K4_pt)
g_pt <- glht(simplelm3, linfct=K_pt)

summary(g_pt)
#this gives the estimates and standard error for each of the components in fb calculation for pt

Kx_pt <- (K1_pt-K2_pt)-(K3_pt-K4_pt)
rownames(Kx_pt) <- "pt feedback"
gx_pt <- glht(simplelm3, linfct=Kx_pt)
summary(gx_pt)
##gives you pt fb!  se here is correct

##calculate fb for APEIBA 

K1_aa <- rbind(`aa in aa`=c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
K2_aa <- rbind(`mean aa in other`=c(0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0)/3)
K3_aa <- rbind(`mean other in aa`=c(0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0)/3)
K4_aa <- rbind(`mean other in other`=c(0,0,0,0,0,1,1,1,0,1,1,1,0,1,1,1)/9)
K_aa <- rbind(K1_aa, K2_aa, K3_aa, K4_aa)
g_aa <- glht(simplelm3, linfct=K_aa)

summary(g_aa)
#this gives the estimates and standard error for each of the components in fb calculation for aa

Kx_aa <- (K1_aa-K2_aa)-(K3_aa-K4_aa)
rownames(Kx_aa) <- "aa feedback"
gx_aa <- glht(simplelm3, linfct=Kx_aa)
summary(gx_aa)
##gives you aa fb!  se here is correct





#check effect of Strep on fbs!

simplelm2 <- lme((logBiomassH)~0+SOIL:SEEDLING+Strep,random=~1|TREE, data2)
summary(simplelm2)

## say I wanted mean om in others
#if you want spaces in the label, must use back quotes (i think that's what they meant)
K1 <- rbind(`om in om`=c(0,1,0,0,0,0,0,0,0,0))
K2 <- rbind(`mean om in other`=c(0,0,1,1,0,0,0,0,0,0)/2)
K3 <- rbind(`mean other in om`=c(0,0,0,0,1,0,0,1,0,0)/2)
K4 <- rbind(`mean other in other`=c(0,0,0,0,0,1,1,0,1,1)/4)
K <- rbind(K1, K2, K3, K4)
g <- glht(simplelm2, linfct=K)

summary(g)
#this gives the estimates and standard error for each of the components in fb calculation for om

Kx <- (K1-K2)-(K3-K4)
rownames(Kx) <- "om feedback"
gx <- glht(simplelm2, linfct=Kx)
summary(gx)
##gives you om fb!  se here is correct

##stats ppl said compare estimates, not p values.  does that apply to nonsignificant fb estimates too?


##calculate fb for VIROLA
## say I wanted mean vs in others
#if you want spaces in the label, must use back quotes (i think that's what they meant)
K1_vs <- rbind(`vs in vs`=c(0,0,0,0,0,0,0,0,0,1))
K2_vs <- rbind(`mean vs in other`=c(0,0,0,0,0,0,0,1,1,0)/2)
K3_vs <- rbind(`mean other in vs`=c(0,0,0,1,0,0,1,0,0,0)/2)
K4_vs <- rbind(`mean other in other`=c(0,1,1,0,1,1,0,0,0,0)/4)
K_vs <- rbind(K1_vs, K2_vs, K3_vs, K4_vs)
g_vs <- glht(simplelm2, linfct=K_vs)

summary(g_vs)
#this gives the estimates and standard error for each of the components in fb calculation for om

Kx_vs <- (K1_vs-K2_vs)-(K3_vs-K4_vs)
rownames(Kx_vs) <- "vs feedback"
gx_vs <- glht(simplelm2, linfct=Kx_vs)
summary(gx_vs)
##gives you vs fb!  se here is correct

##calculate fb for PROTIUM

K1_pt <- rbind(`pt in pt`=c(0,0,0,0,0,1,0,0,0,0))
K2_pt <- rbind(`mean pt in other`=c(0,0,0,0,1,0,1,0,0,0)/2)
K3_pt <- rbind(`mean other in pt`=c(0,0,1,0,0,0,0,0,1,0)/2)
K4_pt <- rbind(`mean other in other`=c(0,1,0,1,0,0,0,1,0,1)/4)
K_pt <- rbind(K1_pt, K2_pt, K3_pt, K4_pt)
g_pt <- glht(simplelm2, linfct=K_pt)

summary(g_pt)
#this gives the estimates and standard error for each of the components in fb calculation for pt

Kx_pt <- (K1_pt-K2_pt)-(K3_pt-K4_pt)
rownames(Kx_pt) <- "pt feedback"
gx_pt <- glht(simplelm2, linfct=Kx_pt)
summary(gx_pt)
##gives you pt fb!  se here is correct

##PCAs

#for Strep vars


