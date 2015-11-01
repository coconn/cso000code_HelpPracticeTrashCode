# Owen-Justine-repeatedmeasuresANOVA.R
# 
# run a few models for Justine (these are the soil variables)
#
# CS O'Connell, UC-B ESPM, Silver Lab
#

########################################################################
# BRING IN DATA / PREP

library(ggplot2)
library(gridExtra)
library(plyr)
library(reshape2)
library(tidyr)
library(magrittr)
library(lubridate)
library(xlsx)
library(nlme)      ## for lme()
library(multcomp)  ## for multiple comparison stuff
#library(scales)
#library(data.table)

# where to save outputs
pathsavetab = "~/Documents/GITHUB/cso000code_HelpPracticeTrashCode/Owen-Justine-repeatedmeasuresANOVA/"
pathsavefigs = "~/Documents/GITHUB/cso000code_HelpPracticeTrashCode/Owen-Justine-repeatedmeasuresANOVA/"

# regr_table
source("~/Documents/GITHUB/RPersonalFunctionsChristine/regr_table.r")

# where excel file is
pathfile = "~/Documents/GITHUB/cso000code_HelpPracticeTrashCode/Owen-Justine-repeatedmeasuresANOVA/"

# bring in excel data for the non-nitrogen variables
#data <- read.xlsx(paste(pathfile,"JustineData.xlsx",sep=""),"data")
#data2 <- as.data.frame(data, stringsAsFactors=FALSE)

# bring in justinedata2 for the nitrogen variables
data <- read.xlsx(paste(pathfile,"JustineData2.xlsx",sep=""),"data")
data2 <- as.data.frame(data, stringsAsFactors=FALSE)

# rename cols
newnames <- c("sampletype","depth","rep","month","avgNpct","avgCpct","CNratio","pH","NH4_ugN_per_gsoil","NO3_ugN_per_gsoil","netnitrification_ugN_per_g_per_d","netmineralization_ugN_per_g_per_d")
names(data2) <- newnames


########################################################################
# WHAT DEPENDENT VARIABLE TO LOOK AT?

# just search and replace whatever variable is currently in use

# these are the variables:
# "netmineralization_ugN_per_g_per_d"
# DONE: "avgCpct","avgNpct","CNratio","pH","NH4_ugN_per_gsoil","NO3_ugN_per_gsoil","netnitrification_ugN_per_g_per_d",


########################################################################
# ONE-WAY REPEATED MEASURES ANOVA

# see these websites
#
# general how to do this
# http://www.r-bloggers.com/two-way-anova-with-repeated-measures/
# see http://ww2.coastal.edu/kingw/statistics/R-tutorials/repeated.html
# "in principle at least, we can see the "store" effect within each and every "subject" (grocery item)"
# aov.out = aov(price ~ store + Error(subject/store), data=groceries2)
#
# post hoc tests
# need to switch to lme package from aov()
# https://stats.stackexchange.com/questions/14078/post-hoc-test-after-anova-with-repeated-measures-using-r
# https://stats.stackexchange.com/questions/575/post-hocs-for-within-subjects-tests
# https://stat.ethz.ch/pipermail/r-help/2008-May/163433.html
# 
# what does error:within mean?
# see this example; looks like it's related to the sampletype pseudoreplication
# http://www.personality-project.org/r/r.anova.html
#

# lme can't handle columns with any NAs
data2_noNA <- subset(data2, !is.na(netmineralization_ugN_per_g_per_d))

#data2_noNA<-data2[is.na(data2$netmineralization_ugN_per_g_per_d)==F,]
#data2_noNA <- subset(data2_noNA, netmineralization_ugN_per_g_per_d>0.000001) # why isn't NH4 ugN per gsoil working

# aov version of the one-way test
aov_onewaysampletype = aov(netmineralization_ugN_per_g_per_d ~ sampletype + Error(month/(sampletype)), data=data2_noNA)
# lme version of the one-way test
lme_onewaysampletype = lme(netmineralization_ugN_per_g_per_d ~ sampletype, data=data2_noNA, random = ~1|month)

# summary info; note different F values (apparently this is a known consequence)
summary(aov_onewaysampletype)
anova(lme_onewaysampletype)

# post hoc test
posthoc_glht <- summary(glht(lme_onewaysampletype, linfct=mcp(sampletype = "Tukey")), test = adjusted(type = "bonferroni"))

# summary info
summary(posthoc_glht)

# save posthoc test as a nice table later
x <- posthoc_glht
pq<-summary(x)$test
mtests <- cbind(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues)
error <- attr(pq$pvalues, "error")
pname <- switch(x$alternativ, less = paste("Pr(<", ifelse(x$df ==0, "z", "t"), ")", sep = ""), greater = paste("Pr(>", ifelse(x$df == 0, "z", "t"), ")", sep = ""), two.sided = paste("Pr(>|",                                                                       ifelse(x$df == 0, "z", "t"), "|)", sep = ""))
colnames(mtests) <- c("Estimate", "Std. Error", ifelse(x$df ==0, "z value", "t value"), pname)

# save tables as single excel doc
# ignore saving the aov() since the formatting was a mess and we're ignoring it anyways
# lme() anova output
write.xlsx(anova(lme_onewaysampletype), file=paste(path.expand(pathsavetab), "stats-tables/netmineralization_ugN_per_g_per_d_onewayANOVA.xlsx",sep=""), sheetName="lme() table")
# post-hoc tukey with bonferroni
write.xlsx(mtests, file=paste(path.expand(pathsavetab), "stats-tables/netmineralization_ugN_per_g_per_d_onewayANOVA.xlsx",sep=""), sheetName="post-hoc tukey with bonferroni", append=TRUE)

# save diagnostic plots

# fitted vs residuals
png(file = paste(pathsavetab, "stats-tables/netmineralization_ugN_per_g_per_d_ANOVAdiagnostics_1.png", sep=""),width=6,height=6,units="in",res=150)
plot(lme_onewaysampletype,col=data2$sampletype, main="Diagnostic Trellis Plot")
dev.off()

# fitted vs sqrt(abs(residuals))
png(file = paste(pathsavetab, "stats-tables/netmineralization_ugN_per_g_per_d_ANOVAdiagnostics_2.png", sep=""),width=6,height=6,units="in",res=150)
scatter.smooth(fitted(lme_onewaysampletype),sqrt(abs(resid(lme_onewaysampletype))))
dev.off()

# qq plot
png(file = paste(pathsavetab, "stats-tables/netmineralization_ugN_per_g_per_d_ANOVAdiagnostics_3.png", sep=""),width=6,height=6,units="in",res=150)
qqnorm(lme_onewaysampletype,col=data2$sampletype, main="QQ Plot", abline=c(0,1))
dev.off()

# boxplot of residuals
png(file = paste(pathsavetab, "stats-tables/netmineralization_ugN_per_g_per_d_ANOVAdiagnostics_4.png", sep=""),width=6,height=6,units="in",res=150)
plot(lme_onewaysampletype,sampletype~resid(.))
dev.off()



########################################################################
# TWO-WAY REPEATED MEASURES ANOVA

# read these on lme() for two-way repeated measures
# http://www.researchgate.net/post/Has_anyone_performed_linear_mixed_model_with_repeated_measures
# http://www.jason-french.com/tutorials/repeatedmeasures.html

# lme can't handle columns with any NAs
data2_noNA <- subset(data2, !is.na(netmineralization_ugN_per_g_per_d))

# code depth as a factor
data2_noNA$depth <- factor(data2_noNA$depth)

# aov version of the one-way test
aov_twowaysampletype = aov(netmineralization_ugN_per_g_per_d ~ sampletype + depth + Error(month/(sampletype + depth)), data=data2_noNA)
# lme version of the one-way test
lme_twowaysampletype = lme(netmineralization_ugN_per_g_per_d ~ sampletype + depth, data=data2_noNA, random = ~1|month/depth/sampletype)

# summary info; note different F values (apparently this is a known consequence)
summary(aov_twowaysampletype)
anova(lme_twowaysampletype)

# post hoc test
posthoc_glht_1 <- summary(glht(lme_twowaysampletype, linfct=mcp(sampletype = "Tukey")), test = adjusted(type = "bonferroni"))
# this basically means nothing - see the individual depth one-way ANOVAs below
posthoc_glht_2 <- summary(glht(lme_twowaysampletype, linfct=mcp(depth = "Tukey")), test = adjusted(type = "bonferroni"))

# save posthoc test as a nice table later
x <- posthoc_glht_1
pq<-summary(x)$test
mtests <- cbind(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues)
error <- attr(pq$pvalues, "error")
pname <- switch(x$alternativ, less = paste("Pr(<", ifelse(x$df ==0, "z", "t"), ")", sep = ""), greater = paste("Pr(>", ifelse(x$df == 0, "z", "t"), ")", sep = ""), two.sided = paste("Pr(>|",                                                                       ifelse(x$df == 0, "z", "t"), "|)", sep = ""))
colnames(mtests) <- c("Estimate", "Std. Error", ifelse(x$df ==0, "z value", "t value"), pname)
mtests_1 <- mtests

# save posthoc test as a nice table later
x <- posthoc_glht_2
pq<-summary(x)$test
mtests <- cbind(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues)
error <- attr(pq$pvalues, "error")
pname <- switch(x$alternativ, less = paste("Pr(<", ifelse(x$df ==0, "z", "t"), ")", sep = ""), greater = paste("Pr(>", ifelse(x$df == 0, "z", "t"), ")", sep = ""), two.sided = paste("Pr(>|",                                                                       ifelse(x$df == 0, "z", "t"), "|)", sep = ""))
colnames(mtests) <- c("Estimate", "Std. Error", ifelse(x$df ==0, "z value", "t value"), pname)
mtests_2 <- mtests

# save tables as single excel doc
# ignore saving the aov() since the formatting was a mess and we're ignoring it anyways
# lme() anova output
write.xlsx(anova(lme_twowaysampletype), file=paste(path.expand(pathsavetab), "stats-tables/netmineralization_ugN_per_g_per_d_twowayANOVA.xlsx",sep=""), sheetName="lme() table")
# post-hoc tukey with bonferroni
write.xlsx(mtests_1, file=paste(path.expand(pathsavetab), "stats-tables/netmineralization_ugN_per_g_per_d_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey with bonf. IV #1", append=TRUE)
write.xlsx(mtests_2, file=paste(path.expand(pathsavetab), "stats-tables/netmineralization_ugN_per_g_per_d_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey with bonf. IV #2", append=TRUE)

# save diagnostic plots

# fitted vs residuals
png(file = paste(pathsavetab, "stats-tables/netmineralization_ugN_per_g_per_d_twowayANOVAdiagnostics_1.png", sep=""),width=6,height=6,units="in",res=150)
plot(lme_twowaysampletype,col=data2$sampletype, main="Diagnostic Trellis Plot")
dev.off()

# fitted vs sqrt(abs(residuals))
png(file = paste(pathsavetab, "stats-tables/netmineralization_ugN_per_g_per_d_twowayANOVAdiagnostics_2.png", sep=""),width=6,height=6,units="in",res=150)
scatter.smooth(fitted(lme_twowaysampletype),sqrt(abs(resid(lme_twowaysampletype))))
dev.off()

# qq plot
png(file = paste(pathsavetab, "stats-tables/netmineralization_ugN_per_g_per_d_twowayANOVAdiagnostics_3.png", sep=""),width=6,height=6,units="in",res=150)
qqnorm(lme_twowaysampletype,col=data2$sampletype, main="QQ Plot", abline=c(0,1))
dev.off()

# boxplot of residuals
png(file = paste(pathsavetab, "stats-tables/netmineralization_ugN_per_g_per_d_twowayANOVAdiagnostics_4.png", sep=""),width=6,height=6,units="in",res=150)
plot(lme_twowaysampletype,sampletype~resid(.))
dev.off()

# boxplot of residuals
png(file = paste(pathsavetab, "stats-tables/netmineralization_ugN_per_g_per_d_twowayANOVAdiagnostics_5.png", sep=""),width=6,height=6,units="in",res=150)
plot(lme_twowaysampletype,depth~resid(.))
dev.off()


########################################################################
# LOOK AT DEPTH WITHIN PILE SUBSET

# get the sampletype subsets
data2_pile <- subset(data2, sampletype=="pile")

# code depth as a factor
data2_pile$depth <- factor(data2_pile$depth)

# get rid of NAs
data2_pile <- subset(data2_pile, !is.na(netmineralization_ugN_per_g_per_d))

## pile test

# aov version of the one-way test
aov_onewaydepth = aov(netmineralization_ugN_per_g_per_d ~ depth + Error(month/(depth)), data=data2_pile)
# lme version of the one-way test
lme_onewaydepth = lme(netmineralization_ugN_per_g_per_d ~ depth, data=data2_pile, random = ~1|month)

# summary info; note different F values (apparently this is a known consequence)
summary(aov_onewaydepth)
anova(lme_onewaydepth)

# post hoc test - don't do, since anova wasn't significant
posthoc_glht <- summary(glht(lme_onewaydepth, linfct=mcp(depth = "Tukey")), test = adjusted(type = "bonferroni"))

# save posthoc test as a nice table later
x <- posthoc_glht
pq<-summary(x)$test
mtests <- cbind(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues)
error <- attr(pq$pvalues, "error")
pname <- switch(x$alternativ, less = paste("Pr(<", ifelse(x$df ==0, "z", "t"), ")", sep = ""), greater = paste("Pr(>", ifelse(x$df == 0, "z", "t"), ")", sep = ""), two.sided = paste("Pr(>|",                                                                       ifelse(x$df == 0, "z", "t"), "|)", sep = ""))
colnames(mtests) <- c("Estimate", "Std. Error", ifelse(x$df ==0, "z value", "t value"), pname)

# save tables as single excel doc
# ignore saving the aov() since the formatting was a mess and we're ignoring it anyways
# lme() anova output
write.xlsx(anova(lme_onewaydepth), file=paste(path.expand(pathsavetab), "stats-tables/netmineralization_ugN_per_g_per_d_depthpileonewayANOVA.xlsx",sep=""), sheetName="lme() table")
# post-hoc tukey with bonferroni
write.xlsx(mtests, file=paste(path.expand(pathsavetab), "stats-tables/netmineralization_ugN_per_g_per_d_depthpileonewayANOVA.xlsx",sep=""), sheetName="post-hoc tukey with bonferroni", append=TRUE)

# save diagnostic plots

# fitted vs residuals
png(file = paste(pathsavetab, "stats-tables/netmineralization_ugN_per_g_per_d_depthpileonewayANOVAdiagnostics_1.png", sep=""),width=6,height=6,units="in",res=150)
plot(lme_onewaydepth,col=data2_pile$depth, main="Diagnostic Trellis Plot")
dev.off()

# fitted vs sqrt(abs(residuals))
png(file = paste(pathsavetab, "stats-tables/netmineralization_ugN_per_g_per_d_depthpileonewayANOVAdiagnostics_2.png", sep=""),width=6,height=6,units="in",res=150)
scatter.smooth(fitted(lme_onewaydepth),sqrt(abs(resid(lme_onewaydepth))))
dev.off()

# qq plot
png(file = paste(pathsavetab, "stats-tables/netmineralization_ugN_per_g_per_d_depthpileonewayANOVAdiagnostics_3.png", sep=""),width=6,height=6,units="in",res=150)
qqnorm(lme_onewaydepth,col=data2_pile$depth, main="QQ Plot", abline=c(0,1))
dev.off()

# boxplot of residuals
png(file = paste(pathsavetab, "stats-tables/netmineralization_ugN_per_g_per_d_depthpileonewayANOVAdiagnostics_4.png", sep=""),width=6,height=6,units="in",res=150)
plot(lme_onewaydepth,depth~resid(.))
dev.off()



########################################################################
# LOOK AT DEPTH WITHIN FIELD SUBSET

# get the sampletype subsets
data2_field <- subset(data2, sampletype=="field")

# code depth as a factor
data2_field$depth <- factor(data2_field$depth)

# get rid of NAs
data2_field <- subset(data2_field, !is.na(netmineralization_ugN_per_g_per_d))

## field test

# aov version of the one-way test
aov_onewaydepth = aov(netmineralization_ugN_per_g_per_d ~ depth + Error(month/(depth)), data=data2_field)
# lme version of the one-way test
lme_onewaydepth = lme(netmineralization_ugN_per_g_per_d ~ depth, data=data2_field, random = ~1|month)

# summary info; note different F values (apparently this is a known consequence)
summary(aov_onewaydepth)
anova(lme_onewaydepth)

# post hoc test - don't do, since anova wasn't significant
summary(glht(lme_onewaydepth, linfct=mcp(depth = "Tukey")), test = adjusted(type = "bonferroni"))

# save posthoc test as a nice table later
x <- posthoc_glht
pq<-summary(x)$test
mtests <- cbind(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues)
error <- attr(pq$pvalues, "error")
pname <- switch(x$alternativ, less = paste("Pr(<", ifelse(x$df ==0, "z", "t"), ")", sep = ""), greater = paste("Pr(>", ifelse(x$df == 0, "z", "t"), ")", sep = ""), two.sided = paste("Pr(>|",                                                                       ifelse(x$df == 0, "z", "t"), "|)", sep = ""))
colnames(mtests) <- c("Estimate", "Std. Error", ifelse(x$df ==0, "z value", "t value"), pname)

# save tables as single excel doc
# ignore saving the aov() since the formatting was a mess and we're ignoring it anyways
# lme() anova output
write.xlsx(anova(lme_onewaydepth), file=paste(path.expand(pathsavetab), "stats-tables/netmineralization_ugN_per_g_per_d_depthfieldonewayANOVA.xlsx",sep=""), sheetName="lme() table")
# post-hoc tukey with bonferroni
write.xlsx(mtests, file=paste(path.expand(pathsavetab), "stats-tables/netmineralization_ugN_per_g_per_d_depthfieldonewayANOVA.xlsx",sep=""), sheetName="post-hoc tukey with bonferroni", append=TRUE)

# save diagnostic plots

# fitted vs residuals
png(file = paste(pathsavetab, "stats-tables/netmineralization_ugN_per_g_per_d_depthfieldonewayANOVAdiagnostics_1.png", sep=""),width=6,height=6,units="in",res=150)
plot(lme_onewaydepth,col=data2_field$depth, main="Diagnostic Trellis Plot")
dev.off()

# fitted vs sqrt(abs(residuals))
png(file = paste(pathsavetab, "stats-tables/netmineralization_ugN_per_g_per_d_depthfieldonewayANOVAdiagnostics_2.png", sep=""),width=6,height=6,units="in",res=150)
scatter.smooth(fitted(lme_onewaydepth),sqrt(abs(resid(lme_onewaydepth))))
dev.off()

# qq plot
png(file = paste(pathsavetab, "stats-tables/netmineralization_ugN_per_g_per_d_depthfieldonewayANOVAdiagnostics_3.png", sep=""),width=6,height=6,units="in",res=150)
qqnorm(lme_onewaydepth,col=data2_field$depth, main="QQ Plot", abline=c(0,1))
dev.off()

# boxplot of residuals
png(file = paste(pathsavetab, "stats-tables/netmineralization_ugN_per_g_per_d_depthfieldonewayANOVAdiagnostics_4.png", sep=""),width=6,height=6,units="in",res=150)
plot(lme_onewaydepth,depth~resid(.))
dev.off()





########################################################################
# NOTES AND TESTING





# random code if I need to use chunks of this

# # bring in stuff
# pitTDRsummary <- read.csv("~/Documents/GITHUB/cso038code_TanguroPitGas/Pit-Data-Rprocessed/pitTDRsummarytable.csv", stringsAsFactors=FALSE)
# 
# pitgassummary <- read.csv("~/Documents/GITHUB/cso038code_TanguroPitGas/Pit-Data-Rprocessed/pitgassummary.csv", stringsAsFactors=FALSE)
# 
# # get rid of rows with NA in sample depth
# pitTDRsummary <- subset(pitTDRsummary, !is.na(pitTDRsummary$sampledepth))
# 
# # get rid of VW for C2, Jan 2015, since it looks like the sensors are on the fritz
# pitTDRsummary <- pitTDRsummary[!(pitTDRsummary$PitID=="C2" & pitTDRsummary$Month=="Jan" & pitTDRsummary$DataType=="VW"),]
# 
# # get rid of November month info
# pitTDRsummary <- pitTDRsummary[!(pitTDRsummary$Month=="Nov"),]
# 
# 

