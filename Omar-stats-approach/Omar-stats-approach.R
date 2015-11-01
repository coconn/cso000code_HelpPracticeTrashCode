# Omar-stats-approach.R
# 
# figure out how to approach first year project data with Omar
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
pathsavetab = "~/Documents/GITHUB/cso000code_HelpPracticeTrashCode/Omar-stats-approach/"
pathsavefigs = "~/Documents/GITHUB/cso000code_HelpPracticeTrashCode/Omar-stats-approach/"

# where excel file is
pathfile = "~/Documents/GITHUB/cso000code_HelpPracticeTrashCode/Omar-stats-approach/"

# bring in Omar's data file
df <- read.csv(paste(pathfile,"CTEDepthBiogeochemistry.csv",sep=""), stringsAsFactor=F)

# make certain vars into factors
df$Block <- as.factor(df$Block)
df$Treatment <- as.factor(df$Treatment)
df$Depth <- as.factor(df$Depth)


########################################################################
# EXPERIMENTAL DESIGN INFO

# From Omar:
# I'm attaching xls and csv versions of my dataset, which follows a complete randomized block design (three blocks and four treatments; n=3). My goal through these analyses is to determine the depth and treatment effects for all of the soil variables we measured (i.e., pH, moisture, C, N, P fractions, etc.), as well as doing regressions among several variables (i.e., C vs. Fe, C vs. P, Fe vs. P, etc.).

# http://www.stat.wisc.edu/~ane/st572/notes/lec24.pdf
# Model: response ∼ treatment + block + error
# this is also the model proposed here: http://www.r-tutor.com/elementary-statistics/analysis-variance/randomized-block-design

# Debate: fixed vs. random block effects?
# The lec24.pdf pdf suggests keeping the block effects random

# writes R code as per:
# fit.lm  = lm(emergence ~ block + treatment, data=emerge) # note that block + treatment order doesn't matter
# anova(fit.lm)
# and from http://web.udl.es/Biomath/Bioestadistica/Dossiers/Temas%20especiales/ANOVA/The%20randomized%20complete%20block%20design%20(R).pdf
# plot tukey:
# plot(TukeyHSD(fit.lm,"treatment"))

# how to deal with the fact that this is a two-way ANOVA with depth

# is there actually a third variable in time period?  this dataset doesn't include that, but didn't Omar mention that previously?
# if so, the repeated measures var can either be used as an additional variable in the model, or treated as a repeated measures variable... I'm honestly not sure which one of these approaches is better (is there even a definitive answer?)
# see: https://stats.stackexchange.com/questions/38995/using-lme-to-analyse-a-complete-randomized-block-design-with-repeated-measures

# some more resources:

# handling a three-way anova (treatment, depth, year) or two-way (treatment, depth) within a block design: 
# http://www.tfrec.wsu.edu/anova/RCB3way.html

# tackling the two-way (three-way) block anova scenario, especially useful for thinking about interactions in this case:
# http://www.psych.nyu.edu/cohen/three_way_ANOVA.pdf

# a stats discussion about how to deal with repeated measures in this context:
# https://stats.stackexchange.com/questions/38995/using-lme-to-analyse-a-complete-randomized-block-design-with-repeated-measures


########################################################################
# WHAT DEPENDENT VARIABLE TO LOOK AT?

# just search and replace whatever variable is currently in use

# these are the variables:
# search for: "netmineralization_ugN_per_g_per_d"
# "pH","moisture","C","N","Cnratio","Fe2","Fe2and3","Feratio","Fe","Al","Si","Bpi","Bpo","Bt","Npi","Npo","Nt","TP"
# DONE: 



########################################################################
# ONE-WAY MIXED EFFECTS MODEL, BLOCK DESIGN

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
data2_noNA <- subset(df, !is.na(C))

# one-way test (only treatment)
lme_onewaysampletype = lme(C ~ Treatment, data=data2_noNA, random = ~1|Block)
anova(lme_onewaysampletype)

# post hoc test
posthoc_glht <- summary(glht(lme_onewaysampletype, linfct=mcp(Treatment = "Tukey")))
#posthoc_glht_bonf <- summary(glht(lme_onewaysampletype, linfct=mcp(Treatment = "Tukey")), test = adjusted(type = "bonferroni"))

# summary info
summary(posthoc_glht)
#summary(posthoc_glht_bonf)

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
write.xlsx(anova(lme_onewaysampletype), file=paste(path.expand(pathsavetab), "stats-tables/C_onewayANOVA.xlsx",sep=""), sheetName="lme() table")
# post-hoc tukey with bonferroni
write.xlsx(mtests, file=paste(path.expand(pathsavetab), "stats-tables/C_onewayANOVA.xlsx",sep=""), sheetName="post-hoc tukey", append=TRUE)

# save diagnostic plots

# fitted vs residuals
png(file = paste(pathsavetab, "stats-tables/C_ANOVAdiagnostics_1.png", sep=""),width=6,height=6,units="in",res=150)
plot(lme_onewaysampletype,col=data2_noNA$Treatment, main="Diagnostic Trellis Plot")
dev.off()

# fitted vs sqrt(abs(residuals))
png(file = paste(pathsavetab, "stats-tables/C_ANOVAdiagnostics_2.png", sep=""),width=6,height=6,units="in",res=150)
scatter.smooth(fitted(lme_onewaysampletype),sqrt(abs(resid(lme_onewaysampletype))))
dev.off()

# qq plot
png(file = paste(pathsavetab, "stats-tables/C_ANOVAdiagnostics_3.png", sep=""),width=6,height=6,units="in",res=150)
qqnorm(lme_onewaysampletype,col=data2_noNA$Treatment, main="QQ Plot", abline=c(0,1))
dev.off()

# boxplot of residuals
png(file = paste(pathsavetab, "stats-tables/C_ANOVAdiagnostics_4.png", sep=""),width=6,height=6,units="in",res=150)
plot(lme_onewaysampletype,Treatment~resid(.))
dev.off()



########################################################################
# TWO-WAY MIXED EFFECTS MODEL, BLOCK DESIGN

# lme can't handle columns with any NAs
data2_noNA <- subset(df, !is.na(C))

# lme version of the two-way test
lme_twowaysampletype = lme(C ~ Treatment*Depth, data=data2_noNA, random = ~1|Block)

# summary info; note different F values (apparently this is a known consequence)
anova(lme_twowaysampletype)

# post hoc test
posthoc_glht_1 <- summary(glht(lme_twowaysampletype, linfct=mcp(Treatment = "Tukey")))
#posthoc_glht_1 <- summary(glht(lme_twowaysampletype, linfct=mcp(sampletype = "Tukey")), test = adjusted(type = "bonferroni"))

# this basically means nothing - see the individual depth one-way ANOVAs below
posthoc_glht_2 <- summary(glht(lme_twowaysampletype, linfct=mcp(Depth = "Tukey")))
#posthoc_glht_2 <- summary(glht(lme_twowaysampletype, linfct=mcp(depth = "Tukey")), test = adjusted(type = "bonferroni"))

# summary info
summary(posthoc_glht_1)
summary(posthoc_glht_2)

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
write.xlsx(anova(lme_twowaysampletype), file=paste(path.expand(pathsavetab), "stats-tables/C_twowayANOVA.xlsx",sep=""), sheetName="lme() table")
# post-hoc tukey with bonferroni
write.xlsx(mtests_1, file=paste(path.expand(pathsavetab), "stats-tables/C_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey IV #1", append=TRUE)
write.xlsx(mtests_2, file=paste(path.expand(pathsavetab), "stats-tables/C_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey IV #2", append=TRUE)

# save diagnostic plots

# fitted vs residuals
png(file = paste(pathsavetab, "stats-tables/C_twowayANOVAdiagnostics_1.png", sep=""),width=6,height=6,units="in",res=150)
plot(lme_twowaysampletype,col=data2_noNA$Depth, main="Diagnostic Trellis Plot")
dev.off()

# fitted vs sqrt(abs(residuals))
png(file = paste(pathsavetab, "stats-tables/C_twowayANOVAdiagnostics_2.png", sep=""),width=6,height=6,units="in",res=150)
scatter.smooth(fitted(lme_twowaysampletype),sqrt(abs(resid(lme_twowaysampletype))))
dev.off()

# qq plot
png(file = paste(pathsavetab, "stats-tables/C_twowayANOVAdiagnostics_3.png", sep=""),width=6,height=6,units="in",res=150)
qqnorm(lme_twowaysampletype,col=data2_noNA$Treatment, main="QQ Plot", abline=c(0,1))
dev.off()

# boxplot of residuals
png(file = paste(pathsavetab, "stats-tables/C_twowayANOVAdiagnostics_4.png", sep=""),width=6,height=6,units="in",res=150)
plot(lme_twowaysampletype,Treatment~resid(.))
dev.off()

# boxplot of residuals
png(file = paste(pathsavetab, "stats-tables/C_twowayANOVAdiagnostics_5.png", sep=""),width=6,height=6,units="in",res=150)
plot(lme_twowaysampletype,Depth~resid(.))
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

