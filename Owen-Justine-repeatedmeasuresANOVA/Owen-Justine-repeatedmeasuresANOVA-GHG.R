# Owen-Justine-repeatedmeasuresANOVA-GHG.R
# 
# run a few models for Justine (these are the GHG variables)
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
data <- read.xlsx(paste(pathfile,"JO_GHG.xlsx",sep=""),"Sheet1")
data2 <- as.data.frame(data, stringsAsFactors=FALSE)


########################################################################
# DATA CLEAN AND LOG TRANSFORM THE RESPONSE VARIABLES

# get rid of values we don't believe
data2$CO2[data2$CO2<=0] <- NA
data2$N2O[data2$N2O<=0] <- NA

# log transform CO2 and N2O
data2$logCO2 <- log(data2$CO2 + 1)
data2$logN2O <- log(data2$N2O + 1)

# CH4 we don't do this, because we're going to use a non-parametric approach


########################################################################
# WHAT DEPENDENT VARIABLE TO LOOK AT?

# just search and replace whatever variable is currently in use

# these are the variables:
#  "CH4"  
# DONE: "logCO2", "logN2O"  



########################################################################
# ONE-WAY REPEATED MEASURES ANOVA
# THIS IS ONLY FOR CO2 AND N2O

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
data2_noNA <- subset(data2, !is.na(logN2O))

#data2_noNA<-data2[is.na(data2$logN2O)==F,]
#data2_noNA <- subset(data2_noNA, logN2O>0.000001) # why isn't NH4 ugN per gsoil working

# aov version of the one-way test
aov_onewaySite = aov(logN2O ~ Site + Error(Month/(Site)), data=data2_noNA)
# lme version of the one-way test
lme_onewaySite = lme(logN2O ~ Site, data=data2_noNA, random = ~1|Month)

# summary info; note different F values (apparently this is a known consequence)
summary(aov_onewaySite)
anova(lme_onewaySite)

# post hoc test
posthoc_glht <- summary(glht(lme_onewaySite, linfct=mcp(Site = "Tukey")), test = adjusted(type = "bonferroni"))

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
write.xlsx(anova(lme_onewaySite), file=paste(path.expand(pathsavetab), "stats-tables/logN2O_onewayANOVA.xlsx",sep=""), sheetName="lme() table")
# post-hoc tukey with bonferroni
write.xlsx(mtests, file=paste(path.expand(pathsavetab), "stats-tables/logN2O_onewayANOVA.xlsx",sep=""), sheetName="post-hoc tukey with bonferroni", append=TRUE)

# save diagnostic plots

# fitted vs residuals
png(file = paste(pathsavetab, "stats-tables/logN2O_ANOVAdiagnostics_1.png", sep=""),width=6,height=6,units="in",res=150)
plot(lme_onewaySite,col=data2$Site, main="Diagnostic Trellis Plot")
dev.off()

# fitted vs sqrt(abs(residuals))
png(file = paste(pathsavetab, "stats-tables/logN2O_ANOVAdiagnostics_2.png", sep=""),width=6,height=6,units="in",res=150)
scatter.smooth(fitted(lme_onewaySite),sqrt(abs(resid(lme_onewaySite))))
dev.off()

# qq plot
png(file = paste(pathsavetab, "stats-tables/logN2O_ANOVAdiagnostics_3.png", sep=""),width=6,height=6,units="in",res=150)
qqnorm(lme_onewaySite,col=data2$Site, main="QQ Plot", abline=c(0,1))
dev.off()

# boxplot of residuals
png(file = paste(pathsavetab, "stats-tables/logN2O_ANOVAdiagnostics_4.png", sep=""),width=6,height=6,units="in",res=150)
plot(lme_onewaySite,Site~resid(.))
dev.off()




########################################################################
# WHAT DEPENDENT VARIABLE TO LOOK AT FOR NON-PARAMETRIC OPTION?

# just search and replace whatever variable is currently in use

# these are the variables:
#  "CH4", "N2O"
# DONE:  


########################################################################
# non-parametric one-way repeated measures anova (Friedman rank sum test)

# bring in function
# see http://www.r-statistics.com/2010/04/repeated-measures-anova-with-r-tutorials/
source("~/Documents/GITHUB/RPersonalFunctionsChristine/friedman_test_with_posthoc.r")

# bring in justinedata2 for the nitrogen variables
data <- read.xlsx(paste(pathfile,"JO_GHG.xlsx",sep=""),"Sheet1")
data2 <- as.data.frame(data, stringsAsFactors=FALSE)

source("~/Documents/GITHUB/RPersonalFunctionsChristine/summarySE.r")
summarySE(data=data2, measurevar="N2O", groupvars=c("Month", "Site"))

# this also can't handle unbalanced designs, so let's try cutting out July 2014
data2_noNA_balanced <- subset(data2, data2$Month!="July 2014")

# check again on the balance
summarySE(data=data2_noNA_balanced, measurevar="N2O", groupvars=c("Month", "Site"))

# call the function
friedman_test_with_posthoc(N2O ~ Site | Date, data2_noNA_balanced)
# this isn't working - we'll go step by step instead




formu <- N2O ~ Site | Month
data <- data2_noNA_balanced

library(coin)
library(multcomp)
library(colorspace)

# get the names out of the formula
formu.names <- all.vars(formu)
Y.name <- formu.names[1]
X.name <- formu.names[2]
block.name <- formu.names[3]

if(dim(data)[2]>3) data <- data[,c(Y.name,X.name,block.name)]      # In case we have a "data" data frame with more then the three columns we need. This code will clean it from them...

# Note: the function doesn't handle NA's. In case of NA in one of the block T outcomes, that entire block should be removed.

# stopping in case there is NA in the Y vector
if(sum(is.na(data[,Y.name])) > 0) stop("Function stopped: This function doesn't handle NA's. In case of NA in Y in one of the blocks, then that entire block should be removed.")

# make sure that the number of factors goes with the actual values present in the data:
data[,X.name ] <- factor(data[,X.name ])
data[,block.name ] <- factor(data[,block.name ])
number.of.X.levels <- length(levels(data[,X.name ]))
if(number.of.X.levels == 2) { warning(paste("'",X.name,"'", "has only two levels. Consider using paired wilcox.test instead of friedman test"))}

# making the object that will hold the friedman test and the other.
the.sym.test <- symmetry_test(formu, data = data,	### all pairwise comparisons
                              teststat = "max",
                              xtrafo = function(Y.data) { trafo( Y.data, factor_trafo = function(x) { model.matrix(~ x - 1) %*% t(contrMat(table(x), "Tukey")) } ) },
                              ytrafo = function(Y.data){ trafo(Y.data, numeric_trafo = rank, block = data[,block.name] ) }
)
# if(to.print.friedman) { print(the.sym.test) }


if(to.post.hoc.if.signif)
{
      if(pvalue(the.sym.test) < signif.P)
      {
            # the post hoc test
            The.post.hoc.P.values <- pvalue(the.sym.test, method = "single-step")	# this is the post hoc of the friedman test
            
            
            # plotting
            if(to.plot.parallel & to.plot.boxplot)	par(mfrow = c(1,2)) # if we are plotting two plots, let's make sure we'll be able to see both
            
            if(to.plot.parallel)
            {
                  X.names <- levels(data[, X.name])
                  X.for.plot <- seq_along(X.names)
                  plot.xlim <- c(.7 , length(X.for.plot)+.3)	# adding some spacing from both sides of the plot
                  
                  if(color.blocks.in.cor.plot)
                  {
                        blocks.col <- rainbow_hcl(length(levels(data[,block.name])))
                  } else {
                        blocks.col <- 1 # black
                  }
                  
                  data2 <- data
                  if(jitter.Y.in.cor.plot) {
                        data2[,Y.name] <- jitter(data2[,Y.name])
                        par.cor.plot.text <- "Parallel coordinates plot (with Jitter)"
                  } else {
                        par.cor.plot.text <- "Parallel coordinates plot"
                  }
                  
                  # adding a Parallel coordinates plot
                  matplot(as.matrix(reshape(data2,  idvar=X.name, timevar=block.name,
                                            direction="wide")[,-1])  ,
                          type = "l",  lty = 1, axes = FALSE, ylab = Y.name,
                          xlim = plot.xlim,
                          col = blocks.col,
                          main = par.cor.plot.text)
                  axis(1, at = X.for.plot , labels = X.names) # plot X axis
                  axis(2) # plot Y axis
                  points(tapply(data[,Y.name], data[,X.name], median) ~ X.for.plot, col = "red",pch = 4, cex = 2, lwd = 5)
            }
            
            if(to.plot.boxplot)
            {
                  # first we create a function to create a new Y, by substracting different combinations of X levels from each other.
                  subtract.a.from.b <- function(a.b , the.data)
                  {
                        the.data[,a.b[2]] - the.data[,a.b[1]]
                  }
                  
                  temp.wide <- reshape(data,  idvar=X.name, timevar=block.name,
                                       direction="wide") 	#[,-1]
                  wide.data <- as.matrix(t(temp.wide[,-1]))
                  colnames(wide.data) <- temp.wide[,1]
                  
                  Y.b.minus.a.combos <- apply(with(data,combn(levels(data[,X.name]), 2)), 2, subtract.a.from.b, the.data =wide.data)
                  names.b.minus.a.combos <- apply(with(data,combn(levels(data[,X.name]), 2)), 2, function(a.b) {paste(a.b[2],a.b[1],sep=" - ")})
                  
                  the.ylim <- range(Y.b.minus.a.combos)
                  the.ylim[2] <- the.ylim[2] + max(sd(Y.b.minus.a.combos))	# adding some space for the labels
                  is.signif.color <- ifelse(The.post.hoc.P.values < .05 , "green", "grey")
                  
                  boxplot(Y.b.minus.a.combos,
                          names = names.b.minus.a.combos ,
                          col = is.signif.color,
                          main = "Boxplots (of the differences)",
                          ylim = the.ylim
                  )
                  legend("topright", legend = paste(names.b.minus.a.combos, rep(" ; PostHoc P.value:", number.of.X.levels),round(The.post.hoc.P.values,5)) , fill =  is.signif.color )
                  abline(h = 0, col = "blue")
                  
            }
            
            list.to.return <- list(Friedman.Test = the.sym.test, PostHoc.Test = The.post.hoc.P.values)
            if(to.print.friedman) {print(list.to.return)}
            return(list.to.return)
            
      }	else {
            print("The results were not significant, There is no need for a posthoc test")
            return(the.sym.test)
      }
}








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

