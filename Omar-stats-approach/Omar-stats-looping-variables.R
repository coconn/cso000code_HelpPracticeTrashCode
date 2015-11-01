# for omar - switch vars in each loop

# needed for saving

# where should this save?
# where to save outputs
#pathsavefiles = "~/Documents/GITHUB/"
#pathsavefiles = "~/Documents/WilliamsFancyDataFolder/"


# are there fancy files names?
# no


variablelist <- c("C","N","pH")

for (i in 1:length(variablelist)) {
      
      ####### THIS IS THE STEP WE HAVE TO SOLVE
      # what variable am I looking at?
      varhere <- variablelist[i]
      ##### WHEN WE GET THIS, THEN PASTE ALL THE STUFF FROM THE OMAR FILE BELOW AND SWITCH OUT VARHERE FOR C
      
      
      ##### OMAR AND CHRISTINE PLAYING AROUND
      
      # lme can't handle columns with any NAs
      data2_noNA <- subset(df, !is.na(varhere))
      
      # one-way test (only treatment)
      lme_onewaysampletype = lme(varhere ~ Treatment, data=data2_noNA, random = ~1|Block)
      anova(lme_onewaysampletype)
      
      # post hoc test
      posthoc_glht <- summary(glht(lme_onewaysampletype, linfct=mcp(Treatment = "Tukey")))
      #posthoc_glht_bonf <- summary(glht(lme_onewaysampletype, linfct=mcp(Treatment = "Tukey")), test = adjusted(type = "bonferroni"))
      
      # summary info
      summary(posthoc_glht)
      #summary(posthoc_glht_bonf)
      
      
      
      # do math here
      
      x <- matrix(rnorm(10),20,5)
      
      
      # print a diff variable
      print(samplestoprocess[i])
      
      # boxplot of residuals
      png(file = paste(pathsavetab, "stats-tables/", variablelist[i], "_ANOVAdiagnostics_4.png", sep=""),width=6,height=6,units="in",res=150)
      plot(lme_onewaysampletype,Treatment~resid(.))
      dev.off()
      
      
      # you want to save x
      
      # define files to save
      #xname <- paste(pathsavefiles, "dataoutput_", i, ".csv", sep = "")
      
      
      
      # save the thing as csv
      #write.csv(x, file=xname, row.names=FALSE)
      
      
}



