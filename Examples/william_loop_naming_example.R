# for william - simpler

# needed for saving

# where should this save?
# where to save outputs
#pathsavefiles = "~/Documents/GITHUB/"
pathsavefiles = "~/Documents/WilliamsFancyDataFolder/"


# are there fancy files names?
# no


samplestoprocess <- 10

for (i in 1:samplestoprocess) {
      
 # do math here
      
      x <- matrix(rnorm(10),20,5)
      
      # you want to save x
      
      # define files to save
      xname <- paste(pathsavefiles, "dataoutput_", i, ".csv", sep = "")
      
      # save the thing as csv
      write.csv(x, file=xname, row.names=FALSE)
      
      
}



