# pileflux_draftcode.r
#
# sintana draft code - get the pile flux for her compost project
#
# Marin Carbon Project
# Compost pile project
# code begun Dec. 2015
# Sintana is a genius!!!!!!, UCB, Silver Lab

# output products:
# 


########################################################################
# GET READY TO BRING IN DATA

library(ggplot2)
library(gridExtra)
library(scales)
library(plyr)
library(dplyr)
library(tidyr)
library(data.table)
library(chron)
library(lubridate)
library(lattice)
library(reshape2)
options(java.parameters = "-Xmx5000m")
library(xlsx)

# define standard error function
ste <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

# where to save outputs
#pathsavefiles = "~/Documents/GITHUB/cso040code_ArrayGHG/ArrayGHG-Data-Rprocessed/"
# pathsavefiles = "~/Documents/GITHUB/cso040code_ArrayGHG/ArrayGHG-Data-Raw/Sensor-data-Ryan-practice/Getting things together for Whendee/"
# pathsavefigs = "~/Documents/GITHUB/cso040code_ArrayGHG/ArrayGHG-Data-Analyses/SensorFigures/"
# sensordatapath = "~/Documents/GITHUB/cso040code_ArrayGHG/ArrayGHG-Data-Raw/Sensor-data-Ryan/nov2015/only new data/"
# calibrationdatapath = "~/Documents/GITHUB/cso040code_ArrayGHG/ArrayGHG-Data-Raw/Sensor-data-Ryan-practice/"


########################################################################
# BRING IN NEW DATA SHEETS

# bring in the Picarro data

# bring in the datalogger data (sensors)



########################################################################
# INFO ABOUT THE DATA

## whatever info


########################################################################
# CALCULATE WIND SPEED AND DIRECTION

# calc speed and dir for every anemometer for every 1 min
# we'll save this as 8 variables

# calc speed and dir for every anemometer for every 4 min
# we'll save this as 8 variables

# calculate how much variability there was during those 4 mins
# time of deviation from the "ok" theta range, and how much angular deviation there was


########################################################################
# GET SENSOR DATA FOR THESE TIME PERIODS

# calc mean and sd for every sensor for every 1 min
# we'll save this as 27 variables
# 3 vars, 9 sensors/var

# calc mean and sd for every sensor for every 4 min
# we'll save this as 27 variables
# 3 vars, 9 sensors/var


########################################################################
# SPLIT THE INTERVALS INTO BLOCKS

# make some tough decisions about this later

# decide which blocks get a flux calculated for them
# was wind direction sufficiently constant?
# if so, ok to calculate a flux

# this part we'll have to do some examples and make sure it works
# be very careful here


########################################################################
# CALCULATE FLUX FOR EACH OK BLOCK

# use a for loop

# what blocks are ok?

# pre-start the data.frame to save the fluxes into

# now start the loop
for(i in 1:2)
{
  print(i)
  
  # get ID of block
  
  # which tower is upwind?
  # step 1 = get polar coordinates (R and theta)
  # step 2 = use the theta to define which GT is up/down wind (ifelse statements)
  
  # concentration differences at each height
  
  # calculate the fetch
  
  # solve for flux
  
  # save into the dataframe
  
}


########################################################################
# GET THE SENSOR DATA INTO THE MASTER DATA SHEET

# go get the sensor data and avg. over the ok blocks

# insert into df



########################################################################
# SAVE AS CSV

# save as csv




########################################################################
# TO DO?

# uncertainty needs to propagate through the calcs
# (decide later on Monte Carlo vs. high and low std dev approach)







