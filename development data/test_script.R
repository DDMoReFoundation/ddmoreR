# Tests for loading SO from PharmML and assiciated getting methods

# Setup 
rm(list=ls())
setwd("C:/Users/cmusselle/Projects/DDmore/TEL-R/development data")
source("setup.R")

# Load in SO
SOObject = LoadSOObject("warfarin_PK_ODE_SO_FULL.xml")

# Testing Low Level Getter Functions 
tools = DDMoRe.TEL:::getToolSettings(SOObject)
raw_results = DDMoRe.TEL:::getRawResults(SOObject)
pop_est = DDMoRe.TEL:::getPopulationEstimates(SOObject)
prec_pop_est = DDMoRe.TEL:::getPrecisionPopulationEstimates(SOObject)
ind_est = DDMoRe.TEL:::getIndividualEstimates(SOObject)
prec_ind_est = DDMoRe.TEL:::getPrecisionIndividualEstimates(SOObject)
residuals = DDMoRe.TEL:::getResiduals(SOObject)
predictions = DDMoRe.TEL:::getPredictions(SOObject)
likelihood = DDMoRe.TEL:::getLikelihood(SOObject)
msgs = DDMoRe.TEL:::getSoftwareMessages(SOObject)


# Test for fetching Raw Data from a file 
MyDataFrame = as.data(SOObject) 

# Convert to xpose data base. 
myXpdb = as.xpdb(SOObject) 


# Plotting tests 
require('xpose4')

##  Now create the diagnostic plots using Xpose!
basic.gof(myXpdb)








