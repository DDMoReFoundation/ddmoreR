# Tests for loading SO from PharmML and assiciated getting methods

# Setup 
rm(list=ls())
setwd("C:/Users/cmusselle/Projects/DDmore/TEL-R/development data/")
source("setup.R")
setwd("C:/Users/cmusselle/Projects/DDmore/TEL-R/development data/warfarin_PK_ODE/eric/")

# Load in SO
SOObject = LoadSOObject("warfarin_PK_ODE_SO.xml")

# Hack to load in required part of Residuals into SO 
indwres = read.csv("ddmore_indwres.csv",header=T, sep = ';')
names(indwres) <- c("ID", "TIME", "IWRES")
indwres$ID <- sub("^", "i", indwres$ID )

popwres = read.csv("ddmore_popwres.csv",header=T, sep = ';')
names(popwres) <- c("ID", "TIME", "WRES")
popwres$ID <- sub("^", "i", popwres$ID )

SOObject@Estimation@Residuals$Population = popwres
SOObject@Estimation@Residuals$Individual = indwres

# Finished Hacking workaround 

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
MyDataFrame = as.data(SOObject, inputDataPath="../../warfarin_conc.csv") 


# Convert to xpose data base. 
myXpdb = as.xpdb(SOObject, inputDataPath="../../warfarin_conc.csv") 

# Plotting tests 
require('xpose4')

##  Now create the diagnostic plots using Xpose!
basic.gof(myXpdb)




# # Test to merge all data together
# rawData = read.csv("../../warfarin_conc.csv", na.strings=".")
# 
# predictions = DDMoRe.TEL:::getPredictions(SOObject)$data
# predictions[["TIME"]] = as.numeric(as.character(predictions[["TIME"]]))
# 
# df = merge(new_rawData2, predictions)
# df = merge(rawData, predictions)
# 
# residuals = DDMoRe.TEL:::getResiduals(SOObject)
#   
# df = merge(df, residuals$Population)
# 
# df = merge(df, residuals$Individual)


# 
# testdf = data.frame(A = as.integer(seq(1,5)), B=as.numeric(seq(5,7, .5)), 
#                     C= as.character(letters[1:5]), D=as.factor(11:15), 
#                     stringsAsFactors = FALSE)






