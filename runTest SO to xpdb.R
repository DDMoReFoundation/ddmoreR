
rm(list=ls())

# Paths setup 
TEL.zip.path = "C:/Users/cmusselle/Projects/DDmore/TEL-R/.__artefacts/DDMoRe.TEL_0.0.1.tar.gz"

SO.output.path = "C:/Users/cmusselle/Projects/DDmore/TEL-R/development data/warfarin_PK_ODE/eric/"

# Installation

install.packages(TEL.zip.path, repos = NULL, type = "source")
require("DDMoRe.TEL")
setwd(SO.output.path)

# Load in SO
SOObject = LoadSOObject("warfarin_PK_ODE_SO.xml")

# Hack to load in required part of Residuals into SO #################################
indwres = read.csv("ddmore_indwres.csv",header=T, sep = ';')
names(indwres) <- c("ID", "TIME", "IWRES")
# Add an 'i' to the begining of the ID column: may not be needed in future  
indwres$ID <- sub("^", "i", indwres$ID )

popwres = read.csv("ddmore_popwres.csv",header=T, sep = ';')
names(popwres) <- c("ID", "TIME", "WRES")
# Add an 'i' to the begining of the ID column: may not be needed in future  
popwres$ID <- sub("^", "i", popwres$ID )

SOObject@Estimation@Residuals$Population = popwres
SOObject@Estimation@Residuals$Individual = indwres

# Finished workaround Hacking ######################################################

# Test for fetching Raw Data from a file 
MyDataFrame = as.data(SOObject, inputDataPath="../../warfarin_conc.csv") 

# Convert to xpose data base. 
myXpdb = as.xpdb(SOObject, inputDataPath="../../warfarin_conc.csv") 

# Plotting tests 
require('xpose4')

##  Now create the diagnostic plots using Xpose!
basic.gof(myXpdb)


## PRED and IPRED vs DV
dv.vs.pred(myXpdb)
dv.vs.ipred(myXpdb)
dv.vs.pred.ipred(myXpdb)

# IWRES plots vs IPRED, TIME
absval.iwres.vs.ipred(myXpdb)
absval.iwres.vs.idv(myXpdb)
iwres.dist.hist(myXpdb)
iwres.dist.qq(myXpdb)

# WRES plots vs IPRED, TIME
wres.vs.pred(myXpdb)
wres.vs.idv(myXpdb)
wres.dist.hist(myXpdb)
wres.dist.qq(myXpdb)

## Individual plots of observed data, PRED, IPRED
ind.plots(myXpdb)

## Plots of parameters, ETAs
parm.splom(myXpdb)
ranpar.splom(myXpdb)
parm.hist(myXpdb)
ranpar.hist(myXpdb)
parm.qq(myXpdb)
ranpar.qq(myXpdb)
ranpar.vs.cov(myXpdb)



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


