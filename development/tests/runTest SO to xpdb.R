
# Clear workspace. 
rm(list=ls())

# Paths setup. Set this to TEL repo location
# root = "C:\\Users\\cmusselle\\Projects\\DDmore\\TEL-R"

TEL.zip.path = paste(root, ".__artefacts/DDMoRe.TEL_0.0.1.tar.gz", sep="\\")
Monolix.SO.path = paste(root, "development data\\MONOLIX_SO", sep="\\")
Nonmem.SO.path = paste(root, "development data\\NONMEM_SO", sep="\\")

# Detach, install and reload Tell package 
if ("DDMoRe.TEL" %in% .packages()) {
  detach("package:DDMoRe.TEL", unload=TRUE)
} 

install.packages(TEL.zip.path, repos = NULL, type = "source")
require("DDMoRe.TEL")

setwd(root)

data.path = Monolix.SO.path

# -------------------
# MonoLix Test 
# -------------------
for (i in c(Monolix.SO.path, Nonmem.SO.path)){

data.path = i

# Load in SO
SOObject = LoadSOObject(paste(data.path, "Warfarin-ODE-latest.SO.xml", sep="\\"))

# Test for fetching Raw Data from a file 
MyDataFrame = as.data(SOObject, inputDataPath=paste(data.path, "warfarin_conc.csv",  sep="\\"))

# Convert to xpose data base. 
myXpdb = as.xpdb(SOObject, inputDataPath=paste(data.path, "warfarin_conc.csv",  sep="\\")) 

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

# Test Higher Level getter functions 

param = getParameterEstimates(SOObject)
est_info = getEstimationInfo(SOObject)