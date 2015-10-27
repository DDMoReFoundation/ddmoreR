#' Using TEL functions, RNMImport, Xpose and PsN in a workflow
#' =========================
#'
#' author: "Mike K Smith
#' -------------------------
#' ### date: `r date()`
#' 

#' Initialise
#' =========================

#' Uses Mango Solutions library RNMImport that reads in NONMEM files
#' and creates an R object
library(RNMImport)
library(xpose4)
library(lattice)
#library(tables)

library(DDMoRe.TEL)

#' Set working directory
setwd("C:/TEMP/StandaloneExecutionEnvironment/PKPDstick_72/MDL_IDE/workspace/tumour_size")

#' Run Original Model: Run0
#' =========================

#' Fit base model
#' -------------------------
#' We can execute an mdl file directly:
base <- estimate("warfarin-ODE-latest.mdl", target="Monolix", subfolder="BaseModel")

class(base )   ## StandardOutput object

warfXpdb <- as.xpdb(base)

###############################################
## Xpose functions to perform model diagnostics
###############################################
basic.gof(warfXpdb)

## PRED and IPRED vs DV and TIME
dv.vs.pred(warfXpdb)
dv.vs.ipred(warfXpdb)
dv.vs.pred.ipred(warfXpdb)

## IWRES plots vs IPRED, TIME
absval.iwres.vs.ipred(warfXpdb)
absval.iwres.vs.idv(warfXpdb)
iwres.dist.hist(warfXpdb)
iwres.dist.qq(warfXpdb)

## WRES / CWRES vs PRED, TIME
wres.vs.pred(warfXpdb)
wres.vs.idv(warfXpdb)
wres.dist.hist(warfXpdb)
wres.dist.qq(warfXpdb)

## Individual plots of observed data, PRED, IPRED
ind.plots(warfXpdb)

## Plots of Parameters, ETAs
parm.splom(warfXpdb)
ranpar.splom(warfXpdb)
parm.hist(warfXpdb)
ranpar.hist(warfXpdb)
parm.qq(warfXpdb)
ranpar.qq(warfXpdb)
ranpar.vs.cov(warfXpdb)

####################################################
### /Xpose
####################################################

#' As part of the TEL package, there will be many functions for working with the 
#' standardised output object. These functions are currently in development, but as
#' an example we can use the getEstimationInfo function:
getEstimationInfo(base)

getParameterEstimates(base)

getParameterEstimates(base, type="structural")

getParameterEstimates(base, what="estimates")

####################################################
## Changing the model
####################################################
# #' We can change certain elements of the MOG (data, parameters, task) and re-run using the same model.
# #' This allows us to examine what happens when we fix parameters, change estimation methodology etc.
# #' If the model is set up appropriately we can test many different models simply by fixing or estimating different parameters.
# 

baseParam <- getParameterObjects("warfarin-ODE-latest.mdl")

## Fix covariate effect estimates to zero so that the covariates are excluded from the model
fixCovEffect <- baseParam
fixCovEffect@STRUCTURAL$BETA_CL_WT <- list(value=0, fix=true)
fixCovEffect@STRUCTURAL$BETA_V_WT <- list(value=0, fix=true)

myParUpdated <- update(baseParam, block="STRUCTURAL", with=list(value=as.vector(finalEstimates)))
myParUpdated

myNewMOG <- as.mogObj(dataObj = warfData, taskObj = warfTask, mdlObj = warfMod, parObj = warfParUpdated)
myNewMOG

# We can then write the MOG back out to MDL:
 write(myNewMOG,"myNewMog.mdl")

# We can then execute this mdl file using NONMEM:
 results2 <- estimate("myNewMog.mdl", target="Monolix")

 results2 <- estimate(myNewMOG, target="Monolix")

 results2 <- estimate(data = warfData, taskObj = warfTask, mdlObj = warfMod, parObj = warfParUpdated, target="Monolix")
