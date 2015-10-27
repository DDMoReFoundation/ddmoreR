#' Product 3 - Expanding TEL task functionality with the warfarin ODE model
#' =========================================================================================
#' Using TEL functions, Monolix, NONMEM, Xpose, PsN, and simulx in a workflow with an initial
#' estimation step in Monolix, followed by an SAEM estimation step in NONMEM (using the
#' final estimates from Monolix is initial estimates in NONMEM), an FOCEI estimation step
#' in NONMEM (using the final estimates from the Monolix step as initial estimates for the
#' FOCEI step), VPC and bootstrap steps in PsN (based on the NONMEM FOCEI step), and a
#' simulation step in simulx (based on the NONMEM FOCEI model).


rm(list=ls())

# Setup imports and directory
if ("DDMoRe.TEL" %in% .packages()) {
  detach("package:DDMoRe.TEL", unload=TRUE)
} 
install.packages("C:/Users/cmusselle/Projects/DDmore/TEL-R/.__artefacts/DDMoRe.TEL_0.0.3.tar.gz", repos = NULL, type = "source")
require("DDMoRe.TEL")

setwd("C:\\Users\\cmusselle\\Projects\\DDmore\\ddmore-use.cases\\MDL\\Product4")

mdlfile <- "UseCase1.mdl" 

myDataObj <- getDataObjects(mdlfile)[[1]]

myData <- read(myDataObj)

myData
