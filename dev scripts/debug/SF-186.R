
rm(list=ls())

if ("DDMoRe.TEL" %in% .packages()) {
  detach("package:DDMoRe.TEL", unload=TRUE)
} 

library(testthat)
require(devtools)
root = "C:\\Users\\cmusselle\\Projects\\DDmore\\TEL-R"
setwd(root)

ddmore = as.package("DDMoRe.TEL")

load_all(ddmore)

require(DDMoRe.TEL)

# --------------------------

# file path definitions 
setwd('C:\\Users/cmusselle/Downloads/')
SO_file = "UseCase2.SO.xml"
mdl_file = 'C:\\Users/cmusselle/Downloads/UseCase2.mdl'
data_file = "warfarin_conc.csv"

# Load SO object
SO = LoadSOObject(SO_file)
df <- as.data(SO, data_file)
xpdb <- as.xpdb(SO, data_file)


out = getPopulationParameters(SO)

# 
temp <- DDMoRe.TEL:::getPopulationEstimates(mlx)
arNames <- names(as.data.frame(temp))

#' Ensure that parameter names are consistent with the model parameters.
#' Parameter names should come from the PharmML - due to issues with naming in the Standard Output Object
#' we need to provide these here for now.
structuralNames <- c("POP_CL","POP_V","POP_KA","POP_TLAG")
variabilityNames <- c("PPV_CL","PPV_V","PPV_KA","PPV_TLAG","RUV_PROP","RUV_ADD")

#' We can then update the parameter object using the "update" function.  
#' In future, we will alter the update(...) function to take a vector of parameters from the estimation
#' to update ALL initial values.

myParObj <- getParameterObjects(mdl_file)[[1]]

myParObjUpdated <- update(myParObj,block="STRUCTURAL",item=parNames[parNames%in%structuralNames],with=list(value=parValues[parNames%in%structuralNames]))
myParObjUpdated <- update(myParObjUpdated,block="VARIABILITY",item=parNames[parNames%in%variabilityNames],with=list(value=parValues[parNames%in%variabilityNames]))


#' The name of the correlation parameter 'CORR_PPV_CL_V' is different in the SO ('r_V_CL'), and needs to be handled differently 
myParObjUpdated <- update(myParObjUpdated,block="VARIABILITY",item="CORR_PPV_CL_V",with=list(value=parValues[parNames%in%"CORR_PPV_CL_V"]))

#' Let's now look at the updated MCL parameter object.
myParObjUpdated


