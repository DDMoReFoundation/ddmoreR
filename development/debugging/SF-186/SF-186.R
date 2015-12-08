#
# Debug script for Issue SF-186
# 
# Have an implementation of updateInitialEstimates to simplify using update on a model object
#

# RELOAD TELL LIBRARY FORM SOURCE AND INITIALIZE SERVERS
# -------------------------------------------------------

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

# Temporary Fix to test locally
TEL.setServer(createFISServer(startupScript = ""))

# --------------------------

# Set file path definitions 
setwd('C:\\see-installer\\SEE\\MDL_IDE\\workspace\\development\\debugging\\SF-186')
mdlfile = 'UseCase5_new.mdl'

# Examine MDL Object
myMDLObj <- getMDLObjects(mdlfile)

# Run the Model
data_file = "warfarin_conc_sexf.csv"

nonmem <- estimate(mdlfile, target="NONMEM", subfolder="Nonmem")




#SO_file = "UseCase5.SO.xml"
#
## Load SO object
#SO = LoadSOObject(SO_file)
#
#
#df <- as.data(SO, data_file)
#xpdb <- as.xpdb(SO, data_file)
#
#out = getPopulationParameters(SO)


#myParObj <- getParameterObjects(mdl_file)[[1]]
#myDataObj <- getDataObjects(mdl_file)[[1]]
#
#myModelObj <- getModelObjects(mdl_file)[[1]]
#
#params = names(myModelObj@INDIVIDUAL_VARIABLES)
#covariates = names(myModelObj@COVARIATES)
#randpar = names(myModelObj@RANDOM_VARIABLE_DEFINITION)




#' We can then update the parameter object using the "update" function.  
#' In future, we will alter the update(...) function to take a vector of parameters from the estimation
#' to update ALL initial values.

myParObj <- getParameterObjects(mdlfile)[[1]]
myParObjUpdated <- update(myParObj,block="STRUCTURAL",item=parNames[parNames%in%structuralNames],with=list(value=parValues[parNames%in%structuralNames]))
myParObjUpdated <- update(myParObjUpdated,block="VARIABILITY",item=parNames[parNames%in%variabilityNames],with=list(value=parValues[parNames%in%variabilityNames]))

#' The name of the correlation parameter 'CORR_PPV_CL_V' is different in the SO ('r_V_CL'), and needs to be handled differently 
myParObjUpdated <- update(myParObjUpdated,block="VARIABILITY",item="CORR_PPV_CL_V",with=list(value=parValues[parNames%in%"CORR_PPV_CL_V"]))

#' Let's now look at the updated MCL parameter object.
myParObjUpdated
