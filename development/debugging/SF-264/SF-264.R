
#
# Debug script for Issue SF-264
# 
# Input Variables defined as Categorical are not converted to Factors in R
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
setwd('C:\\see-installer\\SEE\\MDL_IDE\\workspace\\development\\debugging\\SF-264')
mdlfile = 'UseCase5_old.mdl'

# Examine MDL Object
myMDLObj <- getMDLObjects(mdlfile)

# Run the Model
data_file = "warfarin_conc.csv"

nonmem <- estimate(mdl_file, target="NONMEM", subfolder="Nonmem")


SO_file = "UseCase5.SO.xml"

# Load SO object
SO = LoadSOObject(SO_file)


df <- as.data(SO, data_file)
xpdb <- as.xpdb(SO, data_file)

out = getPopulationParameters(SO)


myParObj <- getParameterObjects(mdl_file)[[1]]
myDataObj <- getDataObjects(mdl_file)[[1]]

myModelObj <- getModelObjects(mdl_file)[[1]]

params = names(myModelObj@INDIVIDUAL_VARIABLES)
covariates = names(myModelObj@COVARIATES)
randpar = names(myModelObj@RANDOM_VARIABLE_DEFINITION)

