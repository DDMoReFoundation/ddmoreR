
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

# Temporary Fix to test locally
TEL.setServer(createFISServer(startupScript = ""))

myParObj <- getParameterObjects(mdl_file)[[1]]
myDataObj <- getDataObjects(mdl_file)[[1]]


myModelObj <- getModelObjects(mdl_file)[[1]]

params = names(myModelObj@INDIVIDUAL_VARIABLES)
covariates = names(myModelObj@COVARIATES)
randpar = names(myModelObj@RANDOM_VARIABLE_DEFINITION)


