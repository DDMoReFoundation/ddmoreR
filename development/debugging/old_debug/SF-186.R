
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

temp <- getPopulationParameters(SO, what="estimates", block="STRUCTURAL")
parValues <- temp$MLE


myParObj <- getParameterObjects(mdl_file)[[1]]
myParObjUpdated <- update(myParObj,
                          block="STRUCTURAL",
                          item=names(parValues),
                          with=list(value=parValues))


