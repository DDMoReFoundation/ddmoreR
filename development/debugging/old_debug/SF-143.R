
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
setwd('C:\\Users\\cmusselle\\Projects\\DDmore\\TEL-R\\dev scripts\\debug\\Nock_model')
SO_file = "Nock_2013_Carboplatin_PK_OncoCourse.MONOLIX.SO.xml"
mdl_file = 'Nock_2013_Carboplatin_PK_OncoCourse.MONOLIX.mdl'
data_file = "Carbo_DDMoRe_log2mod_MDV.CSV"

# Load SO object
SO = LoadSOObject(SO_file)
df <- as.data(SO, data_file)

# Temporary Fix to test locally
# TEL.setServer(createFISServer(startupScript = ""))

xpdb <- as.xpdb(SO, data_file)



# For X in 

MDLObjs <- .getMdlInfoFromSO(SO, what="mdl")

out = getPopulationParameters(SO)


