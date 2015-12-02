#
# Debug script for Issue DDMORE-1419
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
setwd('C:\\see-installer\\SEE\\MDL_IDE\\workspace\\development\\debugging\\DDMORE-1419')
mdlfile = 'UseCase5_new.mdl'

# Examine MDL Object
myMDLObj <- getMDLObjects(mdlfile)

# Run the Model
data_file = "warfarin_conc_sexf.csv"

nonmem <- estimate(mdlfile, target="NONMEM", subfolder="Nonmem")
