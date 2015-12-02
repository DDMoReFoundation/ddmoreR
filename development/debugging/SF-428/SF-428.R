
rm(list = ls())

root = "C:\\Users\\cmusselle\\Projects\\DDmore\\TEL-R"
setwd(root)
full_install = FALSE

if ("ddmore" %in% .packages()) {
  detach("package:ddmore", unload=TRUE)
}

if (full_install) {
  install.packages("C:\\Users\\cmusselle\\Projects\\DDmore\\TEL-R\\.__artefacts/ddmore_0.0.4.tar.gz", repos=NULL, type="source")
} else {
  ddmore = as.package("DDMoRe.TEL")
  load_all(ddmore)
}

require(ddmore)

#----------------------------------------------

# Set file path definitions 
setwd('C:\\Users\\cmusselle\\Projects\\DDmore\\TEL-R\\development\\debugging\\SF-428')

# Fix for Servers
# DDMORE.setServer(createFISServer(startupScript = ""))

mdlfile = 'UseCase2_TIMEchange_fixed.mdl'
data_file = "warfarin_conc_TIMEchange.csv"
SO_file = "UseCase2_TIMEchange_fixed.SO.xml"

# Examine MDL Object
myMDLObj <- getMDLObjects(mdlfile)

# Examine SO 
SO = LoadSOObject(SO_file)

# Form Xpose DB
df <- as.data(SO, data_file)
xpdb <- as.xpdb(SO, data_file)

# Extract Parameters 
out = getPopulationParameters(SO)

# Test File 
test_file("ddmore.TEL/inst/tests/testthat/test.as.data.R")

