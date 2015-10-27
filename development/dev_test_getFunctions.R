
# Clear workspace. 
rm(list=ls())

# Setup imports and directory
if ("DDMoRe.TEL" %in% .packages()) {
  detach("package:DDMoRe.TEL", unload=TRUE)
} 
install.packages("C:/Users/cmusselle/Projects/DDmore/TEL-R/.__artefacts/DDMoRe.TEL_0.0.3.tar.gz", repos = NULL, type = "source")
require("DDMoRe.TEL")


# Load in SO
data.path = "C://Users//cmusselle/Projects/DDmore/TEL-R/ddmore.TEL//inst//tests//data//PharmMLSO/HandCoded//warfarin_PK_ODE_SO_FULL-v0_2.xml"

SOObject <- LoadSOObject(data.path)

out = getPopulationParameters(SOObject)

out_structural = getPopulationParameters(SOObject, type="structural")
out_variability = getPopulationParameters(SOObject, type="variability")
out_estimates = getPopulationParameters(SOObject, what="estimates", keep.only="mean")
out_precision = getPopulationParameters(SOObject, what="precision")
out_intervals = getPopulationParameters(SOObject, what="intervals")
