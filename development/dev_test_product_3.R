
# Clear workspace. 
rm(list=ls())

# Setup imports and directory
if ("DDMoRe.TEL" %in% .packages()) {
  detach("package:DDMoRe.TEL", unload=TRUE)
} 
install.packages("C:/Users/cmusselle/Projects/DDmore/TEL-R/.__artefacts/DDMoRe.TEL_0.0.3.tar.gz", repos = NULL, type = "source")
require("DDMoRe.TEL")


# Load in SO
data.path = "~/Projects/DDmore/TEL-R/dev scripts/Product3_EXAMPLE/models/Warfarin-ODE-latest-Monolix"
data.path = "~/Projects/DDmore/TEL-R/dev scripts/Product3_EXAMPLE/models/Warfarin-ODE-latest-Monolix/Product3_Warfarin-ODE-latest-Monolix.SO.xml"

SOObject <- LoadSOObject(data.path)






out = getPopulationParameters(SOObject)

out_structural = getPopulationParameters(SOObject, type="structural")
out_variability = getPopulationParameters(SOObject, type="variability")
out_estimates = getPopulationParameters(SOObject, what="estimates", keep.only="mean")
out_precision = getPopulationParameters(SOObject, what="precision")
out_intervals = getPopulationParameters(SOObject, what="intervals")
