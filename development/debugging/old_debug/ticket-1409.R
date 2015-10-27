
library(testthat)
require(devtools)
root = "C:\\Users\\cmusselle\\Projects\\DDmore\\TEL-R"
setwd(root)

ddmore = as.package("DDMoRe.TEL")

load_all(ddmore)

require(DDMoRe.TEL)


setwd("C:\\Users\\cmusselle\\Downloads\\")

SOObject <- LoadSOObject('bootstrap_UPDRS1.SO.xml')


out = getPopulationParameters(SOObject, what='intervals', keep.only="mean")
out = getPopulationParameters(SOObject, what='precision')
out = getPopulationParameters(SOObject)

