

library(testthat)
require(devtools)
root = "C:\\Users\\cmusselle\\Projects\\DDmore\\TEL-R"
setwd(root)

ddmore = as.package("DDMoRe.TEL")

load_all(ddmore)

require(DDMoRe.TEL)

# ----------------

setwd('C://Users/cmusselle/Downloads/')

SO = LoadSOObject('UseCase9.SO.xml')

mdl_file = 'UseCase9.mdl'

data_file = "warfarin_infusion.csv"

xpdb <- as.xpdb(SO, data_file)
