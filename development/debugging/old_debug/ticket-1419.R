
library(testthat)
require(devtools)
root = "C:\\Users\\cmusselle\\Projects\\DDmore\\TEL-R"
setwd(root)

ddmore = as.package("DDMoRe.TEL")

load_all(ddmore)

require(DDMoRe.TEL)

# ----------------

setwd('C:\\Users\\cmusselle\\Projects\\DDmore\\TEL-R\\dev scripts\\debug\\UseCase1_discrete')

SO = LoadSOObject('UseCase1.SO.xml')

mdl_file = 'UseCase1.mdl'

data_file = "warfarin_conc.csv"

xpdb <- as.xpdb(SO, data_file)
