
library(testthat)
require(devtools)
root = "C:\\Users\\cmusselle\\Projects\\DDmore\\TEL-R"
setwd(root)

ddmore = as.package("DDMoRe.TEL")

load_all(ddmore)

require(DDMoRe.TEL)

setwd('C://Users/cmusselle/Downloads/')

UC1_FOCEI_SO = LoadSOObject('UseCase1_FOCEI.SO.xml')

xpdb <- as.xpdb(UC1_FOCEI_SO,"warfarin_conc.csv")


