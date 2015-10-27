
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
setwd('C://Users/cmusselle/Downloads/')
SO_file = "UseCase2.SO.xml"
mdl_file = 'UseCase2.mdl'
data_file = "warfarin_conc_analytic.csv"

# Load SO object

SO = LoadSOObject(SO_file)

df <- as.data(SO, data_file)

xpdb <- as.xpdb(SO, data_file)

out = getPopulationParameters(SO)

