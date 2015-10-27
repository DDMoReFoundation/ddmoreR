

library(testthat)
require(devtools)
root = "C:\\Users\\cmusselle\\Projects\\DDmore\\TEL-R"
setwd(root)

ddmore = as.package("DDMoRe.TEL")

load_all(ddmore)

require(DDMoRe.TEL)



# ----------------

setwd('C:\\Users\\cmusselle\\Projects\\DDmore\\TEL-R\\dev scripts\\debug\\UseCase8_1')


SO = LoadSOObject('UseCase8_1.SO.xml')

mdl_file = 'UseCase8_1.mdl'

data_file = "warfarin_conc_bov_P4.csv"

xpdb <- as.xpdb(SO, data_file)

# Check differnce in raw data and predictions
source('C:\\Users\\cmusselle\\Projects\\DDmore\\TEL-R\\dev scripts\\compareDFs.R')

pred = SO@Estimation@Predictions$data

rawData <- DDMoRe.TEL::read.NONMEMDataSet(data_file)


# print lines in csv 

idx = sapply(rawData[['ID']], FUN=function(x) {grepl('#', x)})

comment_ids = rawData[idx, ]

noncomment_ids = rawData[!idx, ]

