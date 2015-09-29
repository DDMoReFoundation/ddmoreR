
# Clear workspace. 
rm(list=ls())

# Setup imports and directory
if ("DDMoRe.TEL" %in% .packages()) {
  detach("package:DDMoRe.TEL", unload=TRUE)
} 
install.packages("C:/Users/cmusselle/Projects/DDmore/TEL-R/.__artefacts/DDMoRe.TEL_0.0.3.tar.gz", repos = NULL, type = "source")
require("DDMoRe.TEL")

# Load in SO
data.path = "C:\\Users\\cmusselle\\Projects\\DDmore\\TEL-R/dev scripts/debug/UseCase1_10/UseCase1_10.SO.xml"
csv.file.path = "C:\\Users\\cmusselle\\Projects\\DDmore\\TEL-R/dev scripts/debug//UseCase1_10//warfarin_conc.csv"

SOObject <- LoadSOObject(data.path)

populationParameters = getPopulationParameters(SOObject, what = "estimates")

# Test for fetching Raw Data from a file 
MyDataFrame = as.data(SOObject, inputDataPath=csv.file.path)

# Convert to xpose data base. 
myXpdb = as.xpdb(SOObject, inputDataPath=csv.file.path) 

