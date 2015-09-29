
# Clear workspace. 
rm(list=ls())

# Setup imports and directory
if ("DDMoRe.TEL" %in% .packages()) {
  detach("package:DDMoRe.TEL", unload=TRUE)
} 
install.packages("C:/Users/cmusselle/Projects/DDmore/TEL-R/.__artefacts/DDMoRe.TEL_0.0.3.tar.gz", repos = NULL, type = "source")
require("DDMoRe.TEL")

# Load in SO
data.path = "C:\\Users\\cmusselle\\Projects\\DDmore\\ddmore-use.cases\\MDL\\Product4\\UseCase1.SO.xml"

Nonmem.SOObject <- LoadSOObject(data.path)

populationParameters = getPopulationParameters(Nonmem.SOObject, what = "estimates")

csv.file.path = "C:\\Users\\cmusselle\\Projects\\DDmore\\ddmore-use.cases\\MDL\\Product4\\warfarin_conc.csv"
inputDataPath = csv.file.path

# Test for fetching Raw Data from a file 
MyDataFrame = as.data(Nonmem.SOObject, inputDataPath=csv.file.path)

# Convert to xpose data base. 
myXpdb = as.xpdb(Nonmem.SOObject, inputDataPath=csv.file.path) 



## Code to load in raw data 
# rawData <- read.NONMEMDataSet(inputDataPath)
# # Checks for Column format
# rawData[["ID"]] <- as.numeric(rawData[["ID"]]) 
# rawData[["TIME"]] <- as.numeric(rawData[["TIME"]]) 
# names(rawData) <- toupper(names(rawData))
# 
# # Reorder data frame to have ID and TIME column as first two. 
# ID.col = names(rawData) == "ID"
# TIME.col = names(rawData) == "TIME"
# remaining.names = setdiff(names(rawData),c("ID","TIME"))
# rawData = cbind(rawData[, ID.col], 
#                 rawData[, TIME.col], 
#                 rawData[, remaining.names],
#                 deparse.level = 0)
# # Update names for first two columns 
# names(rawData) <- c(c("ID", "TIME"), remaining.names) 
# 
# N.residuals = Nonmem.SOObject@Estimation@Residuals$ResidualTable$data
# N.predictions = Nonmem.SOObject@Estimation@Predictions$data
# N.indiv_estimates = Nonmem.SOObject@Estimation@IndividualEstimates$Estimates$Mean$data
# N.indiv_estimates_RE = Nonmem.SOObject@Estimation@IndividualEstimates$RandomEffects$EffectMean$data
# 
# 
# out = mergeByPosition(rawData, predictions)
# 
# out2 = mergeByPosition(out, indiv_estimates)


# Test for fetching Raw Data from a file 
MyDataFrame = as.data(Nonmem.SOObject, inputDataPath=csv.file.path)

# Convert to xpose data base. 
myXpdb = as.xpdb(Nonmem.SOObject, inputDataPath=csv.file.path) 



# --------------- Previous results in Monolix

data.path = "C:\\Users\\cmusselle\\Projects\\DDmore\\TEL-R/dev scripts/Product3_EXAMPLE/models/Warfarin-ODE-latest-Monolix/Product3_Warfarin-ODE-latest-Monolix.SO.xml"

csv.file.path = "C:\\Users\\cmusselle\\Projects\\DDmore\\TEL-R/dev scripts/Product3_EXAMPLE/models/Warfarin-ODE-latest-Monolix/warfarin_conc.csv"

Monolix.SOObject = LoadSOObject(data.path)

# M.residuals = Monolix.SOObject@Estimation@Residuals$ResidualTable$data
# M.predictions = Monolix.SOObject@Estimation@Predictions$data
# M.indiv_estimates = Monolix.SOObject@Estimation@IndividualEstimates$Estimates$Mean$data
# M.indiv_estimates_RE = Monolix.SOObject@Estimation@IndividualEstimates$RandomEffects$EffectMean$data

# Test for fetching Raw Data from a file 
MyDataFrame = as.data(Monolix.SOObject, inputDataPath=csv.file.path)

# Convert to xpose data base. 
myXpdb = as.xpdb(Monolix.SOObject, inputDataPath=csv.file.path) 


