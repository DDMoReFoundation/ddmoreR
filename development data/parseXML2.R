
rm(list=ls())

# Setup imports and directory
detach("package:DDMoRe.TEL", unload=TRUE)
install.packages("C:/Users/cmusselle/Projects/DDmore/TEL-R/.__artefacts/DDMoRe.TEL_0.0.2.tar.gz", repos = NULL, type = "source")
require("DDMoRe.TEL")
require("XML")

setwd("C:/Users/cmusselle/Projects/DDmore/TEL-R/ddmore.TEL")

source("ddmore.TEL/R/xmlParsers.R")

C:\Users\cmusselle\Projects\DDmore\TEL-R\ddmore.TEL\inst\tests\data\PharmMLSO\HandCoded

# Generate Blank SO object 
SOObject = createSOObject()

# Get a reference to the root node in the xml doc
#root = xmlRoot(xmlTreeParse("development data\\MONOLIX_SO\\Warfarin-ODE-latest.SO.xml"))
root = xmlRoot(xmlTreeParse("inst\\tests\\data\\PharmMLSO\\HandCoded\\warfarin_PK_ODE_SO_FULL.xml"))
root = xmlRoot(xmlTreeParse("inst\\tests\\data\\PharmMLSO\\MachineGenerated\\Warfarin-ODE-latest.SO.xml"))


# Fetch List of SOBlock elements
SOBlockList = root[names(root) == "SOBlock"]

# Assumes only one SOBlock for now!
stopifnot(length(SOBlockList) == 1)

### Future for loop to start here if multiple SO blocks

# Fetch all Components of the SO object that are defined
SOChildren <- xmlChildren(SOBlockList[[1]])

# Execute Parsers
SOObject = ParseToolSettings(SOObject, SOChildren$ToolSettings)

SOObject = ParseRawResults(SOObject, SOChildren$RawResults)

SOObject = ParsePopulationEstimates(SOObject, SOChildren$Estimation[["PopulationEstimates"]])

SOObject = ParsePrecisionPopulationEstimates(SOObject, SOChildren$Estimation[["PrecisionPopulationEstimates"]])

SOObject = ParseIndividualEstimates(SOObject, SOChildren$Estimation[["IndividualEstimates"]])

SOObject = ParseResiduals(SOObject, SOChildren$Estimation[["Residuals"]])

SOObject = ParsePredictions(SOObject, SOChildren$Estimation[["Predictions"]])





SOChildren = xmlChildren(root[["SOBlock"]])

PIE = root[["SOBlock"]][["Estimation"]][["PrecisionIndividualEstimates"]]

LogNormalDistribution 




MLEDataSet = root[[2]][["Estimation"]][["PopulationEstimates"]][["MLE"]]

MLEDataSet = root[[2]][["Estimation"]][["PopulationEstimates"]][["MLE"]]



TaskInformationNode = root[["SOBlock"]][["TaskInformation"]]

SOObject = ParseTaskInformation(SOObject, TaskInformationNode)



ResidualsNode = root[["SOBlock"]][["Estimation"]][["Residuals"]]



SOObject = ParseResiduals(SOObject, ResidualsNode)



#Node = xpathApply(root, "/d:SO/d:SOBlock/d:Estimation/d:PrecisionPopulationEstimates", namespaces=ns)



# 
# parsers = c("ToolSettings", "RawResults")
# 
# for (nodeName in SOChildList) {
#   
#   # Get node 
#   stopifnot(nodeName %in% slotNames(SOObject))
#       
#   # Update SO Object  
#   if (nodeName %in% parsers) {
#     SOObject = do.call(paste0("Parse", nodeName), list(SOObject, RootSONode[[nodeName]]))
#   }
#   # Get node 
# }
# 
# 



