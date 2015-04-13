
rm(list=ls())

# Setup imports and directory
# Detach, install and reload Tell package 
if ("DDMoRe.TEL" %in% .packages()) {
  detach("package:DDMoRe.TEL", unload=TRUE)
}
install.packages("C:/Users/cmusselle/Projects/DDmore/TEL-R/.__artefacts/DDMoRe.TEL_0.0.3.tar.gz", repos = NULL, type = "source")
require("DDMoRe.TEL")
require("XML")

setwd("C:/Users/cmusselle/Projects/DDmore/TEL-R/ddmore.TEL")

source("R/xmlParsers.R")

# Generate Blank SO object 
SOObject = createSOObject()

# Get a reference to the root node in the xml doc
#root = xmlRoot(xmlTreeParse("development data\\MONOLIX_SO\\Warfarin-ODE-latest.SO.xml"))
root = xmlRoot(xmlTreeParse("inst\\tests\\data\\PharmMLSO\\HandCoded\\warfarin_PK_ODE_SO_FULL-v0_1.xml"))
#root = xmlRoot(xmlTreeParse("inst\\tests\\data\\PharmMLSO\\MachineGenerated\\Warfarin-ODE-latest.SO.xml"))


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


# Development of Model Diagnostics Block
SOChildren = xmlChildren(root[["SOBlock"]])

source("R/xmlParsers.R")
MD = root[["SOBlock"]][["ModelDiagnostic"]]

x = ParseModelDiagnostic(SOObject, MD)





# Simulation Block 
root = xmlRoot(xmlTreeParse("inst\\tests\\data\\PharmMLSO\\HandCoded\\warfarin_PK_ODE_SO_FULL.xml"))

SimulationNode = root[['SOBlock']][['Simulation']]

SOObject = createSOObject()

SOObject = ParseSimulation(SOObject, SimulationNode)

ReplicateBlockList = SimulationNode[names(SimulationNode) == "SimulationBlock"]



SimulationBlocks = SimulationNode[names(SimulationNode) == "SimulationBlock"]


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



