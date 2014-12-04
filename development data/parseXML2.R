
rm(list=ls())

# Setup imports and directory
detach("package:DDMoRe.TEL", unload=TRUE)
install.packages("C:/Users/cmusselle/Projects/DDmore/TEL-R/.__artefacts/DDMoRe.TEL_0.0.1.tar.gz", repos = NULL, type = "source")
require("DDMoRe.TEL")
require("XML")

setwd("C:/Users/cmusselle/Projects/DDmore/TEL-R/development data")

source("../ddmore.TEL/R/xmlParsers.R")

# Generate Blank SO object 
SOObject = createSOObject()

# Get a reference to the root node in the xml doc
root = xmlRoot(xmlTreeParse("development data\\MONOLIX_SO\\Warfarin-ODE-latest.SO.xml"))

# Update the namespace to use 'd' as the defualt. Must be done to use Xpath expressions with namespaces
ns = xmlNamespaceDefinitions(root, simplify = TRUE)
defaultNS = 'd'
names(ns)[1] <- defaultNS

# Fetch List of SOBlock elements
sink("NUL")
SOlist = xpathApply(root, "//d:SOBlock", namespaces=ns)
sink()

# Assumes only one SOBlock for now!
stopifnot(length(SOlist) == 1)

### Future for loop to start here

RootSONode = SOlist[[1]]

# Fetch all Components of the SO object that are defined
SOChildren = xmlChildren(RootSONode)

# Execute Parsers
SOObject = ParseToolSettings(SOObject, SOChildren$ToolSettings)

SOObject = ParseRawResults(SOObject, SOChildren$RawResults)

SOObject = ParsePopulationEstimates(SOObject, SOChildren$Estimation[["PopulationEstimates"]])

SOObject = ParsePrecisionPopulationEstimates(SOObject, SOChildren$Estimation[["PrecisionPopulationEstimates"]])

SOObject = ParseIndividualEstimates(SOObject, SOChildren$Estimation[["IndividualEstimates"]])

SOObject = ParseResiduals(SOObject, SOChildren$Estimation[["Residuals"]])

SOObject = ParsePredictions(SOObject, SOChildren$Estimation[["Predictions"]])




MLEDataSet = root[[2]][["Estimation"]][["PopulationEstimates"]][["MLE"]]

ParseDataSet(MLEDataSet)


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



