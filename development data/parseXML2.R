
rm(list=ls())


# Setup imports and directory
detach("package:DDMoRe.TEL", unload=TRUE)
install.packages("C:/Users/cmusselle/Projects/DDmore/TEL-R/.__artefacts/DDMoRe.TEL_0.0.1.tar.gz", repos = NULL, type = "source")
require("DDMoRe.TEL")
require("XML")
setwd("C:/Users/cmusselle/Projects/DDmore/TEL-R/data")

source("xmlParsers.R")

# Generate Blank SO object 
SOObject = createSOObject()

# Get a reference to the root node in the xml doc
root = xmlRoot(xmlTreeParse("warfarin_PK_ODE_SO_FULL.xml"))

# Update the namespace to use 'd' as the defualt. Must be done to use Xpath expressions with namespaces
ns = xmlNamespaceDefinitions(root, simplify = TRUE)
defaultNS = 'd'
names(ns)[1] <- defaultNS

# Fetch List of SOBlock elements
SOlist = xpathApply(root, "//d:SOBlock", namespaces=ns)

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



