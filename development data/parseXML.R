
# Setup imports and directory
require("XML")
setwd("C:/Users/cmusselle/Projects/DDmore/TEL-R/ddmore.TEL/data")

# Get a reference to the root node in the xml doc
root = xmlRoot(xmlTreeParse("warfarin_PK_ODE_SO.xml"))

# Update the namespace to use 'd' as the defualt. Must be done to use Xpath expressions with namespaces
ns = xmlNamespaceDefinitions(xmlRoot(doc), simplify = TRUE)
defaultNS = 'd'
names(ns)[1] <- defaultNS

# Query to fetch all DataSet elements
datasetList = getNodeSet(doc, "//d:DataSet", namespace=ns)[[1]]

# Query to fetch all DataSet/Definition elements
nlist = xpathSApply(root, "//d:DataSet/ds:Definition", namespaces=ns)

# Querry to fetch all columnId values for an attribute from a list of returned nodes
colnames=lapply(nlist, xmlSApply, function(x) xmlGetAttr(x, name="columnId"))


# Find name of all child nodes 
lapply(xmlChildren(doc), xmlName)

#=======================================================================

# Pass in first Table 


tempNode = xpathApply(Node, paste("/", xmlName(Node), "/PopulationEstimates/MLE/*"), namespaces=ns)





#'
#' Return a list of all XML elemtents matching the expression
#' 

"FindAll" <- function(nodeObj, tagName) {

  stopifnot(class(nodeObj[[1]])[1] == "XMLNode")
    
  ns = xmlNamespaceDefinitions(nodeObj, simplify = TRUE)
  defaultNS = 'd'
  names(ns)[1] <- defaultNS
  
  nodes = getNodeSet(doc, paste0("//", defaultNS, ":", tagName), namespaces = ns)
  
  return(nodes)
}

#'
#' Return a list of all XML elemtents matching the expression
#' 

"FindAllChildren" <- function(nodeObj, parentNode, tagName) {
  
  stopifnot(class(nodeObj[[1]])[1] == "XMLNode")

  ns = xmlNamespaceDefinitions(nodeObj, simplify = TRUE)
  defaultNS = 'd'
  names(ns)[1] <- defaultNS
  
  #Search For parent Node
  parentNodes = getNodeSet(doc, paste0("//", defaultNS, ":", parentNode), namespaces = ns)

  
  parentNodes = getNodeSet(doc, paste0("//", defaultNS, ":", parentNode), namespaces = ns)
  
  
  return(nodes)
}


doc = xmlRoot(xmlTreeParse("warfarin_PK_ODE_SO.xml"))

nlist = FindAll(doc, 'Table')


"ParseDataSet" <- function() {}



# xpathSApply(doc, "//SOBlock/*", function(x) as.numeric(xmlValue(x)), res)


f = system.file("warfarin_PK_ODE_SO.xml")
doc = xmlParse(f)

ns = xmlNamespaceDefinitions(xmlRoot(doc), simplify = TRUE)
defaultNS = 'd'
names(ns)[1] <- defaultNS

dd = xmlToDataFrame(getNodeSet(doc, "//DataSet/Table", namespace=ns))


root = xmlRoot(xmlTreeParse("warfarin_PK_ODE_SO.xml"))
f = system.file("warfarin_PK_ODE_SO.xml")
doc = xmlParse(f)


ns = xmlNamespaceDefinitions(root, simplify = TRUE)
defaultNS = 'd'
names(ns)[1] <- defaultNS

dd = xmlToDataFrame(nodes=getNodeSet(doc, "//d:DataSet/ds:Table", namespace=ns)[1])
