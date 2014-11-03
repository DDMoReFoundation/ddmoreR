## ' LoadPharmML
## ' 
## ' SOObject <- LoadPharmML(SOObject, "warphrin_PK_ODE_SO_FULL.xml")
## '
## # ' @include StandardOutputSubClasses.R StandardOutputObject.R telClasses.R
## ' @export
## ' @docType methods
## ' @rdname LoadPharmML-methods
## setGeneric(name="LoadPharmML",
##            def=function(theObject,fileHandle)
##            {
##                standardGeneric("LoadPharmML")
##            }
## )
## #' Top level method to load in all SO components at once
## #' 
## #' @rdname LoadPharmML-methods
## #' @aliases LoadPharmML,LoadPharmML-method
## setMethod(f="LoadPharmML", signature="StandardOutputObject", 
##   definition=function(SOObject, fileHandle){
#  	
##   	# Error Checking
##   	stopifnot(class(fileHandle) == "character" & file.exists(fileHandle))
#
##   	# Get a reference to the root node in the xml doc
## 	root = xmlRoot(xmlTreeParse("warfarin_PK_ODE_SO_FULL.xml"))
#
## 	# Update the namespace to use 'd' as the defualt. Must be done to use Xpath expressions with namespaces
## 	ns = xmlNamespaceDefinitions(root, simplify = TRUE)
## 	defaultNS = 'd'
## 	names(ns)[1] <- defaultNS
#	
## 	# Fetch List of SOBlock elements
## 	SOlist = xpathApply(root, "//d:SOBlock", namespaces=ns)
#
## 	# Assumes only one SOBlock for now!
## 	stopifnot(length(SOlist) == 1)
#
## 	### Future for loop to start here
#
## 	# Fetch all Components of the SO object that are defined
## 	SOChildren = xmlChildren(SOlist[[1]])
#
## 	# Execute Parsers
## 	SOObject = ParseToolSettings(SOObject, SOChildren[["ToolSettings"]])
## 	SOObject = ParseRawResults(SOObject, SOChildren[["RawResults"]])
## 	SOObject = ParsePopulationEstimates(SOObject, SOChildren[["Estimation"]][["PopulationEstimates"]])
## 	SOObject = ParsePrecisionPopulationEstimates(SOObject, SOChildren[["Estimation"]][["PrecisionPopulationEstimates"]])
## 	SOObject = ParseIndividualEstimates(SOObject, SOChildren[["Estimation"]][["IndividualEstimates"]])
## 	SOObject = ParseResiduals(SOObject, SOChildren[["Estimation"]][["Residuals"]])
## 	SOObject = ParsePredictions(SOObject, SOChildren[["Estimation"]][["Predictions"]])
#
## 	return(SOObject)
## })
#
#
#
#
## #' @rdname getModelObjects-methods
## #' @aliases getModelObjects,mogObj,mogObj-method
## setMethod(f="setCoordinate",
##                       signature="FirstQuadrant",
##                       definition=function(theObject,xVal,yVal)
##                       {
##                               theObject@x <- xVal
##                               theObject@y <- yVal
##                               return(theObject)
##                       }
##                       )
#
## # #' 
## # #'
## # setGeneric("getTaskPropertiesObjects", function(x, name, HOST='localhost', PORT='9010') { 
## #   # create object in R from parser:
## #   res <- .parseMDLFile(x, type="taskobj", HOST=HOST, PORT=PORT)
#  
## #   return(res)
## # })
#
#
## # #' @rdname getTaskPropertiesObjects-methods
## # #' @aliases getTaskPropertiesObjects,mogObj,mogObj-method
## # setMethod("getTaskPropertiesObjects", signature=signature(x="mogObj"), function(x){
## #    return(x@taskObj)
## # })