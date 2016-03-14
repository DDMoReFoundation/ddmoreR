

#' The DiagnosticStructuralModel Object Class (S4) 
#'
#' An object to house all data associated with Diagnostic Structural Model.
#' 
#' @slot IndivObservationPrediction DataSet object
#' @slot VPC DataSet object
#' 
#' @name DiagnosticStructuralModel-class
#' @rdname DiagnosticStructuralModel-class
#' @exportClass DiagnosticStructuralModel
#' @aliases DiagnosticStructuralModel
#' @examples
#' dsm <- new(Class = "DiagnosticStructuralModel")
#' print(dsm)
#' validObject(dsm)
#' 
#' @include StandardOutputObjectXmlParsers.R

setClass(Class = "DiagnosticStructuralModel",
	slots = c(
		IndivObservationPrediction = "DataSet",
		VPC = "DataSet"
	),
	validity = function(object) {
		# TODO implement checking
		return(TRUE)
	}
)

#' Initialisation function / Constructor for DiagnosticStructuralModel S4 class
#' @param .Object new instance of the class
#' @param xmlNodeDiagnosticStructuralModel XML Node representation of the block
#' @include StandardOutputObjectXmlParsers.R
setMethod("initialize", "DiagnosticStructuralModel", function(.Object, xmlNodeDiagnosticStructuralModel = NULL) {
	.genericParseElements(.Object, xmlNodeDiagnosticStructuralModel)
})

# Simplify the indication of the slots that are populated for DiagnosticStructuralModel
# object, to essentially a yes/no as to whether any of the slots are populated
setMethod("getPopulatedSlots", "DiagnosticStructuralModel", function(object) {
	if (length(callNextMethod(object)) > 0) list()
})


#' The DiagnosticIndividualParams Object Class (S4) 
#'
#' An object to house all data associated with Diagnostic Individual Parameters.
#' 
#' @slot RandomEffects DataSet object
#' @slot IndivParamsCovariates DataSet object
#' @slot DistributionIndivParams DataSet or DataSetDistribution object
#' 
#' @name DiagnosticIndividualParams-class
#' @rdname DiagnosticIndividualParams-class
#' @exportClass DiagnosticIndividualParams
#' @aliases DiagnosticIndividualParams
#' @examples
#' dips <- new(Class = "DiagnosticIndividualParams")
#' print(dips)
#' validObject(dips)
#' 
#' @include StandardOutputObjectXmlParsers.R

setClass(Class = "DiagnosticIndividualParams",
	slots = c(
		RandomEffects = "DataSet",
		IndivParamsCovariates = "DataSet",
		DistributionIndivParams = "DataSet" # or DataSetDistribution
	),
	validity = function(object) {
		# TODO implement checking
		return(TRUE)
	}
)

#' Initialisation function / Constructor for DiagnosticIndividualParams S4 class
#' @param .Object new instance of the class
#' @param xmlNodeDiagnosticIndividualParams XML Node representation of the block
#' @include StandardOutputObjectXmlParsers.R
setMethod("initialize", "DiagnosticIndividualParams", function(.Object, xmlNodeDiagnosticIndividualParams = NULL) {
	.genericParseElements(.Object, xmlNodeDiagnosticIndividualParams)
})

# Simplify the indication of the slots that are populated for DiagnosticIndividualParams
# object, to essentially a yes/no as to whether any of the slots are populated
setMethod("getPopulatedSlots", "DiagnosticIndividualParams", function(object) {
	if (length(callNextMethod(object)) > 0) list()
})


#' The ModelDiagnostic Object Class (S4) 
#'
#' An object to house all data associated with Model Diagnostic.
#' 
#' @slot DiagnosticStructuralModel object
#' @slot DiagnosticIndividualParams object
#' 
#' @name ModelDiagnostic-class
#' @rdname ModelDiagnostic-class
#' @exportClass ModelDiagnostic
#' @aliases ModelDiagnostic
#' @examples
#' md <- new(Class = "ModelDiagnostic")
#' print(md)
#' validObject(md)
#' 
#' @include StandardOutputObjectXmlParsers.R

setClass(Class = "ModelDiagnostic",
	slots = c(
		DiagnosticStructuralModel = "DiagnosticStructuralModel",
		DiagnosticIndividualParams = "DiagnosticIndividualParams"
	),
	validity = function(object) {
		# TODO implement checking
		return(TRUE)
	}
)

setMethod("initialize", "ModelDiagnostic", function(.Object, xmlNodeModelDiagnostic = NULL) {
	
	if (!is.null(xmlNodeModelDiagnostic)) {
		for (child in .getChildNodes(xmlNodeModelDiagnostic)) {
			childName <- xmlName(child)
			if (childName %in% slotNames(.Object)) {
				slot(.Object, childName) <- new(Class = childName, child)
			} else {
				warning(paste("Unexpected child node of ModelDiagnostic node encountered: ", childName))
			}
			
		} # end for
	}
	
	.Object
})
