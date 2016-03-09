

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
	slots = c("IndivObservationPrediction", "VPC"),
	prototype = list(
		IndivObservationPrediction = DataSet(),
		VPC = DataSet()
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
	slots = c("RandomEffects", "IndivParamsCovariates", "DistributionIndivParams"),
	prototype = list(
		RandomEffects = DataSet(),
		IndivParamsCovariates = DataSet(),
		DistributionIndivParams = DataSet()
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
	slots = c("DiagnosticStructuralModel", "DiagnosticIndividualParams"),
	prototype = list(
		DiagnosticStructuralModel = new (Class = "DiagnosticStructuralModel"),
		DiagnosticIndividualParams = new (Class = "DiagnosticIndividualParams")),
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
