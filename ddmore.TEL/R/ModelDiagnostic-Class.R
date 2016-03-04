

#' The DiagnosticStructuralModel Object Class (S4) 
#'
#' An object to house all data associated with Diagnostic Structural Model
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

setClass(Class = "DiagnosticStructuralModel",
	slots = c("IndivObservationPrediction", "VPC"),
	prototype = list(
			IndivObservationPrediction = DataSet(),
			VPC = DataSet()),
	validity = function(object) {
		# TODO implement checking
		return(TRUE)
	}
)

setMethod("initialize", "DiagnosticStructuralModel", function(.Object, xmlNodeDiagnosticStructuralModel = NULL) {
			
	if (!is.null(xmlNodeDiagnosticStructuralModel)) {
		for (child in .getChildNodes(xmlNodeDiagnosticStructuralModel)) {
			childName <- xmlName(child)
			if (childName %in% slotNames(.Object)) {
				slot(.Object, childName) <- ParseElement(child)
			} else {
				warning(paste("Unexpected child node of DiagnosticStructuralModel node encountered: ", childName))
			}
			
		} # end for
	}
	
	.Object
})


#' The DiagnosticIndividualParams Object Class (S4) 
#'
#' An object to house all data associated with Diagnostic Individual Parameters
#' 
#' @slot RandomEffects DataSet object
#' @slot IndivParamsCovariates DataSet object
#' @slot DistributionIndivParams TODO Sample _or_ Table
#' 
#' @name DiagnosticIndividualParams-class
#' @rdname DiagnosticIndividualParams-class
#' @exportClass DiagnosticIndividualParams
#' @aliases DiagnosticIndividualParams
#' @examples
#' dips <- new(Class = "DiagnosticIndividualParams")
#' print(dips)
#' validObject(dips)

setClass(Class = "DiagnosticIndividualParams",
	slots = c("RandomEffects", "IndivParamsCovariates", "DistributionIndivParams"),
	prototype = list(
			RandomEffects = DataSet(),
			IndivParamsCovariates = DataSet(),
			DistributionIndivParams = DataSet()), # TODO Sample _or_ Table
	validity = function(object) {
		# TODO implement checking
		return(TRUE)
	}
)

setMethod("initialize", "DiagnosticIndividualParams", function(.Object, xmlNodeDiagnosticIndividualParams = NULL) {

	if (!is.null(xmlNodeDiagnosticIndividualParams)) {
		for (child in .getChildNodes(xmlNodeDiagnosticIndividualParams)) {
			childName <- xmlName(child)
			if (childName %in% slotNames(.Object)) {
				slot(.Object, childName) <- ParseElement(child)
			} else {
				warning(paste("Unexpected child node of DiagnosticIndividualParams node encountered: ", childName))
			}
		} # end for
	}
	
	.Object
})



#' The ModelDiagnostic Object Class (S4) 
#'
#' An object to house all data associated with Model Diagnostic
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

setClass(Class = "ModelDiagnostic",
	slots = c("DiagnosticStructuralModel", "DiagnosticIndividualParams"),
	prototype = list(
			DiagnosticStructuralModel = new (Class="DiagnosticStructuralModel"),
			DiagnosticIndividualParams = new (Class="DiagnosticIndividualParams")),
	validity = function(object) {
		# TODO implement checking
		return(TRUE)
	}
)

ModelDiagnostic <- function(xmlNodeModelDiagnostic = NULL, ...) {
    newObj <- new(Class = "ModelDiagnostic", ...)
	
	if (!is.null(xmlNodeModelDiagnostic)) {
		for (child in .getChildNodes(xmlNodeModelDiagnostic)) {
			childName <- xmlName(child)
			if (childName %in% slotNames(newObj)) {
				slot(newObj, childName) <- new(Class = childName, child)
			} else {
				warning(paste("Unexpected child node of ModelDiagnostic node encountered: ", childName))
			}
			
		} # end for
	}
	
	newObj
}
