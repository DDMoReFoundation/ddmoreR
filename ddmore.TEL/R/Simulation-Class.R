
#' The SimulationBlock Object Class (S4) 
#'
#' An object to house all data associated with a Simulation Block
#' 
#' @slot replicate integer
#' @slot SimulatedProfiles list of SimulatedProfiles objects
#' @slot IndivParameters DataSet object
#' @slot RandomEffects DataSet object
#' @slot Covariates DataSet object
#' @slot Regressors DataSet object
#' @slot PopulationParameters DataSet object
#' @slot Dosing DataSet object
#' @slot RawResultsFile file path as a string
#' 
#' @name SimulationBlock-class
#' @rdname SimulationBlock-class
#' @exportClass SimulationBlock
#' @aliases SimulationBlock
#' @examples
#' sb <- new(Class = "SimulationBlock")
#' print(sb)
#' validObject(sb)
#' 
#' @include xmlParsers.R

setClass(Class = "SimulationBlock",
	slots = c(
		"replicate", "SimulatedProfiles", "IndivParameters", "RandomEffects",
		"Covariates", "Regressors", "PopulationParameters", "Dosing", "RawResultsFile"),
	# TODO implement nested classes
	prototype = list(
			replicate = integer(0),
			SimulatedProfiles = list(),
			IndivParameters = DataSet(),
			RandomEffects = DataSet(),
			Covariates = DataSet(),
			Regressors = DataSet(),
			PopulationParameters = DataSet(),
			Dosing = DataSet(),
			RawResultsFile = character(0)),
	validity = function(object) {
		# TODO implement checking
    	return(TRUE)
	}
)

#' Initialisation function / Constructor for SimulationBlock S4 class
#' @param .Object new instance of the class
#' @param xmlNodeSimulationBlock XML Node representation of the block
#' @include xmlParsers.R
setMethod("initialize", "SimulationBlock", function(.Object, xmlNodeSimulationBlock = NULL) {
	
	if (!is.null(xmlNodeSimulationBlock)) {
		
		.Object <- .genericParseElements(.Object, xmlNodeSimulationBlock, customParseChildNodeNames = c("SimulatedProfiles", "RawResultsFile"))
		
		.Object@replicate <- xmlAttrs(xmlNodeSimulationBlock)[["replicate"]]
		
		# Custom parsing of child nodes that aren't simply handled by ParseElement()
		for (child in .getChildNodes(xmlNodeSimulationBlock)) {
			childName <- xmlName(child)
			if (childName == "SimulatedProfiles") {
				# Append each SimulatedProfile to the end of the list
				slot(.Object, childName) <- as.list(c(slot(.Object, childName), new (Class = "SimulatedProfiles", xmlNodeSimulatedProfiles = child)))
			} else if (childName == "RawResultsFile") {
				.Object@RawResultsFile <- xmlValue(.getChildNode(.getChildNodes(child), "path"))
			}
		}
	}

	.Object
})


#' The SimulatedProfiles Object Class (S4) 
#'
#' An object to house all data associated with Simulation Block Simulated Profiles
#' 
#' @slot name string
#' @slot extFileNo integer
#' @slot DataSet DataSet object
#' @name SimulatedProfiles-class
#' @rdname SimulatedProfiles-class
#' @exportClass SimulatedProfiles
#' @aliases SimulatedProfiles
#' @examples
#' sp <- new(Class = "SimulatedProfiles")
#' print(sp)
#' validObject(sp)
#' 
#' @include xmlParsers.R

setClass(Class = "SimulatedProfiles",
	slots = c(
			"name", "extFileNo", "DataSet"),
	# TODO implement nested classes
	prototype = list(
			name = character(0),
			extFileNo = integer(0),
			DataSet = DataSet()),
	validity = function(object) {
		# TODO implement checking
		return(TRUE)
	}
)

#' Initialisation function / Constructor for SimulatedProfiles S4 class
#' @param .Object new instance of the class
#' @param xmlNodeSimulatedProfiles XML Node representation of the block
#' @include xmlParsers.R
setMethod("initialize", "SimulatedProfiles", function(.Object, xmlNodeSimulatedProfiles = NULL) {
	
	if (!is.null(xmlNodeSimulatedProfiles)) {
		spAttrs <- xmlAttrs(xmlNodeSimulatedProfiles)
		for (spAttrName in names(spAttrs)) {
			if (spAttrName %in% c("name", "extFileNo")) {
				slot(.Object, spAttrName) <- spAttrs[[spAttrName]]
			}
		}
		.Object@DataSet <- ParseElement(xmlNodeSimulatedProfiles)
	}
	
	.Object
})


