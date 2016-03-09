
#' The SimulationBlock Object Class (S4) 
#'
#' An object to house all data associated with a Simulation Block.
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
#' @include StandardOutputObjectXmlParsers.R

setClass(Class = "SimulationBlock",
	slots = c(
		replicate = "integer",
		SimulatedProfiles = "list",
		IndivParameters = "DataSet",
		RandomEffects = "DataSet",
		Covariates = "DataSet",
		Regressors = "DataSet",
		PopulationParameters = "DataSet",
		Dosing = "DataSet",
		RawResultsFile = "character"
	),
	validity = function(object) {
		# TODO implement checking
		return(TRUE)
	}
)

#' Initialisation function / Constructor for SimulationBlock S4 class
#' @param .Object new instance of the class
#' @param xmlNodeSimulationBlock XML Node representation of the block
#' @include StandardOutputObjectXmlParsers.R
setMethod("initialize", "SimulationBlock", function(.Object, xmlNodeSimulationBlock = NULL) {
	
	if (!is.null(xmlNodeSimulationBlock)) {
		
		.Object <- .genericParseElements(.Object, xmlNodeSimulationBlock, customParseChildNodeNames = c("SimulatedProfiles", "RawResultsFile"))
		
		.Object@replicate <- as.integer(xmlAttrs(xmlNodeSimulationBlock)[["replicate"]])
		
		# Custom parsing of child nodes that aren't simply handled by parseSODataElement()
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
#' An object to house all data associated with Simulation Block Simulated Profiles.
#' 
#' @slot name string
#' @slot extFileNo integer
#' @slot DataSet DataSet object
#' 
#' @name SimulatedProfiles-class
#' @rdname SimulatedProfiles-class
#' @exportClass SimulatedProfiles
#' @aliases SimulatedProfiles
#' @examples
#' sp <- new(Class = "SimulatedProfiles")
#' print(sp)
#' validObject(sp)
#' 
#' @include StandardOutputObjectXmlParsers.R

setClass(Class = "SimulatedProfiles",
	slots = c(
		name = "character",
		extFileNo = "integer",
		DataSet = "DataSet"
	),
	validity = function(object) {
		# TODO implement checking
		return(TRUE)
	}
)

#' Initialisation function / Constructor for SimulatedProfiles S4 class
#' @param .Object new instance of the class
#' @param xmlNodeSimulatedProfiles XML Node representation of the block
#' @include StandardOutputObjectXmlParsers.R
setMethod("initialize", "SimulatedProfiles", function(.Object, xmlNodeSimulatedProfiles = NULL) {
	
	if (!is.null(xmlNodeSimulatedProfiles)) {
		spAttrs <- xmlAttrs(xmlNodeSimulatedProfiles)
		for (spAttrName in names(spAttrs)) {
			switch (spAttrName,
				"name" = slot(.Object, spAttrName) <- spAttrs[[spAttrName]],
				"extFileNo" = slot(.Object, spAttrName) <- as.integer(spAttrs[[spAttrName]])
			)
		}
		.Object@DataSet <- parseSODataElement(xmlNodeSimulatedProfiles)
	}
	
	.Object
})

