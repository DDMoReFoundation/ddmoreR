
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
		SimulatedProfiles = "list", # of SimulationDataSet objects
		IndivParameters = "list", # of SimulationDataSet objects
		RandomEffects = "list", # of SimulationDataSet objects
		Covariates = "list", # of SimulationDataSet objects
		Regressors = "list", # of SimulationDataSet objects
		PopulationParameters = "list", # of SimulationDataSet objects
		Dosing = "list", # of SimulationDataSet objects
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
		
		.Object@replicate <- as.integer(xmlAttrs(xmlNodeSimulationBlock)[["replicate"]])
		
		for (child in .getChildNodes(xmlNodeSimulationBlock)) {
			childName <- xmlName(child)
			if (childName == "RawResultsFile") {
				.Object@RawResultsFile <- xmlValue(.getChildNode(.getChildNodes(child), "path"))
			}
			else if (childName %in% slotNames(.Object)) {
				# Append each SimulationDataSet to the end of the appropriate list
				slot(.Object, childName) <- append(slot(.Object, childName), new (Class = "SimulationDataSet", child))
			}
			else {
				warning(paste("Unexpected child node of SimulationBlock node encountered:", childName))
			}
		}
	}

	.Object
})

# Simplify the indication of the slots that are populated for SimulationBlock object,
# to essentially a yes/no for each of the lists of SimulationDataSet objects overall,
# rather than explicitly listing each element of each list
setMethod("getPopulatedSlots", "SimulationBlock", function(object) {
	res <- NULL
	for (slotName in slotNames(object)) {
		nestedObj <- slot(object, slotName)
		if (class(nestedObj) == "list") {
			populatedSubSlots <- getPopulatedSlots(nestedObj)
			if (length(populatedSubSlots) == 1) {
				res <- append(res, paste0(slotName, "[1]"))
			} else if (length(populatedSubSlots) > 1) {
				res <- append(res, paste0(slotName, "[1..", length(populatedSubSlots), "]"))
			}
		}
	}
	res
})


#' The SimulationDataSet Object Class (S4) 
#'
#' An object to house all data associated with the following Simulation Block nested blocks:
#' SimulatedProfiles, IndivParameters, RandomEffects, Covariates, Regressors, PopulationParameters, Dosing.
#' 
#' For the purposes of representation within the SO class structure, this is a \linkS4class{DataSet}
#' class augmented with optional name and extFileNo attributes.
#' 
#' @slot name string
#' @slot extFileNo integer
#' @slot description (Optional) dataframe containing column header information for dataset
#' @slot data (Optional) matrix containing actual data with one column for each column in description
#' 
#' @name SimulationDataSet-class
#' @rdname SimulationDataSet-class
#' @exportClass SimulationDataSet
#' @aliases SimulationDataSet
#' @examples
#' sds <- new(Class = "SimulationDataSet")
#' print(sds)
#' validObject(sds)
#' 
#' @include StandardOutputObjectXmlParsers.R

setClass(Class = "SimulationDataSet",
	contains = "DataSet", # superclass
	slots = c(
		name = "character",
		extFileNo = "integer"
	),
	validity = function(object) {
		# TODO implement this checking
		return(TRUE)
	}
)

#' Initialisation function / Constructor for \linkS4class{SimulationDataSet} S4 class,
#' subclass of \linkS4class{DataSet} S4 class.
#' 
#' @param .Object new instance of the class
#' @param xmlNodeSimulationDataSet XML Node representation of the block
#' 
#' @slot description (Optional) dataframe containing column header information for dataset
#' @slot data (Optional) matrix containing actual data with one column for each column in description
#' 
#' @include StandardOutputObjectXmlParsers.R
setMethod("initialize", "SimulationDataSet", function(.Object, xmlNodeSimulationDataSet) {
	if (!is.null(xmlNodeSimulationDataSet)) {
		spAttrs <- xmlAttrs(xmlNodeSimulationDataSet)
		for (spAttrName in names(spAttrs)) {
			switch (spAttrName,
					"name" = slot(.Object, spAttrName) <- spAttrs[[spAttrName]],
					"extFileNo" = slot(.Object, spAttrName) <- as.integer(spAttrs[[spAttrName]])
			)
		}
		dataSet <- parseSODataElement(xmlNodeSimulationDataSet)
		callNextMethod(.Object, dataSet@description, dataSet@data)
	}
})

