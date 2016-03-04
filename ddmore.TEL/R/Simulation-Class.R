
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
#' @slot RawResultsFile path as a string
#' 
#' @name SimulationBlock-class
#' @rdname SimulationBlock-class
#' @exportClass SimulationBlock
#' @aliases SimulationBlock
#' @examples
#' sb <- new(Class = "SimulationBlock")
#' print(sb)
#' validObject(sb)

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

SimulationBlock <- function(xmlNodeSimulationBlock = NULL, ...) {
    newObj <- new(Class = "SimulationBlock", ...)

	if (!is.null(xmlNodeSimulationBlock)) {
		newObj@replicate <- xmlAttrs(xmlNodeSimulationBlock)[["replicate"]]
		for (child in .getChildNodes(xmlNodeSimulationBlock)) {
			childName <- xmlName(child)
			if (childName %in% c("SimulatedProfiles")) {
				# Add each SimulatedProfile to the end of the list
				slot(newObj, childName) <- c(slot(newObj, childName), SimulatedProfiles(child))
			} else if (childName %in% c(
					"IndivParameters", "RandomEffects", "Covariates", "Regressors", "PopulationParameters", "Dosing")) {
				# Table expected - TODO call specific function ?
				slot(newObj, childName) <- ParseElement(child)
			}
			else if (childName %in% c("RawResultsFile")) {
				newObj@RawResultsFile <- xmlValue(.getChildNode(.getChildNodes(child), "path"))
			}
			else {
				warning(paste("Unexpected child node of SimulationBlock node encountered: ", childName))
			}
		} # end for
	}

	newObj
}


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

SimulatedProfiles <- function(xmlNodeSimulatedProfiles = NULL, ...) {
    newObj <- new(Class = "SimulatedProfiles", ...)
	
	if (!is.null(xmlNodeSimulatedProfiles)) {
		spAttrs <- xmlAttrs(xmlNodeSimulatedProfiles)
		for (spAttrName in names(spAttrs)) {
			if (spAttrName %in% c("name", "extFileNo")) {
				slot(newObj, spAttrName) <- spAttrs[[spAttrName]]
			}
		}
		newObj@DataSet <- ParseElement(xmlNodeSimulatedProfiles)
	}
	
	newObj
}


