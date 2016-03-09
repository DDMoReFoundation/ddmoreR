
# ====================== #
# Standard Output Object #
# ====================== #
# 
# This file contains all code for the top level S4 class of StandardOutputObject.
# Classes for individual slots within the StandardOutputObject class structure are
# stored in separate files, named xxx-Class.R.
#
# Author: cmusselle, ccampbell, mwise


###############################################################################
#' The Standard Output Object Class (S4) 
#'
#' Contains slots having specific classes for each SO section.
#'
#' @slot ToolSettings A named list containing the settings relating to the execution tool,
#' 					  specifically, relevant file paths
#' @slot RawResults An object of S4 class \linkS4class{RawResults}
#' @slot TaskInformation An object of S4 class \linkS4class{TaskInformation} containing
#' 						 output messages from the task that was executed
#' @slot Estimation An object of S4 class \linkS4class{Estimation}, housing all data
#' 					associated with population and individual estimates, precision,
#' 					residuals, predictions, likelihoods and output messages
#' 					(and error messages) from individual modelling software 
#' @slot ModelDiagnostic An object of S4 class \linkS4class{ModelDiagnostic}
#' 						 housing all data associated with model diagnostics
#' 						 and model comparisons
#' @slot Simulation A list of objects of S4 class \linkS4class{SimulationBlock}
#' 					housing data produced from a simulation task
#' @slot OptimalDesign A list of objects of S4 class \linkS4class{OptimalDesignBlock}
#'					   housing results produced by Optimal Design
#' @slot .pathToSourceXML (Internal) The absolute path to the XML file from which
#' 						  this SO object was created
#' 
#' @author cmusselle

setClass("StandardOutputObject", 
	slots = c(
		ToolSettings = "list", 
		RawResults = "RawResults",
		TaskInformation = "TaskInformation",
		Estimation = "Estimation",
		ModelDiagnostic = "ModelDiagnostic",
		Simulation = "list", # of SimulationBlock objects
		OptimalDesign = "list", # of OptimalDesignBlock objects
		# The absolute path to the XML file that was parsed to create this SO
		.pathToSourceXML = "character"
	),
	# Validity Checking Function 
	validity = function(object) {
		stopifnot(class(object@ToolSettings) == "list")
		stopifnot(class(object@RawResults) == "RawResults")
		stopifnot(class(object@TaskInformation) == "TaskInformation")
		stopifnot(class(object@Estimation) == "Estimation")
		stopifnot(class(object@ModelDiagnostic) == "ModelDiagnostic")
		stopifnot(class(object@Simulation) == "list")
		stopifnot(class(object@OptimalDesign) == "list")
		return(TRUE)
	}
)

#' Initialisation function / Constructor for \linkS4class{StandardOutputObject} S4 class.
#' Process an SOBlock element and its sub-tree of elements, from the SO XML tree, and populate
#' a \linkS4class{StandardOutputObject} object from the data contained within.
#' 
#' @param .Object new instance of the class
#' @param xmlNodeSOBlock XML Node representation of the block
#' @include StandardOutputObject.R
#' @include Estimation-Class.R
#' @include Simulation-Class.R
#' @include OptimalDesign-Class.R
#' @include ModelDiagnostic-Class.R
#' @include RawResults-Class.R
#' @include TaskInformation-Class.R
#' @include StandardOutputObjectXmlParsers.R
setMethod("initialize", "StandardOutputObject", function(.Object, xmlNodeSOBlock = NULL) {

	# Fetch all Components of the SO object that are defined
	soChildren <- .getChildNodes(xmlNodeSOBlock)
	
	messageList <- list(parsed=list(), skipped=list())
	
	# Error checking of unexpected elements
	expectedTags <- grep(pattern = '^[^\\.]', x = slotNames(.Object), value = TRUE) # Slots of SO class excluding those we want to treat as hidden i.e. .pathToSourceXML
	unexpected <- setdiff(names(soChildren), expectedTags)
	if (length(unexpected) != 0) {
		warning(paste("The following unexpected elements were detected in the PharmML SO. These will be ignored.", 
						paste(unexpected, collapse="\n      "), sep="\n      "))
	}
	
	# Error checking of expected XML structure + Parser Execution
	if ("ToolSettings" %in% names(soChildren)) {
		
		# Extract child tags and values as a list with names = tag names and elements = tag values
		xmlNodeToolSettings <- soChildren[["ToolSettings"]]
		.Object@ToolSettings <- xmlApply(xmlNodeToolSettings, xmlValue)
		names(.Object@ToolSettings) <- xmlApply(xmlNodeToolSettings, function(x) { xmlAttrs(x)[["oid"]] })
		
		messageList[["parsed"]] <- append(messageList[["parsed"]], "ToolSettings")
	} else {
		messageList[["skipped"]] <- append(messageList[["skipped"]], "ToolSettings")
	}
	
	if ("RawResults" %in% names(soChildren)) {
		.Object@RawResults <- new (Class = "RawResults", .getChildNode(soChildren, "RawResults"))
		messageList[["parsed"]] <- append(messageList[["parsed"]], "RawResults")
	} else {
		messageList[["skipped"]] <- append(messageList[["skipped"]], "RawResults")
	}
	
	if ("TaskInformation" %in% names(soChildren)) {
		.Object@TaskInformation <- new (Class = "TaskInformation", .getChildNode(soChildren, "TaskInformation"))
		messageList[["parsed"]] <- append(messageList[["parsed"]], "TaskInformation")
	} else {
		messageList[["skipped"]] <- append(messageList[["skipped"]], "TaskInformation")
	}
	
	if ("Estimation" %in% names(soChildren)) {
		.Object@Estimation <- new (Class = "Estimation", .getChildNode(soChildren, "Estimation"))
		messageList[["parsed"]] <- append(messageList[["parsed"]], "Estimation")
	} else {
		messageList[["skipped"]] <- append(messageList[["skipped"]], "Estimation")
	}
	
	if ("Simulation" %in% names(soChildren)) {
		
		# Parse all Simulation Blocks within the Simulation node
		simulationBlockNodeList <- soChildren[["Simulation"]][names(soChildren[["Simulation"]]) == "SimulationBlock"]
		.Object@Simulation <- lapply(
				X = simulationBlockNodeList,
				FUN = function(xmlNodeSimulationBlock) { new (Class = "SimulationBlock", xmlNodeSimulationBlock = xmlNodeSimulationBlock) }
		)
		
		messageList[["parsed"]] <- append(messageList[["parsed"]], "Simulation")
	} else {
		messageList[["skipped"]] <- append(messageList[["skipped"]], "Simulation")
	}
	
	if ("OptimalDesign" %in% names(soChildren)) {
		
		# Parse all OptimalDesign Blocks within the Simulation node
		optimalDesignBlockNodeList <- soChildren[["OptimalDesign"]][names(soChildren[["OptimalDesign"]]) == "OptimalDesignBlock"]
		.Object@OptimalDesign <- lapply(
				X = optimalDesignBlockNodeList,
				FUN = function(xmlNodeOptimalDesignBlock) { new (Class = "OptimalDesignBlock", xmlNodeOptimalDesignBlock = xmlNodeOptimalDesignBlock) }
		)
		
		messageList[["parsed"]] <- append(messageList[["parsed"]], "OptimalDesign")	
	} else {
		messageList[["skipped"]] <- append(messageList[["skipped"]], "OptimalDesign")
	}
	
	if ("ModelDiagnostic" %in% names(soChildren)) {
		
		# Parse the ModelDiagnostic block
		.Object@ModelDiagnostic <- new (Class = "ModelDiagnostic", .getChildNode(soChildren, "ModelDiagnostic"))
		
		messageList[["parsed"]] <- append(messageList[["parsed"]], "ModelDiagnostic")	
	} else {
		messageList[["skipped"]] <- append(messageList[["skipped"]], "ModelDiagnostic")
	}

	# Run validation functions on S4 Class and subclasses
	# TODO Call top-level validation and it should cascade down
	validObject(.Object)
	validObject(.Object@RawResults)
	validObject(.Object@Estimation)
	validObject(.Object@Simulation)
	validObject(.Object@OptimalDesign)
	
	# Print parsed and skipped elements
	message(paste("\nThe following elements were parsed successfully:", 
			paste(messageList$parsed, collapse="\n      "), sep="\n      "))
	
	.Object
})


#' is.SOObject
#'
#' Determines if an object is of class "StandardOutputObject".
#'
#' @usage is.SOObject(object)
#'
#' @return TRUE or FALSE
#' @export
# Called from jobExecution.R
is.SOObject <- function(x) {
    is(object = x, class2 = "StandardOutputObject")
}
