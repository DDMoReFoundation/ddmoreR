################################################################################
# Copyright (C) 2016 Mango Business Solutions Ltd, http://www.mango-solutions.com
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU Affero General Public License as published by the
# Free Software Foundation, version 3.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
# for more details.
#
# You should have received a copy of the GNU Affero General Public License along
# with this program. If not, see <http://www.gnu.org/licenses/agpl-3.0.html>.
################################################################################


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

	if (!is.null(xmlNodeSOBlock)) {
	
		# Fetch all Components of the SO object that are defined
		soChildren <- .getChildNodes(xmlNodeSOBlock)
		
		# Error checking of unexpected elements
        # Slots of SO class excluding those we want to treat as hidden i.e. .pathToSourceXML
		expectedTags <- grep(pattern = '^[^\\.]', x = slotNames(.Object), value = TRUE) 
		unexpected <- setdiff(names(soChildren), expectedTags)
		if (length(unexpected) != 0) {
			warning(paste("The following unexpected top-level elements were detected in the PharmML SO. These will be ignored.", 
				paste(unexpected, collapse="\n      "), sep="\n      "))
		}
		
		# Error checking of expected XML structure + Parser Execution
		if ("ToolSettings" %in% names(soChildren)) {
			# Extract child tags and values as a list with names = tag names and elements = tag values
			xmlNodeToolSettings <- soChildren[["ToolSettings"]]
			.Object@ToolSettings <- xmlApply(xmlNodeToolSettings, xmlValue)
			names(.Object@ToolSettings) <- xmlApply(xmlNodeToolSettings, function(x) { xmlAttrs(x)[["oid"]] })
		}
		if ("RawResults" %in% names(soChildren)) {
			.Object@RawResults <- new (Class = "RawResults", .getChildNode(soChildren, "RawResults"))
		}
		if ("TaskInformation" %in% names(soChildren)) {
			.Object@TaskInformation <- new (Class = "TaskInformation", .getChildNode(soChildren, "TaskInformation"))
		}
		if ("Estimation" %in% names(soChildren)) {
			.Object@Estimation <- new (Class = "Estimation", .getChildNode(soChildren, "Estimation"))
		}
		if ("Simulation" %in% names(soChildren)) {
			# Parse all Simulation Blocks within the Simulation node
			simulationBlockNodeList <- soChildren[["Simulation"]][names(soChildren[["Simulation"]]) == "SimulationBlock"]
			.Object@Simulation <- lapply(
					X = simulationBlockNodeList,
					FUN = function(xmlNodeSimulationBlock) { 
                        new (Class = "SimulationBlock", xmlNodeSimulationBlock = xmlNodeSimulationBlock) }
			)
		}
		if ("OptimalDesign" %in% names(soChildren)) {
			# Parse all OptimalDesign Blocks within the Simulation node
			optimalDesignBlockNodeList <- soChildren[["OptimalDesign"]][names(soChildren[["OptimalDesign"]]) == "OptimalDesignBlock"]
			.Object@OptimalDesign <- lapply(
					X = optimalDesignBlockNodeList,
					FUN = function(xmlNodeOptimalDesignBlock) { 
                        new (Class = "OptimalDesignBlock", xmlNodeOptimalDesignBlock = xmlNodeOptimalDesignBlock) }
			)
		}
		if ("ModelDiagnostic" %in% names(soChildren)) {
			# Parse the ModelDiagnostic block
			.Object@ModelDiagnostic <- new (Class = "ModelDiagnostic", .getChildNode(soChildren, "ModelDiagnostic"))
		}

	}
	
	# Run validation functions on S4 Class and subclasses
	# TODO Call top-level validation and it should cascade down
	validObject(.Object)
	validObject(.Object@RawResults)
	validObject(.Object@Estimation)
	validObject(.Object@Simulation)
	validObject(.Object@OptimalDesign)
	
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
