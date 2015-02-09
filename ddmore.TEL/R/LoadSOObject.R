################################################################################
#' LoadSOObject
#'
#' Parse in the SO Object of a PharmML .SO.xml file and return it as a
#' new \link{StandardOutputObject} representing the single individual SOBlock
#' contained within the top-level SO element.
#' 
#' If there are multiple SOBlocks present in the file then this method will
#' throw an error; the plural version of this function, \link{LoadSOObjects},
#' must be used instead.
#'
#' @param file The relative path to the .SO.xml file
#' @return Returns a newly created instance of the \link{StandardOutputObject} class
#'         populated with the data from the single SOBlock section of the PharmML file
#' 
#' @export
#' @include StandardOutputObject.R xmlParsers.R 
LoadSOObject <- function(file) {
	
	root <<- validateAndLoadXMLSOFile(file)
	soBlocks <- root[names(root) == "SOBlock"]
	if (length(soBlocks) != 1) {
		stop("LoadSOObject() is used for the case where there is known to be exactly one SOBlock in the SO XML file;\n",
			 "  use the plural version of the function, LoadSOObjects(), instead if there are multiple SOBlocks in the file.")
	}
	
	# Set working directory to that specified in file
	# (I (MSW) don't like this, can't we amend the xmlParsers.R to resolve the associated data files
	# relative to the directory in which the .SO.xml file lives, but remaining in the current directory?)
	old.wd <- getwd()
	setwd(dirname(file))
	
	SOObject <- createSOObjectFromXMLSOBlock(soBlocks[[1]])
	
	# Reset Working directory 
	setwd(old.wd)
	
	SOObject
}

################################################################################
#' LoadSOObjects
#'
#' Parse in the SO Object of a PharmML .SO.xml file and return it as a list of
#' new \link{StandardOutputObject}s representing the individual SOBlocks
#' contained within the top-level SO element.
#' 
#' If it is known that there is only one SOBlock present in the file then the
#' alternative function \link{LoadSOObject} can be used instead.
#'
#' @param file The relative path to the .SO.xml file
#' @return Returns a list of newly created instances of the \link{StandardOutputObject} class
#'         populated with the data from the individual SOBlock sections of the PharmML file
#' 
#' @export
#' @include StandardOutputObject.R xmlParsers.R 
LoadSOObjects <- function(file) {

	root <- validateAndLoadXMLSOFile(file)
	
	# Set working directory to that specified in file
	# (I (MSW) don't like this, can't we amend the xmlParsers.R to resolve the associated data files
	# relative to the directory in which the .SO.xml file lives, but remaining in the current directory?)
	old.wd <- getwd()
	setwd(dirname(file))

  # Fetch List of SOBlock elements
  SOBlockList <- root[names(root) == "SOBlock"]
  soObjNames <- make.names(lapply(SOBlockList, function(soBlock) {
	# Use the blkId as the name of the SOBlock in the named list, for want of a better name
	# (the default is "SOBlock" which is repeated for all elements)
	soBlock$attributes["blkId"]
  }))

  SOObjectList <- lapply(SOBlockList, createSOObjectFromXMLSOBlock)

  # Reset Working directory 
  setwd(old.wd)

  names(SOObjectList) <- soObjNames
  SOObjectList
}

# Check that the .SO.xml file exists; if so then parse the XML document and return a reference to the root node.
validateAndLoadXMLSOFile <- function(file) {
	
	# Error checking
	if (!(class(file) == "character" && file.exists(file))) {
		stop("Standard Output results file ", file, " does not exist.")
	}
	
	# Return a reference to the root node in the XML doc
	xmlRoot(xmlTreeParse(file))
}

# Process an SOBlock element from the SO XML tree and populate a StandardOutputObject object from the data contained within.
createSOObjectFromXMLSOBlock <- function(soBlock) {
	
	# Generate Blank SO object
	SOObject <- createSOObject()
	
	# Fetch all Components of the SO object that are defined
	SOChildren <- xmlChildren(soBlock)
	
	# Error Checking of unexpected elements
	expectedTags = c("ToolSettings", "RawResults", "TaskInformation", "Estimation", 
			"Simulation")
	unexpected = setdiff(names(SOChildren), expectedTags)
	if (length(unexpected) != 0) {
		warning(paste("The following unexpected elements were detected in the PharmML SO.", 
						paste(unexpected, collapse="\n      "), sep="\n      "))
	}
	
	# Error checking of expected XML structure + Parser Execution
	if ("ToolSettings" %in% names(SOChildren)){
		SOObject <- ParseToolSettings(SOObject, SOChildren[["ToolSettings"]])
	} else {
		message("ToolSettings element not detected in PharmML. Skipping...")
	}
	
	if ("RawResults" %in% names(SOChildren)){
		SOObject <- ParseRawResults(SOObject, SOChildren[["RawResults"]])
	} else {
		message("RawResults element not detected in PharmML. Skipping...")
	}
	
	if ("TaskInformation" %in% names(SOChildren)){
		SOObject <- ParseTaskInformation(SOObject, SOChildren[["TaskInformation"]])
	} else {
		message("TaskInformation element not detected in PharmML. Skipping...")
	}
	
	if ("Estimation" %in% names(SOChildren)){
		
		# Error Checking of unexpected elements in Estimation Block
		expectedTags = c("PopulationEstimates", "PrecisionPopulationEstimates", 
				"IndividualEstimates", "PrecisionIndividualEstimates", "Residuals", 
				"Predictions", "Likelihood")
		unexpected = setdiff(names(SOChildren[["Estimation"]]), expectedTags)
		if (length(unexpected) != 0) {
			warning(paste("The following unexpected elements were detected in the Estimation block of the PharmML SO.", 
							paste(unexpected, collapse="\n      "), sep="\n      "))
		}
		
		if ("PopulationEstimates" %in% names(SOChildren[["Estimation"]])){
			SOObject <- ParsePopulationEstimates(SOObject, SOChildren[["Estimation"]][["PopulationEstimates"]])
		} else {
			message("PopulationEstimates element not detected in PharmML. Skipping...")
		}
		
		if ("PrecisionPopulationEstimates" %in% names(SOChildren[["Estimation"]])){
			SOObject <- ParsePrecisionPopulationEstimates(SOObject, SOChildren[["Estimation"]][["PrecisionPopulationEstimates"]])
		} else {
			message("PrecisionPopulationEstimates element not detected in PharmML. Skipping...")
		}
		
		if ("IndividualEstimates" %in% names(SOChildren[["Estimation"]])){
			SOObject <- ParseIndividualEstimates(SOObject, SOChildren[["Estimation"]][["IndividualEstimates"]])
		} else {
			message("IndividualEstimates element not detected in PharmML. Skipping...")
		}
		
		if ("PrecisionIndividualEstimates" %in% names(SOChildren[["Estimation"]])){
			SOObject <- ParsePrecisionIndividualEstimates(SOObject, SOChildren[["Estimation"]][["PrecisionIndividualEstimates"]])
		} else {
			message("PrecisionIndividualEstimates element not detected in PharmML. Skipping...")
		}
		
		if ("Residuals" %in% names(SOChildren[["Estimation"]])){
			SOObject <- ParseResiduals(SOObject, SOChildren[["Estimation"]][["Residuals"]])
		} else {
			message("Residuals element not detected in PharmML. Skipping...")
		}
		
		if ("Predictions" %in% names(SOChildren[["Estimation"]])){
			SOObject <- ParsePredictions(SOObject, SOChildren[["Estimation"]][["Predictions"]])
		} else {
			message("Predictions element not detected in PharmML. Skipping...")
		}
		
		if ("Likelihood" %in% names(SOChildren[["Estimation"]])){
			SOObject <- ParseLikelihood(SOObject, SOChildren[["Estimation"]][["Likelihood"]])
		} else {
			message("Likelihood element not detected in PharmML. Skipping...")
		}
		
	} else {
		message("Estimation element not detected in PharmML. Skipping...")
	}
	
	if ("Simulation" %in% names(SOChildren)){
		
		# Error Checking of unexpected elements of Simulation node
		expectedTags = c("Description", "OriginalDataset", "SimulationBlock")
		unexpected = setdiff(names(SOChildren[["Simulation"]]), expectedTags)
		if (length(unexpected) != 0) {
			warning(paste("The following unexpected elements were detected in the Simulation block of the PharmML SO.", 
							paste(unexpected, collapse="\n      "), sep="\n      "))
		}
		
		# Parse the Simulation node
		SOObject <- ParseSimulation(SOObject, SOChildren[["Simulation"]])
		
	} else {
		message("Simulation element not detected in PharmML. Skipping...")
	}
	
	
	# Run validation functions on S4 Class and subclasses
	validObject(SOObject)
	validObject(SOObject@RawResults)
	validObject(SOObject@Estimation)
	validObject(SOObject@Simulation)
	validObject(SOObject@OptimalDesign)
	
	SOObject
}
