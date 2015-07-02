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
	
	file <- file_path_as_absolute(file)
	
	root <- validateAndLoadXMLSOFile(file)
	soBlocks <- root[names(root) == "SOBlock"]
	
	if (length(soBlocks) == 0) {
		stop("PharmML parsing aborted. There does not appear to be any SOBlock sections in the specified file.")
	}
	if (length(soBlocks) > 1) {
		stop("LoadSOObject() is used for the case where there is known to be exactly one SOBlock in the SO XML file;\n",
			 "  use the plural version of the function, LoadSOObjects(), instead if there are multiple SOBlocks in the file.")
	}
	
	# Set working directory to that specified in file
	# (I (MSW) don't like this, can't we amend the xmlParsers.R to resolve the associated data files
	# relative to the directory in which the .SO.xml file lives, but remaining in the current directory?)
	old.wd <- getwd()
	setwd(dirname(file))
	
	SOObject <- createSOObjectFromXMLSOBlock(soBlocks[[1]])
	
	# Populate the (hidden) slot specifying the XML file from which this SO was parsed
	SOObject@.pathToSourceXML <- file

	# Print out any errors in the SO Object to the R console to make it obvious if execution failed
	if (length(SOObject@TaskInformation$Messages$Errors) > 0) {
		message("\nThe following ERRORs were raised during the job execution:", file=stderr())
		for (e in (SOObject@TaskInformation$Messages$Errors)) { message(paste0(" ", e$Name, ": ", str_trim(e$Content)), file=stderr()) }
	}
	if (length(SOObject@TaskInformation$Messages$Warnings) > 0) {
		message("\nThe following WARNINGs were raised during the job execution:", file=stderr())
		for (e in (SOObject@TaskInformation$Messages$Warnings)) { message(paste0(" ", e$Name, ": ", str_trim(e$Content)), file=stderr()) }
	}
	message("") # Extra line break
	
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
	
	file <- file_path_as_absolute(file)

	root <- validateAndLoadXMLSOFile(file)
	soBlocks <- root[names(root) == "SOBlock"]

	if (length(soBlocks) == 0) {
		stop("PharmML parsing aborted. There does not appear to be any SOBlock sections in the specified file.")
	}
	if (length(soBlocks) == 1) {
		stop("LoadSOObjects() is used for the case where there is known to multiple SOBlock in the SO XML file;\n", 
			" use the singlular version of the function, LoadSOObject(), instead if there is only a single SOBlocks in the file.")
	} 

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
		xmlAttrs(soBlock)[["blkId"]]
	}))

	SOObjectList <- lapply(SOBlockList, createSOObjectFromXMLSOBlock)
  
	# Populate the (hidden) slot specifying the XML file from which this SO was parsed
	SOObjectList <- lapply(SOObjectList, function(SOObject) {
		SOObject@.pathToSourceXML <- file
		SOObject
	})

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
	xmlRoot(xmlTreeParse(file, useInternalNodes=TRUE)) # useInternalNodes is an important flag that avoids exponential memory usage!
}

# Process an SOBlock element from the SO XML tree and populate a StandardOutputObject object from the data contained within.
createSOObjectFromXMLSOBlock <- function(soBlock) {
	
	# Generate Blank SO object
	SOObject <- createSOObject()
	
	# Fetch all Components of the SO object that are defined
	SOChildren <- xmlChildren(soBlock)
	
	messageList <- list(parsed=list(), skipped=list())

	# Error Checking of unexpected elements
	expectedTags = c("ToolSettings", "RawResults", "TaskInformation", "Estimation", 
			"Simulation", "ModelDiagnostic")
	unexpected = setdiff(names(SOChildren), expectedTags)
	if (length(unexpected) != 0) {
		warning(paste("The following unexpected elements were detected in the PharmML SO. These will be ignored.", 
						paste(unexpected, collapse="\n      "), sep="\n      "))
	}
	
	# Error checking of expected XML structure + Parser Execution
	if ("ToolSettings" %in% names(SOChildren)){
		SOObject <- ParseToolSettings(SOObject, SOChildren[["ToolSettings"]])
		messageList[["parsed"]] <- append(messageList[["parsed"]], "ToolSettings")
	} else {
		messageList[["skipped"]] <- append(messageList[["skipped"]], "ToolSettings")
	}
	
	if ("RawResults" %in% names(SOChildren)){
		SOObject <- ParseRawResults(SOObject, SOChildren[["RawResults"]])
		messageList[["parsed"]] <- append(messageList[["parsed"]], "RawResults")
	} else {
		messageList[["skipped"]] <- append(messageList[["skipped"]], "RawResults")
	}
	
	if ("TaskInformation" %in% names(SOChildren)){
		SOObject <- ParseTaskInformation(SOObject, SOChildren[["TaskInformation"]])
		messageList[["parsed"]] <- append(messageList[["parsed"]], "TaskInformation")
	} else {
		messageList[["skipped"]] <- append(messageList[["skipped"]], "TaskInformation")
	}
	
	if ("Estimation" %in% names(SOChildren)){
		
		# Error Checking of unexpected elements in Estimation Block
		expectedTags = c("PopulationEstimates", "PrecisionPopulationEstimates", 
				"IndividualEstimates", "PrecisionIndividualEstimates", "Residuals", 
				"Predictions", "Likelihood")
		unexpected = setdiff(names(SOChildren[["Estimation"]]), expectedTags)
		if (length(unexpected) != 0) {
			warning(paste("The following unexpected elements were detected in the Estimation section of the PharmML SO. These will be ignored.", 
							paste(unexpected, collapse="\n      "), sep="\n      "))
		}
		
		if ("PopulationEstimates" %in% names(SOChildren[["Estimation"]])){
			SOObject <- ParsePopulationEstimates(SOObject, SOChildren[["Estimation"]][["PopulationEstimates"]])
			messageList[["parsed"]] <- append(messageList[["parsed"]], "Estimation:PopulationEstimates")
		} else {
			messageList[["skipped"]] <- append(messageList[["skipped"]], "Estimation:PopulationEstimates")
		}
		
		if ("PrecisionPopulationEstimates" %in% names(SOChildren[["Estimation"]])){
			SOObject <- ParsePrecisionPopulationEstimates(SOObject, SOChildren[["Estimation"]][["PrecisionPopulationEstimates"]])
			messageList[["parsed"]] <- append(messageList[["parsed"]], "Estimation:PrecisionPopulationEstimates")
		} else {
			messageList[["skipped"]] <- append(messageList[["skipped"]], "Estimation:PrecisionPopulationEstimates")
		}
		
		if ("IndividualEstimates" %in% names(SOChildren[["Estimation"]])){
			SOObject <- ParseIndividualEstimates(SOObject, SOChildren[["Estimation"]][["IndividualEstimates"]])
			messageList[["parsed"]] <- append(messageList[["parsed"]], "Estimation:IndividualEstimates")
		} else {
			messageList[["skipped"]] <- append(messageList[["skipped"]], "Estimation:IndividualEstimates")
		}
		
		if ("PrecisionIndividualEstimates" %in% names(SOChildren[["Estimation"]])){
			SOObject <- ParsePrecisionIndividualEstimates(SOObject, SOChildren[["Estimation"]][["PrecisionIndividualEstimates"]])
			messageList[["parsed"]] <- append(messageList[["parsed"]], "Estimation:PrecisionIndividualEstimates")
		} else {
			messageList[["skipped"]] <- append(messageList[["skipped"]], "Estimation:PrecisionIndividualEstimates")
		}
		
		if ("Residuals" %in% names(SOChildren[["Estimation"]])){
			SOObject <- ParseResiduals(SOObject, SOChildren[["Estimation"]][["Residuals"]])
			messageList[["parsed"]] <- append(messageList[["parsed"]], "Estimation:Residuals")
		} else {
			messageList[["skipped"]] <- append(messageList[["skipped"]], "Estimation:Residuals")
		}
		
		if ("Predictions" %in% names(SOChildren[["Estimation"]])){
			SOObject <- ParsePredictions(SOObject, SOChildren[["Estimation"]][["Predictions"]])
			messageList[["parsed"]] <- append(messageList[["parsed"]], "Estimation:Predictions")
		} else {
			messageList[["skipped"]] <- append(messageList[["skipped"]], "Estimation:Predictions")
		}
		
		if ("Likelihood" %in% names(SOChildren[["Estimation"]])){
			SOObject <- ParseLikelihood(SOObject, SOChildren[["Estimation"]][["Likelihood"]])
			messageList[["parsed"]] <- append(messageList[["parsed"]], "Estimation:Likelihood")
		} else {
			messageList[["skipped"]] <- append(messageList[["skipped"]], "Estimation:Likelihood")
		}
		
	} else {
		messageList[["skipped"]] <- append(messageList[["skipped"]], "Estimation")
	}
	
	if ("Simulation" %in% names(SOChildren)){
		
		# Error Checking of unexpected elements of Simulation node
		expectedTags = c("Description", "OriginalDataset", "SimulationBlock")
		unexpected = setdiff(names(SOChildren[["Simulation"]]), expectedTags)
		if (length(unexpected) != 0) {
			warning(paste("The following unexpected elements were detected in the parent Simulation section of the PharmML SO. These will be ignored.", 
							paste(unexpected, collapse="\n      "), sep="\n      "))
		} 
		
		# Parse the Simulation node
		SOObject <- ParseSimulation(SOObject, SOChildren[["Simulation"]])
		messageList[["parsed"]] <- append(messageList[["parsed"]], "Simulation")	
	} else {
		messageList[["skipped"]] <- append(messageList[["skipped"]], "Simulation")
	}
	
	if ("ModelDiagnostic" %in% names(SOChildren)){
		
		# Error Checking of unexpected elements of Simulation node
		expectedTags = c("DiagnosticPlotsIndividualParams", "DiagnosticPlotsStructuralModel")
		unexpected = setdiff(names(SOChildren[["ModelDiagnostic"]]), expectedTags)
		if (length(unexpected) != 0) {
			warning(paste("The following unexpected elements were detected in the ModelDiagnostic section of the PharmML SO. These will be ignored.", 
							paste(unexpected, collapse="\n      "), sep="\n      "))
		}
		
		# Parse the Simulation node
		SOObject <- ParseModelDiagnostic(SOObject, SOChildren[["ModelDiagnostic"]])
		messageList[["parsed"]] <- append(messageList[["parsed"]], "ModelDiagnostic")	
	} else {
		messageList[["skipped"]] <- append(messageList[["skipped"]], "ModelDiagnostic")
	}
	
	# Run validation functions on S4 Class and subclasses
	validObject(SOObject)
	validObject(SOObject@RawResults)
	validObject(SOObject@Estimation)
	validObject(SOObject@Simulation)
	validObject(SOObject@OptimalDesign)
	
	# Print parsed and skipped elements.
	message(paste("\nThe following elements were parsed successfully:", 
				paste(messageList$parsed, collapse="\n      "), sep="\n      "))

	SOObject
}
