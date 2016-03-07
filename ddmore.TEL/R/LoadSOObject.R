
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
#' @examples 
#' mlx <- LoadSOObject(file = system.file(package = "ddmore", 
#'     "tests", "data", "PharmMLSO", "MachineGenerated", 
#'     "UseCase2_TIMEchange_fixed.SO.xml"))
#' @export
#' @include StandardOutputObject.R
#' @include Simulation-Class.R
#' @include OptimalDesign-Class.R
#' @include ModelDiagnostic-Class.R
#' @include Estimation-Class.R
#' @include xmlParsers.R 

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
    on.exit({
        # Reset Working directory 
        setwd(old.wd)
    })
	setwd(dirname(file))
	
	SOObject <- createSOObjectFromXMLSOBlock(soBlock = soBlocks[[1]])
	
	# Populate the (hidden) slot specifying the XML file from which this SO was parsed
	slot(object = SOObject, name = ".pathToSourceXML") <- file

	# Print out any errors in the SO Object to the R console to make it obvious if execution failed
	.printSOMessages(
		.getSOX(so = SOObject, type = "Errors"),
		.getSOX(so = SOObject, type = "Warnings"),
		.getSOX(so = SOObject, type = "Info")
	)
	
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

LoadSOObjects <- function(file) {
	
	file <- file_path_as_absolute(file)

	root <- validateAndLoadXMLSOFile(file)
	soBlocks <- root[names(root) == "SOBlock"]

	if (length(soBlocks) == 0) {
		stop("PharmML parsing aborted. There does not appear to be any SOBlock sections in the specified file.")
	}
	if (length(soBlocks) == 1) {
		# Deliberately don't error because if SSE execution (which would normally generate multiple SO blocks) failed
		# with an error, there might only be one SOBlock, containing TaskInformation section with the error messages
		warning("LoadSOObjects() is used for the case where there is known to be multiple SOBlock in the SO XML file;\n", 
			"  use the singular version of the function, LoadSOObject(), instead if there is only a single SOBlock in the file.")
	}

	# Set working directory to that specified in file
	# (I (MSW) don't like this, can't we amend the xmlParsers.R to resolve the associated data files
	# relative to the directory in which the .SO.xml file lives, but remaining in the current directory?)
	old.wd <- getwd()
    on.exit({
        # Reset Working directory 
        setwd(old.wd)
    })
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
		slot(object = SOObject, name = ".pathToSourceXML") <- file
		SOObject
	})
    
	# Print out any errors in the SO Object to the R console to make it obvious if execution failed
	.printSOMessages(
		unlist(lapply(X = SOObjectList, FUN = .getSOX, type = "Errors"), recursive=FALSE),
		unlist(lapply(X = SOObjectList, FUN = .getSOX, type = "Warnings"), recursive=FALSE),
		unlist(lapply(X = SOObjectList, FUN = .getSOX, type = "Info"), recursive=FALSE)
	)

	names(SOObjectList) <- soObjNames
	SOObjectList
}

# get messages out of SO objects
.getSOX <- function(so, type) { slot(object = so, name = "TaskInformation")$Messages[[type]] }

#'
#' Check that the .SO.xml file exists; if so then parse the XML document and return a reference to the root node.
#'  
validateAndLoadXMLSOFile <- function(file) {
	
	# Error checking
	if (!(class(file) == "character" && file.exists(file))) {
		stop("Standard Output results file ", file, " does not exist.")
	}
	
	# Return a reference to the root node in the XML doc
    # useInternalNodes is an important flag that avoids exponential memory usage!
	# Want to be able to pass in a no-op handler function for "comment" nodes to strip comments out during parsing, i.e.
	#  handlers=list("comment"=function(x,...){NULL})
	# but this doesn't seem to be compatible with useInternalNodes=TRUE!
	xmlRoot(xmlTreeParse(file, useInternalNodes=TRUE))
}

#'
#' Process an SOBlock element from the SO XML tree and populate a StandardOutputObject object from the data contained within.
#'
#' @include StandardOutputObject.R
#' @include Simulation-Class.R
#' @include OptimalDesign-Class.R
#' @include ModelDiagnostic-Class.R
#' @include Estimation-Class.R
#' @include xmlParsers.R
#'  
createSOObjectFromXMLSOBlock <- function(soBlock) {
	
	# Generate Blank SO object
	SOObject <- StandardOutputObject()
	
	# Fetch all Components of the SO object that are defined
	SOChildren <- xmlChildren(soBlock)
	
	messageList <- list(parsed=list(), skipped=list())

	# Error Checking of unexpected elements
	expectedTags <- c("ToolSettings", "RawResults", "TaskInformation", "Estimation", 
			"Simulation", "ModelDiagnostic")
	unexpected <- setdiff(names(SOChildren), expectedTags)
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
		expectedTags <- c("PopulationEstimates", "PrecisionPopulationEstimates", 
				"IndividualEstimates", "PrecisionIndividualEstimates", "Residuals", 
				"Predictions", "OFMeasures")
		unexpected <- setdiff(names(SOChildren[["Estimation"]]), expectedTags)
		if (length(unexpected) != 0) {
			warning(paste("The following unexpected elements were detected in the Estimation section of the PharmML SO. These will be ignored.", 
							paste(unexpected, collapse="\n      "), sep="\n      "))
		}
		
		if ("PopulationEstimates" %in% names(SOChildren[["Estimation"]])){
			#SOObject <- ParsePopulationEstimates(SOObject, SOChildren[["Estimation"]][["PopulationEstimates"]])
			SOObject@Estimation@PopulationEstimates <- new (Class = "PopulationEstimates", SOChildren[["Estimation"]][["PopulationEstimates"]])
			messageList[["parsed"]] <- append(messageList[["parsed"]], "Estimation:PopulationEstimates")
		} else {
			messageList[["skipped"]] <- append(messageList[["skipped"]], "Estimation:PopulationEstimates")
		}
		
		if ("PrecisionPopulationEstimates" %in% names(SOChildren[["Estimation"]])){
			#SOObject <- ParsePrecisionPopulationEstimates(SOObject, SOChildren[["Estimation"]][["PrecisionPopulationEstimates"]])
			SOObject@Estimation@PrecisionPopulationEstimates <- new (Class = "PrecisionPopulationEstimates", SOChildren[["Estimation"]][["PrecisionPopulationEstimates"]])
			messageList[["parsed"]] <- append(messageList[["parsed"]], "Estimation:PrecisionPopulationEstimates")
		} else {
			messageList[["skipped"]] <- append(messageList[["skipped"]], "Estimation:PrecisionPopulationEstimates")
		}
		
		if ("IndividualEstimates" %in% names(SOChildren[["Estimation"]])){
			#SOObject <- ParseIndividualEstimates(SOObject, SOChildren[["Estimation"]][["IndividualEstimates"]])
			SOObject@Estimation@IndividualEstimates <- new (Class = "IndividualEstimates", SOChildren[["Estimation"]][["IndividualEstimates"]])
			messageList[["parsed"]] <- append(messageList[["parsed"]], "Estimation:IndividualEstimates")
		} else {
			messageList[["skipped"]] <- append(messageList[["skipped"]], "Estimation:IndividualEstimates")
		}
		
		if ("PrecisionIndividualEstimates" %in% names(SOChildren[["Estimation"]])){
			#SOObject <- ParsePrecisionIndividualEstimates(SOObject, SOChildren[["Estimation"]][["PrecisionIndividualEstimates"]])
			SOObject@Estimation@PrecisionIndividualEstimates <- new (Class = "PrecisionIndividualEstimates", SOChildren[["Estimation"]][["PrecisionIndividualEstimates"]])
			messageList[["parsed"]] <- append(messageList[["parsed"]], "Estimation:PrecisionIndividualEstimates")
		} else {
			messageList[["skipped"]] <- append(messageList[["skipped"]], "Estimation:PrecisionIndividualEstimates")
		}
		
		if ("Residuals" %in% names(SOChildren[["Estimation"]])){
			#SOObject <- ParseResiduals(SOObject, SOChildren[["Estimation"]][["Residuals"]])
			SOObject@Estimation@Residuals <- new (Class = "Residuals", SOChildren[["Estimation"]][["Residuals"]])
			messageList[["parsed"]] <- append(messageList[["parsed"]], "Estimation:Residuals")
		} else {
			messageList[["skipped"]] <- append(messageList[["skipped"]], "Estimation:Residuals")
		}
		
		if ("Predictions" %in% names(SOChildren[["Estimation"]])){
			#SOObject <- ParsePredictions(SOObject, SOChildren[["Estimation"]][["Predictions"]])
			# Table expected - TODO call specific function ?
			SOObject@Estimation@Predictions <- ParseElement(SOChildren[["Estimation"]][["Predictions"]])
			messageList[["parsed"]] <- append(messageList[["parsed"]], "Estimation:Predictions")
		} else {
			messageList[["skipped"]] <- append(messageList[["skipped"]], "Estimation:Predictions")
		}
		
		if ("OFMeasures" %in% names(SOChildren[["Estimation"]])){
			#SOObject <- ParseOFMeasures(SOObject, SOChildren[["Estimation"]][["OFMeasures"]])
			SOObject@Estimation@OFMeasures <- new (Class = "OFMeasures", SOChildren[["Estimation"]][["OFMeasures"]])
			messageList[["parsed"]] <- append(messageList[["parsed"]], "Estimation:OFMeasures")
		} else {
			messageList[["skipped"]] <- append(messageList[["skipped"]], "Estimation:OFMeasures")
		}
		
	} else {
		messageList[["skipped"]] <- append(messageList[["skipped"]], "Estimation")
	}
	
	if ("Simulation" %in% names(SOChildren)){
		
		# Error Checking of unexpected elements of Simulation node
		expectedTags <- c("SimulationBlock")
		unexpected <- setdiff(names(SOChildren[["Simulation"]]), expectedTags)
		if (length(unexpected) != 0) {
			warning(paste("The following unexpected elements were detected in the parent Simulation section of the PharmML SO. These will be ignored.", 
							paste(unexpected, collapse="\n      "), sep="\n      "))
		} 
		
		# Parse all Simulation Blocks within the Simulation node
		simulationBlockNodeList <- SOChildren[["Simulation"]][names(SOChildren[["Simulation"]]) == "SimulationBlock"]
		SOObject@Simulation <- lapply(
			X = simulationBlockNodeList,
			FUN = function(xmlNodeSimulationBlock) { new (Class = "SimulationBlock", xmlNodeSimulationBlock = xmlNodeSimulationBlock) }
		)
		
		messageList[["parsed"]] <- append(messageList[["parsed"]], "Simulation")	
	} else {
		messageList[["skipped"]] <- append(messageList[["skipped"]], "Simulation")
	}
	
	if ("OptimalDesign" %in% names(SOChildren)){
		
		# Error Checking of unexpected elements of OptimalDesign node
		expectedTags <- c("OptimalDesignBlock")
		unexpected <- setdiff(names(SOChildren[["OptimalDesign"]]), expectedTags)
		if (length(unexpected) != 0) {
			warning(paste("The following unexpected elements were detected in the parent OptimalDesign section of the PharmML SO. These will be ignored.", 
							paste(unexpected, collapse="\n      "), sep="\n      "))
		} 
		
		# Parse all OptimalDesign Blocks within the Simulation node
		optimalDesignBlockNodeList <- SOChildren[["OptimalDesign"]][names(SOChildren[["OptimalDesign"]]) == "OptimalDesignBlock"]
		SOObject@OptimalDesign <- lapply(
				X = optimalDesignBlockNodeList,
				FUN = function(xmlNodeOptimalDesignBlock) { new (Class = "OptimalDesignBlock", xmlNodeOptimalDesignBlock = xmlNodeOptimalDesignBlock) }
		)
		
		messageList[["parsed"]] <- append(messageList[["parsed"]], "OptimalDesign")	
	} else {
		messageList[["skipped"]] <- append(messageList[["skipped"]], "OptimalDesign")
	}
	
	if ("ModelDiagnostic" %in% names(SOChildren)){

		# Parse the ModelDiagnostic block
		SOObject@ModelDiagnostic <- new (Class = "ModelDiagnostic", xmlNodeModelDiagnostic = SOChildren[["ModelDiagnostic"]])
		
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

.printSOMessages <- function(soErrorMsgs, soWarningMsgs, soInfoMsgs) {
	
	if (!is.empty(soErrorMsgs)) {
		message("\nThe following ERRORs were raised during the job execution:", file=stderr())
		for (e in (soErrorMsgs)) { message(paste0(" ", e$Name, ": ", str_trim(e$Content)), file=stderr()) }
	}
	if (!is.empty(soWarningMsgs)) {
		message("\nThe following WARNINGs were raised during the job execution:", file=stderr())
		for (e in (soWarningMsgs)) { message(paste0(" ", e$Name, ": ", str_trim(e$Content)), file=stderr()) }
	}
	# Also print out any information messages
	if (!is.empty(soInfoMsgs)) {
		message("\nThe following MESSAGEs were raised during the job execution:", file=stderr())
		for (e in (soInfoMsgs)) { message(paste0(" ", e$Name, ": ", str_trim(e$Content)), file=stderr()) }
	}
	message("") # Extra line break
	
}
