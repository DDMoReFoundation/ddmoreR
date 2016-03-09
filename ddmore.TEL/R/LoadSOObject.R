
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
#' @include StandardOutputObjectXmlParsers.R 

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
	# (I (MSW) don't like this, can't we amend the StandardOutputObjectXmlParsers.R to resolve the associated data files
	# relative to the directory in which the .SO.xml file lives, but remaining in the current directory?)
	old.wd <- getwd()
    on.exit({
        # Reset Working directory 
        setwd(old.wd)
    })
	setwd(dirname(file))
	
	SOObject <- .createSOObjectFromXMLSOBlock(soBlocks[[1]])
	
	# Populate the (hidden) slot specifying the XML file from which this SO was parsed
	slot(object = SOObject, name = ".pathToSourceXML") <- file

	# Print out any errors in the SO Object to the R console to make it obvious if execution failed
	.printSOMessages(
		getSOMessages(so = SOObject, type = "ERROR"),
		getSOMessages(so = SOObject, type = "WARNING"),
		getSOMessages(so = SOObject, type = "INFORMATION")
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
	# (I (MSW) don't like this, can't we amend the StandardOutputObjectXmlParsers.R to resolve the associated data files
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

	SOObjectList <- lapply(SOBlockList, .createSOObjectFromXMLSOBlock)
  
	# Populate the (hidden) slot specifying the XML file from which this SO was parsed
	SOObjectList <- lapply(SOObjectList, function(SOObject) {
		slot(object = SOObject, name = ".pathToSourceXML") <- file
		SOObject
	})
    
	# Print out any errors in the SO Object to the R console to make it obvious if execution failed
	.printSOMessages(
		unlist(lapply(X = SOObjectList, FUN = getSOMessages, type = "ERROR"), recursive=FALSE),
		unlist(lapply(X = SOObjectList, FUN = getSOMessages, type = "WARNING"), recursive=FALSE),
		unlist(lapply(X = SOObjectList, FUN = getSOMessages, type = "INFORMATION"), recursive=FALSE)
	)

	names(SOObjectList) <- soObjNames
	SOObjectList
}

#'
#' Process an SOBlock element and its sub-tree of elements, from the SO XML tree, and populate
#' a \linkS4class{StandardOutputObject} object from the data contained within.
#' Delegates the parsing to the class initialiser/constructor.
#'
#' @param xmlNodeSOBlock
#' @return object of class \linkS4class{StandardOutputObject}
#' 
#' @include StandardOutputObject-Class.R
#' 
.createSOObjectFromXMLSOBlock <- function(xmlNodeSOBlock) {
	new (Class = "StandardOutputObject", xmlNodeSOBlock)
}

# get messages out of SO objects
getSOMessages <- function(so, type) {
	slotName <- paste0(capitalise_first(tolower(type)), "Messages")
	slot(so@TaskInformation, slotName)
}

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
