
MDL_FILE_EXT <- 'mdl'
JSON_FILE_EXT <- 'json'

MOG_OBJECT_TYPES <- c("dataObj", "parObj", "mdlObj", "taskObj")

# This used to include ".PKMACRO" too; add this back in if "PKMACRO" is re-introduced into the MDL syntax,
# and also update the R doc on the MODEL_PREDICTION slot in the class definition of mdlObj accordingly.
MODEL_PREDICTION_SUBBLOCKS <- c(".DEQ", ".COMPARTMENT") 


################################################################################
#' .parseMDLFile
#'
#' Calls the MDL parser, providing a specified path to an MDL file, and returns a list of
#' all objects from that MDL file of a specified type. Optionally a name can be specified
#' too to target a specific object. The recognised types are:
#' \itemize{
#'   \item \code{dataObj} -> Returns S4 object(s) of Data Object class \code{\linkS4class{dataObj}}.
#'   \item \code{parObj} -> Returns S4 object(s) of Parameter Object class \code{\linkS4class{parObj}}.
#'   \item \code{mdlObj} -> Returns S4 object(s) of Model Object class \code{\linkS4class{mdlObj}}.
#'   \item \code{taskObj} -> Returns S4 object(s) of Task Properties Object class \code{\linkS4class{taskObj}}.
#' }
#' 
#' This function is not intended for use by the user. The functions
#' \link{getDataObjects}, \link{getParameterObjects}, \link{getModelObjects} and
#' \link{getTaskPropertiesObjects}, or \link{getMDLObjects} to obtain all objects from an
#' MDL file in a unified list, should be used instead.
#'
#' @param f Path to the .mdl file containing the objects.
#' @param name (Optional) Specifies the dataObj/parObj/mdlObj/taskObj object,
#'        by name, to be retrieved. If multiple objects of a particular type exist in the
#'        .mdl file then using the name argument allows the user to target a specific object.
#' @param type String specifying the type of object(s) to extract. Possible values are
#'        \code{dataObj}, \code{parObj}, \code{mdlObj}, \code{taskObj}.
#' @param fisServer FISServer instance.
#' @return A list of parsed objects which are contained in the MDL file, that match
#'         the specified criteria. If name is specified, only the single specified object is returned.
#' 
#' @usage .parseMDLFile('Warfarin-ODE-latest.mdl', type='dataObj')
#' 
#' @include telClasses.R
.parseMDLFile <- function(f, name, type, fisServer) {
    .precondition.checkArgument(is.FISServer(fisServer), "fisServer", "FIS Server instance is required.")
  if (!missing(type) && !type%in%c(MOG_OBJECT_TYPES)) {
    stop(paste0("Type specified is not one of \"", paste(MOG_OBJECT_TYPES, collapse="\", \""), "\""))
  }
  
  # Call parser and read in the JSON data
  raw <- .parseMDLFile0(f, fisServer);

  if (!missing(name)) {
  
    res <- .extractNamedObject(raw, name) # Single object
	if (length(res) == 0) {
	  stop(paste0("No object named \"", name, "\" of type \"", type, "\" found in the parsed MDL file"))
	}
    
  } else {
	  
    res <- .extractTypeObjects(raw, type) # List of objects
	if (length(res) == 0) {
	  warning(paste0("No objects of type \"", type, "\" found in the parsed MDL file"))
	}
  
  }
  
  return(res)
}


.parseMDLFile0 <- function(f, fisServer) {
    .precondition.checkArgument(is.FISServer(fisServer), "fisServer", "FIS Server instance is required.")
	if (file_ext(f) == MDL_FILE_EXT) {
		json <- readMDL(fisServer, f)
	} else if (file_ext(f) == JSON_FILE_EXT) { # For testing purposes
		json <- readLines(f, warn=FALSE)[[1]]
	} else {
		stop(paste("The file extension for the file being parsed into R objects should be .mdl; the filename was", f))
	}
  
    fromJSON(json)

}


.extractNamedObject <- function(raw, name) {
	
	val <- raw[sapply(raw, function(e) {e$name == name})] # Creates a list containing the single matching object
  
	.extractObjs(val)[[1]]
}


.extractTypeObjects <- function(raw, type) {
	
	val <- raw[sapply(raw, function(e) {e$type == type})] # Creates a list containing the matching objects

	.extractObjs(val)
}


.extractObjs <- function(raw) {
	
	applyObjectNamesToListOfObjects(
		lapply(raw, function(b) {
	
			createObjFn <- switch(b$type,
				dataObj = .createDataObj,
				parObj = .createParObj,
				mdlObj = .createMdlObj,
				taskObj = .createTaskObj
			)
			
			createObjFn(b$blocks, b$name)
	
		})
	)
	
}


.createParObj <- function(parObjAsList, name) {
	
	if (is.null(parObjAsList)) {
		stop("Input argument parObjAsList is null")
	}
	
	parObj <- new("parObj",
		DECLARED_VARIABLES = as.list(parObjAsList$DECLARED_VARIABLES), # as.list handles null; TODO Transformation like translateIntoNamedList to be applied to this instead
		STRUCTURAL = removeExtraLayerOfNesting(parObjAsList$STRUCTURAL), # removeExtraLayerOfNesting handles null
		VARIABILITY = removeExtraLayerOfNesting(parObjAsList$VARIABILITY), # removeExtraLayerOfNesting handles null
		name = name
	)
	
	parObj
} 


.createDataObj <- function(dataObjAsList, name) {
	
	if (is.null(dataObjAsList)) {
		stop("Input argument dataObjAsList is null")
	}

    dataObj <- new("dataObj",
        SOURCE = removeExtraLayerOfNesting(dataObjAsList$SOURCE), # removeExtraLayerOfNesting handles null
		DECLARED_VARIABLES = as.list(dataObjAsList$DECLARED_VARIABLES), # as.list handles null; TODO Transformation like translateIntoNamedList to be applied to this instead
        DATA_INPUT_VARIABLES = removeExtraLayerOfNesting(dataObjAsList$DATA_INPUT_VARIABLES), # removeExtraLayerOfNesting handles null
        DATA_DERIVED_VARIABLES = as.list(dataObjAsList$DATA_DERIVED_VARIABLES), # as.list handles null; TODO Transformation like translateIntoNamedList to be applied to this instead
		name = name
    )
    
	dataObj@SOURCE <- lapply(dataObj@SOURCE, function(srcfile) {
	    # Unquote the file name so that the file name within the R object is more easily manipulated
	    srcfile$file <- strip_quotes(srcfile$file)
	    # Similarly for the Ignore character
	    srcfile$ignore <- strip_quotes(srcfile$ignore)
		srcfile
	})

    dataObj
}


.createMdlObj <- function(mdlObjAsList, name) {
	
	if (is.null(mdlObjAsList)) {
		stop("Input argument mdlObjAsList is null")
	}
	
    mdlObj <- new("mdlObj",
		IDV = as.char.vector(mdlObjAsList$IDV), # our version of as.vector for strings that handles null
		COVARIATES = as.list(mdlObjAsList$COVARIATES), # as.list handles null; TODO Transformation like translateIntoNamedList to be applied to this instead
		VARIABILITY_LEVELS = removeExtraLayerOfNesting(mdlObjAsList$VARIABILITY_LEVELS), # removeExtraLayerOfNesting handles null
        STRUCTURAL_PARAMETERS = as.char.vector(mdlObjAsList$STRUCTURAL_PARAMETERS), # our version of as.vector for strings that handles null
        VARIABILITY_PARAMETERS = as.char.vector(mdlObjAsList$VARIABILITY_PARAMETERS), # our version of as.vector for strings that handles null
        RANDOM_VARIABLE_DEFINITION = as.list(mdlObjAsList$RANDOM_VARIABLE_DEFINITION), # as.list handles null; TODO Transformation like translateIntoNamedList to be applied to this instead
        INDIVIDUAL_VARIABLES = as.list(mdlObjAsList$INDIVIDUAL_VARIABLES), # as.list handles null; TODO Transformation like translateIntoNamedList to be applied to this instead
		MODEL_PREDICTION = as.list(mdlObjAsList$MODEL_PREDICTION), # as.list handles null; TODO transformation into objects to be applied here
        OBSERVATION = as.list(mdlObjAsList$OBSERVATION), # as.list handles null; TODO Transformation like translateIntoNamedList to be applied to this instead
		GROUP_VARIABLES = as.list(mdlObjAsList$GROUP_VARIABLES), # as.list handles null; TODO Transformation like translateIntoNamedList to be applied to this instead
		name = name
    )
	
	mdlObj
}


.createTaskObj <- function(taskObjAsList, name) {
	
	if (is.null(taskObjAsList)) {
		stop("Input argument taskObjAsList is null")
	}
	
	taskObj <- new("taskObj",
		ESTIMATE = as.list(taskObjAsList$ESTIMATE), # as.list handles null
		SIMULATE = as.list(taskObjAsList$SIMULATE), # as.list handles null
		name = name
	)
	
	taskObj
}

##############################################################
#' writeMogObj
#'
#' Takes in an instance of the R \linkS4class{mogObj} class along with a specified file path, 
#' and writes out the content of the MOG Object to that file as MDL.
#'
#' The \linkS4class{mogObj} class comprises of a single instance of each of the following objects:
#' \itemize{
#' 	 \item \linkS4class{dataObj} class
#' 	 \item \linkS4class{parObj} class
#' 	 \item \linkS4class{mdlObj} class
#' 	 \item \linkS4class{taskObj} class
#' }
#' 
#' It is recommended that the file not have an extension, whereby the .mdl extension will be appended.
#' 
#' @usage writeMogObj(myMogObj, 'C:/Users/fred/mymodel')
#'
#' @param object Instance of R class \linkS4class{mogObj}.
#' @param f File path to the .mdl file (optionally without the .mdl extension) that will be created.
#' @param fisServer FISServer instance.
#'
#' @export
#' 
#' @docType methods
#' @include telClasses.R
#' @rdname writeMogObj-methods

setGeneric("writeMogObj", function(object, f, fisServer=TEL.getServer()) { 
  standardGeneric("writeMogObj")
})

#' @rdname writeMogObj-methods
#' @aliases writeMogObj,mogObj,mogObj-method
setMethod("writeMogObj", "mogObj", function(object, f, fisServer=TEL.getServer()) {
    .precondition.checkArgument(is.FISServer(fisServer), "fisServer", "FIS Server instance is required.")
    json <- .generateJSON(object)

    .write.mclobj0(json, f, fisServer = fisServer)
})

.generateJSON <- function(object) {
    if (!validity.mogObj(object)) {
        stop("Object is not a valid MOG Object")
    }
    m = object
    
    parObjBlocks <- .removeNullEntries(list(
        DECLARED_VARIABLES = m@parObj@DECLARED_VARIABLES,
        STRUCTURAL = addExtraLayerOfNesting(m@parObj@STRUCTURAL),
        VARIABILITY = addExtraLayerOfNesting(m@parObj@VARIABILITY)
    ))

	dataObjBlocksSOURCE <- lapply(m@dataObj@SOURCE, function(srcfile) {
		# Enclose the file name in double quotes ready for writing back to MDL
		srcfile$file <- add_quotes(srcfile$file)
		# Similarly for the Ignore character
		srcfile$ignore <- add_quotes(srcfile$ignore)
		srcfile
	})
    
    dataObjBlocks <- .removeNullEntries(list(
        SOURCE = addExtraLayerOfNesting(dataObjBlocksSOURCE),
        DECLARED_VARIABLES = m@dataObj@DECLARED_VARIABLES,
        DATA_INPUT_VARIABLES = addExtraLayerOfNesting(m@dataObj@DATA_INPUT_VARIABLES),
        DATA_DERIVED_VARIABLES = m@dataObj@DATA_DERIVED_VARIABLES
    ))

    mdlObjBlocks <- .removeNullEntries(list(
        IDV = m@mdlObj@IDV,
        COVARIATES = m@mdlObj@COVARIATES,
        VARIABILITY_LEVELS = addExtraLayerOfNesting(m@mdlObj@VARIABILITY_LEVELS),
        STRUCTURAL_PARAMETERS = m@mdlObj@STRUCTURAL_PARAMETERS,
        VARIABILITY_PARAMETERS = m@mdlObj@VARIABILITY_PARAMETERS,
        RANDOM_VARIABLE_DEFINITION = m@mdlObj@RANDOM_VARIABLE_DEFINITION,
        INDIVIDUAL_VARIABLES = m@mdlObj@INDIVIDUAL_VARIABLES,
        MODEL_PREDICTION = m@mdlObj@MODEL_PREDICTION,
        OBSERVATION = m@mdlObj@OBSERVATION,
        GROUP_VARIABLES = m@mdlObj@GROUP_VARIABLES
    ))
    
    taskObjBlocks <- .removeNullEntries(list(
        ESTIMATE = m@taskObj@ESTIMATE,
		SIMULATE = m@taskObj@SIMULATE
    ))
    
    dataObjName <- m@dataObj@name
    parObjName <- m@parObj@name
    mdlObjName <- m@mdlObj@name
    taskObjName <- m@taskObj@name
    mogDefinitionName <- if (length(m@name) == 0 || m@name == '') "outputMog" else m@name
	
	# Extra layer of nesting needed for the JSON - ensures that order of items is maintained (although it doesn't actually matter for this block)
	mogObjBlocks <- list(OBJECTS=list(
		list(list(type="dataObj")),
		list(list(type="parObj")),
		list(list(type="mdlObj")),
		list(list(type="taskObj"))
	))
	names(mogObjBlocks$OBJECTS[[1]]) <- c(dataObjName)
	names(mogObjBlocks$OBJECTS[[2]]) <- c(parObjName)
	names(mogObjBlocks$OBJECTS[[3]]) <- c(mdlObjName)
	names(mogObjBlocks$OBJECTS[[4]]) <- c(taskObjName)
    
	allObjsAsList <- list(
		list(name=dataObjName, type="dataObj", blocks=dataObjBlocks),
		list(name=parObjName, type="parObj", blocks=parObjBlocks),
		list(name=mdlObjName, type="mdlObj", blocks=mdlObjBlocks),
		list(name=taskObjName, type="taskObj", blocks=taskObjBlocks),
		list(name=mogDefinitionName, type="mogObj", blocks=mogObjBlocks)
	)
    
    json <- toJSON(allObjsAsList)
    return(json)
}

.write.mclobj0 <- function(json, f, fisServer) {
    .precondition.checkArgument(is.FISServer(fisServer), "fisServer", "FIS Server instance is required.")
    fullPath <- normalizePath(f, winslash="/", mustWork=FALSE)
	
	if (file_ext(f) == MDL_FILE_EXT) {
		
		wreq <- URLencode(toJSON(list(
			fileName = fullPath,
			fileContent = json
		)), reserved=TRUE) # ensures that & characters etc. get encoded too
		
		# Call parser and post the JSON data:
		cmd <- URLencode(getWriteMDLUrl(fisServer))
		
		postfield <- sprintf('%s%s','writeRequest=',wreq)
		
		h = basicTextGatherer()
		RCurl:::curlPerform(url = cmd, postfields = postfield, writefunction = h$update)
		retStatus <- h$value()
		
		if (fromJSON(retStatus)$status != "Successful") {
			stop("Failed to send write request. Details of the error: ", retStatus)
		}
		# Don't print out the JSON-format return status
		invisible(retStatus)
		
	} else {
		stop(paste("The file extension for the file being written out from R objects should be .mdl; the filename was", f))
	}

}

# Process a list removing any empty lists or strings (character vectors) entries from it.
# Applied to the outgoing JSON as it is being written out, this is the reverse of applying
# as.list() / as.character() to the JSON coming in to transform missing blocks in the
# original MDL into empty lists / character vectors respectively.
# This translation is only required because R cannot handle null in slots in S4 classes.
.removeNullEntries <- function(l) {
	l[!sapply(l,
		function(x) { length(x) == 0 }
	)]
}

# Incoming (JSON->R) lists of variables etc. have their variable/symbol names as attributes
# (".name") of the list elements; use these as the names in the creation of a named list
# of these variables etc., to go in the slots in the R classes.
# Also, remove the name attribute (".name") to avoid duplication and confusion.
# Named lists allow for easier access and manipulation of the R objects by R workflows.
# This function also handles null which gets converted into an empty list.
# TODO update this, we no longer write variable names to ".name" attr but to "name" attr
translateIntoNamedList <- function(x) {
	l <- as.list(x) # Handle null which gets converted into an empty list
	# Note that a variable that doesn't have a .name would be default get translated into an
	# element with name "NULL"; to actually give it no name, NA needs to be assigned instead
	names(l) <- sapply(l, function(e) { ifelse(is.null(e$.name), NA, e$.name) }) # 'e' is the list element
	lapply(l, function(e) { e$.name <- NULL; e }) # 'e' is the list element
}

# When writing out JSON, the named lists from the R objects need to have their names
# stripped off, in order that they are written out as lists in the JSON rather than
# (unordered) maps.
# Each name is 'moved' onto an attribute named ".name" of the list element instead.
# TODO update this, we no longer write variable names to ".name" attr but to "name" attr
translateNamedListIntoList <- function(l) {
	if (length(l) > 0) { # trap the empty-list condition
		res <- lapply(1:length(l), function(i) {
			if (!is.na(names(l)[[i]])) { # Ignore unnamed elements since they don't need a .name assigned
				l[[i]]$.name <- names(l)[[i]]
			}
			l[[i]]
		})
		names(res) <- NULL
		res
	} else {
		list()
	}
}

# Given an (unnamed) list containing instances of dataObj, parObj, mdlObj and/or taskObj,
# extract the 'name' attributes from each of these and return the same list as passed in
# but now being a named list, i.e. having those extracted names assigned as keys.
applyObjectNamesToListOfObjects <- function(objList) {
	names(objList) <- sapply(objList, function(obj) { obj@name })
	objList
}

# Given a list that contains individual elements that are themselves
# lists of length 1, strip off the top-level list to give a list
# containing the aggregated elements of the individual second-level lists.
# Normally the individual sub-lists would be named lists, in which
# case the names would also be aggregated together, and applied to the
# new top-level 'unified' list.
removeExtraLayerOfNesting <- function(l) {
	
	lNames <- lapply(as.list(l), function(l) names(l))
	res <- lapply(as.list(l), function(l) l[[1]])
	
	if (!any(sapply(lNames, is.null))) { # There are no NULL names in the original nested lists
		names(res) <- lNames
	}
	
	res
}

# The reverse of removeExtraLayerOfNesting(), used when writing the JSON back out.
# Given a named list, wrap each element in the list in an outer list, maintaining
# the name of the element in the sub-list but leaving the outer list unnamed.
addExtraLayerOfNesting <- function(l) {
	if (length(l) > 0) { # trap the empty-list condition
		lapply(1:length(l), function(i) {
			res <- list(l[[i]])
			names(res) <- names(l)[[i]]
			res
		})
	} else {
		list()
	}
}

# Our version of as.vector for strings that handles null correctly
# i.e. as.char.vector(NULL) = character(0)
# to be consistent with as.list(NULL) = list()
as.char.vector <- function(v) {
	c(character(0), v)
}



################################################################################
#' as.PharmML
#' 
#' Converts an MDL file into a PharmML file.
#' 
#' @param f Path to the .mdl file to be converted.
#' @param fisServer FISServer instance.
#' @return Path to the generated .xml PharmML file.
#' 
#' @export
as.PharmML <- function(f, fisServer = TEL.getServer()) {
    .precondition.checkArgument(is.FISServer(fisServer), "fisServer", "FIS Server instance is required.")
    .precondition.checkArgument(file.exists(f), "f", sprintf("MDL file %s must exist.",f))
	
	resultFile <- MDLToPharmML(fisServer, f)
	
	if (!file.exists(resultFile)) {
		stop("Failed to convert MDL to PharmML; PharmML file path returned from the conversion service was \"", resultFile, "\" but this file does not exist.");
	}
	return(resultFile)
}
