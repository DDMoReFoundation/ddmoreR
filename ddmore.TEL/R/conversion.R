
MDL_FILE_EXT <- 'mdl'
JSON_FILE_EXT <- 'json'

MOG_OBJECT_TYPES <- c("dataobj", "parobj", "mdlobj", "taskobj")

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
#'   \item \code{dataobj} -> Returns S4 object(s) of Data Object class \code{\linkS4class{dataObj}}.
#'   \item \code{parobj} -> Returns S4 object(s) of Parameter Object class \code{\linkS4class{parObj}}.
#'   \item \code{mdlobj} -> Returns S4 object(s) of Model Object class \code{\linkS4class{mdlObj}}.
#'   \item \code{taskobj} -> Returns S4 object(s) of Task Properties Object class \code{\linkS4class{taskObj}}.
#' }
#' 
#' This function is not intended for use by the user. The functions
#' \link{getDataObjects}, \link{getParameterObjects}, \link{getModelObjects} and
#' \link{getTaskPropertiesObjects}, or \link{getMDLObjects} to obtain all objects from an
#' MDL file in a unified list, should be used instead.
#'
#' @param f Path to the .mdl file containing the objects.
#' @param name (Optional) Specifies the dataobj/parobj/mdlobj/taskobj/mogobj object,
#'        by name, to be retrieved. If multiple objects of a particular type exist in the
#'        .mdl file then using the name argument allows the user to target a specific object.
#' @param type String specifying the type of object(s) to extract. Possible values are
#'        \code{dataobj}, \code{parobj}, \code{mdlobj}, \code{taskobj} and \code{mogobj}.
#' @param fisServer FISServer instance.
#' @return A list of parsed objects which are contained in the MDL file, that match
#'         the specified criteria. If name is specified, only the single specified object is returned.
#' 
#' @usage .parseMDLFile('Warfarin-ODE-latest.mdl', type='dataobj')
#' 
#' @include telClasses.R
.parseMDLFile <- function(f, name, type, fisServer) {
    .precondition.checkArgument(is.FISServer(fisServer), "fisServer", "FIS Server instance is required.")
  if (!type%in%c(MOG_OBJECT_TYPES, "mogObj")) {
    stop(paste0("Type specified is not one of \"", paste(MOG_OBJECT_TYPES, collapse="\", \""), "\" or \"mogobj\""))
  }
  
  # Call parser and read in the JSON data
  raw <- .parseMDLFile0(f, fisServer);

  if (!missing(name)) {
  
    res <- .extractNamedObject(raw, name, type)
	if (length(res) == 0) {
	  stop(paste0("No object named \"", name, "\" of type \"", type, "\" found in the parsed MDL file"))
	}

	# res is currently returned as a list with one element. This will return the element itself instead 
	res <- res[[1]]
    
  } else {
	  
    res <- .extractTypeObject(raw, type)
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
  
    fromJSON(json)[[1]]

}


.extractNamedObject <- function(raw, name, type) {
	
  val <- raw[name] # Creates a list containing the single matching object
  
  .extractTypeObject(val, type)
}


.extractTypeObject <- function(raw, type) {
	
	switch (type,
		dataobj  = .extractObj(raw, "dataobj", .createDataObj),
		parobj  = .extractObj(raw, "parobj", .createParObj),
		mdlobj  = .extractObj(raw, "mdlobj", .createMdlObj),
		taskobj  = .extractObj(raw, "taskobj", .createTaskObj)
	)
  
}


.extractObj <- function(raw, identifier, createObjFn) {
	
	# Extract identifiers
	logi <- sapply(raw, 
		function(x) {
			x$identifier==identifier
		}
	)
	
	subList <- raw[logi]
	
	# This is essentially
	#  objList <- lapply(subList, createObjFn)
	# but with the extra line of code that assigns the name
	objList <- lapply(names(subList), function(n) { # 'n' is the name of the list element
		obj <- createObjFn(subList[[n]])
		obj@name <- n
		obj
	} )
	
	# Having the list of objects as a named list is not strictly necessary since the objects have a "name" attribute,
	# but it is convenient to be able to do e.g.
	#  parsed <- getMDLObjects("Warfarin-ODE-28Oct2014.mdl")
	#  dataObj <- parsed$warfarin_PK_ODE_dat
	names(objList) <- names(subList)
	
	return(objList)
}


.createParObj <- function(parObjAsList) {
	
	if (is.null(parObjAsList)) {
		stop("Input argument parObjAsList is null")
	}

	# Weren't sure what to do about the VARIABILITY block since you can have a mixture
	# of named parameters, "matrix" blocks, "diag" blocks and "same" blocks.
	# The transformation on this block in the code below essentially removes the extra layer
	# of nesting there is in the VARIABILITY data coming in.
	# It allows the named parameters to be accessed as such e.g. myMog@parObj@VARIABILITY$PPV_KOUT,
	# as per all the other such lists of named parameters/variables there are in the R objects.
	# A side effect of this though, peculiar to the VARIABILITY block due to its heterogeneous
	# nature, is that this gives rise to multiple elements having the same name in the list
	# (multiple "same" blocks probably being the most common scenario).
	# Although R doesn't actually complain, having duplicate names in a named list undoubtedly
	# isn't a good idea, so such elements have a "_n" suffix appended to their names where "n"
	# is an number that increments individually for "matrix", "diag" and "same"; when writing
	# the R objects back out to JSON (and thence to MDL), these suffixes are dropped.

	diagCnt <- 0; matrixCnt <- 0; sameCnt <- 0;
	variabilityNames <- lapply(as.list(parObjAsList$VARIABILITY), function(x) {
		elemName <- names(x) # only one element in each sub-list of the main list
		if (elemName == "diag") {
			diagCnt <<- diagCnt + 1
			elemName <- paste0(elemName, "_", diagCnt)
		}
		else if (elemName == "matrix") {
			matrixCnt <<- matrixCnt + 1
			elemName <- paste0(elemName, "_", matrixCnt)
		}
		else if (elemName == "same") {
			sameCnt <<- sameCnt + 1
			elemName <- paste0(elemName, "_", sameCnt)
		}
		elemName
	})
	
	res <- new("parObj",
		DECLARED_VARIABLES = translateIntoNamedList(parObjAsList$DECLARED_VARIABLES), # as.list done within the function
		STRUCTURAL = translateIntoNamedList(parObjAsList$STRUCTURAL), # as.list done within the function
		VARIABILITY = removeExtraLayerOfNesting(parObjAsList$VARIABILITY),
		# TODO: TBC - These need to be populated
		PRIOR_PARAMETERS = list(),
		TARGET_CODE = as.character(parObjAsList$TARGET_CODE)
	)
	
	names(res@VARIABILITY) <- variabilityNames
	
	res
} 


.createDataObj <- function(dataObjAsList) {
	
	if (is.null(dataObjAsList)) {
		stop("Input argument dataObjAsList is null")
	}

    res <- new("dataObj",
        SOURCE = as.list(dataObjAsList$SOURCE),
		DECLARED_VARIABLES = translateIntoNamedList(dataObjAsList$DECLARED_VARIABLES), # as.list done within the function
        DATA_INPUT_VARIABLES = translateIntoNamedList(dataObjAsList$DATA_INPUT_VARIABLES), # as.list done within the function
        DATA_DERIVED_VARIABLES = translateIntoNamedList(dataObjAsList$DATA_DERIVED_VARIABLES), # as.list done within the function
        # TODO: TBC - This needs to be populated
		TARGET_CODE = as.character(dataObjAsList$TARGET_CODE)
    )
    
    # Unquote the file name so that the file name within the R object is more easily manipulated
    res@SOURCE$file <- strip_quotes(res@SOURCE$file)
    # Similarly for the Ignore character
    res@SOURCE$ignore <- strip_quotes(res@SOURCE$ignore)

    res
}


.createMdlObj <- function(mdlObjAsList) {
	
	if (is.null(mdlObjAsList)) {
		stop("Input argument mdlObjAsList is null")
	}
	
    res <- new("mdlObj",
		IDV = translateIntoNamedList(mdlObjAsList$IDV), # as.list done within the function
		COVARIATES = translateIntoNamedList(mdlObjAsList$COVARIATES), # as.list done within the function
		VARIABILITY_LEVELS = translateIntoNamedList(mdlObjAsList$VARIABILITY_LEVELS), # as.list done within the function
        STRUCTURAL_PARAMETERS = translateIntoNamedList(mdlObjAsList$STRUCTURAL_PARAMETERS), # as.list done within the function
        VARIABILITY_PARAMETERS = translateIntoNamedList(mdlObjAsList$VARIABILITY_PARAMETERS), # as.list done within the function
        RANDOM_VARIABLE_DEFINITION = translateIntoNamedList(mdlObjAsList$RANDOM_VARIABLE_DEFINITION), # as.list done within the function
        INDIVIDUAL_VARIABLES = translateIntoNamedList(mdlObjAsList$INDIVIDUAL_VARIABLES), # as.list done within the function
		MODEL_PREDICTION = .parseModelPredictionListFromJSON(mdlObjAsList$MODEL_PREDICTION),
        OBSERVATION = translateIntoNamedList(mdlObjAsList$OBSERVATION), # as.list done within the function
		GROUP_VARIABLES = translateIntoNamedList(mdlObjAsList$GROUP_VARIABLES), # as.list done within the function
		MODEL_OUTPUT_VARIABLES = translateIntoNamedList(mdlObjAsList$MODEL_OUTPUT_VARIABLES), # as.list done within the function
		# TODO: TBC - These three slots need to be populated
		ESTIMATION = list(),
		SIMULATION = list(),
		TARGET_CODE = as.character(mdlObjAsList$TARGET_CODE)
    )
	
	res
}


.createTaskObj <- function(taskObjAsList) {
	
	if (is.null(taskObjAsList)) {
		stop("Input argument taskObjAsList is null")
	}
	
	res <- new("taskObj",
		ESTIMATE = as.character(taskObjAsList$ESTIMATE),
		SIMULATE = as.character(taskObjAsList$SIMULATE),
		EVALUATE = as.character(taskObjAsList$EVALUATE),
		OPTIMISE = as.character(taskObjAsList$OPTIMISE),
		DATA = as.character(taskObjAsList$DATA),
		MODEL = as.character(taskObjAsList$MODEL),
		TARGET_CODE = as.character(taskObjAsList$TARGET_CODE)
	)  
}



##############################################################
#' write
#'
#' Takes in an instance of R class \link{\code{mogObj}} comprising a single instance of each of:
#' \itemize{
#'   \item{\linkS4class{\code{dataObj}} class}
#'   \item{\linkS4class{\code{parObj}} class}
#'   \item{\linkS4class{\code{mdlObj}} class}
#'   \item{\linkS4class{\code{taskObj}} class}
#' }
#' and a specified file path, and writes out the content of the MOG Object to that file as MDL.
#' 
#' It is recommended that the file not have an extension, whereby the .mdl extension will be appended.
#' 
#' @usage write(myMogObj, 'C:/Users/fred/mymodel')
#'
#' @param object Instance of R class \link{\code{mogObj}}.
#' @param f File path to the .mdl file (optionally without the .mdl extension) that will be created.
#' @param fisServer FISServer instance.
#'
#' @export
#' 
#' @docType methods
#' @rdname write-methods
#' @include telClasses.R

setGeneric("write", function(object, f, fisServer=TEL.getServer()) { 
  standardGeneric("write")
})

#' @rdname write-methods
#' @aliases write,mogObj,mogObj-method
setMethod("write", "mogObj", function(object, f, fisServer=TEL.getServer()) {
    .precondition.checkArgument(is.FISServer(fisServer), "fisServer", "FIS Server instance is required.")
    json <- .generateJSON(object)

    .write.mclobj0(json, f, fisServer = fisServer)
})

.generateJSON <- function(object) {
    if (!validity.mogObj(object)) {
        stop("Object is not a valid MOG Object")
    }
    m = object
    
    # See comment in .createParObj function re the extra transformation required on VARIABILITY slot
    # (the transformation here is the reverse of that in .createParObj function)
    parObjAsList <- .removeNullEntries(list(
        DECLARED_VARIABLES = translateNamedListIntoList(m@parObj@DECLARED_VARIABLES),
        STRUCTURAL = translateNamedListIntoList(m@parObj@STRUCTURAL),
        VARIABILITY = addExtraLayerOfNesting(m@parObj@VARIABILITY),
        # TODO: TBC - These two slots need to be populated
        PRIOR_PARAMETERS = m@parObj@PRIOR_PARAMETERS,
        TARGET_CODE = m@parObj@TARGET_CODE,
        identifier = "parobj"
    ))
    
    if (length(m@parObj@VARIABILITY) > 0) { # trap the empty-list condition
        lapply(1:length(m@parObj@VARIABILITY), function(i) {
            elemName <- names(m@parObj@VARIABILITY)[[i]]
            # Strip off the redundant count from the end of 'special' variability parameter elements
            elemName <- gsub("^(same|diag|matrix)_.*$", "\\1", elemName, fixed=FALSE)
            names(parObjAsList$VARIABILITY[[i]]) <<- elemName
        })
    }
    # Clear the top-level names of the list elements as these names have been moved onto the individual
    # sub-elements (we have added an extra layer of nesting to the VARIABILITY list)
    names(parObjAsList$VARIABILITY) <- NULL
    
    dataObjAsList <- .removeNullEntries(list(
        SOURCE = .removeNullEntries(m@dataObj@SOURCE), # removeNullEntries() required since ignoreChar etc. might not be specified
        DECLARED_VARIABLES = translateNamedListIntoList(m@dataObj@DECLARED_VARIABLES),
        DATA_INPUT_VARIABLES = translateNamedListIntoList(m@dataObj@DATA_INPUT_VARIABLES),
        DATA_DERIVED_VARIABLES = translateNamedListIntoList(m@dataObj@DATA_DERIVED_VARIABLES),
        TARGET_CODE = m@parObj@TARGET_CODE,
        identifier = "dataobj"
    ))
    
    # Enclose the file name in double quotes ready for writing back to MDL
    dataObjAsList$SOURCE$file <- add_quotes(dataObjAsList$SOURCE$file)
    # Similarly for the Ignore character
    dataObjAsList$SOURCE$ignore <- add_quotes(dataObjAsList$SOURCE$ignore)
    
    mdlObjAsList <- .removeNullEntries(list(
        IDV = translateNamedListIntoList(m@mdlObj@IDV),
        COVARIATES = translateNamedListIntoList(m@mdlObj@COVARIATES),
        VARIABILITY_LEVELS = translateNamedListIntoList(m@mdlObj@VARIABILITY_LEVELS),
        STRUCTURAL_PARAMETERS = translateNamedListIntoList(m@mdlObj@STRUCTURAL_PARAMETERS),
        VARIABILITY_PARAMETERS = translateNamedListIntoList(m@mdlObj@VARIABILITY_PARAMETERS),
        RANDOM_VARIABLE_DEFINITION = translateNamedListIntoList(m@mdlObj@RANDOM_VARIABLE_DEFINITION),
        INDIVIDUAL_VARIABLES = translateNamedListIntoList(m@mdlObj@INDIVIDUAL_VARIABLES),
        MODEL_PREDICTION = .parseModelPredictionNamedListIntoJSON(m@mdlObj@MODEL_PREDICTION),
        OBSERVATION = translateNamedListIntoList(m@mdlObj@OBSERVATION),
        GROUP_VARIABLES = translateNamedListIntoList(m@mdlObj@GROUP_VARIABLES),
        MODEL_OUTPUT_VARIABLES = translateNamedListIntoList(m@mdlObj@MODEL_OUTPUT_VARIABLES),
        # TODO: TBC - These three slots need to be populated
        ESTIMATION = m@mdlObj@ESTIMATION,
        SIMULATION = m@mdlObj@SIMULATION,
        TARGET_CODE = m@mdlObj@TARGET_CODE,
        identifier = "mdlobj"
    ))
    
    taskObjAsList <- .removeNullEntries(list(
        ESTIMATE = m@taskObj@ESTIMATE,
        # TODO: TBC - These six slots need to be populated
        SIMULATE = m@taskObj@SIMULATE,
        EVALUATE = m@taskObj@EVALUATE,
        OPTIMISE = m@taskObj@OPTIMISE,
        DATA = m@taskObj@DATA,
        MODEL = m@taskObj@MODEL,
        TARGET_CODE = m@taskObj@TARGET_CODE,
        identifier = "taskobj"
    ))
    
    dataObjName <- m@dataObj@name
    parObjName <- m@parObj@name
    mdlObjName <- m@mdlObj@name
    taskObjName <- m@taskObj@name
    mogDefinitionName <- if (length(m@name) == 0 || m@name == '') "outputMog" else m@name
    
    allObjsAsList <- list(
        dataObjAsList, parObjAsList, mdlObjAsList, taskObjAsList,
        # The mog definition object
        list(
            identifier = "mogobj",
            OBJECTS = setNames(
                vector("list", 4), # Creates an empty list of length 4
                list(dataObjName, parObjName, mdlObjName, taskObjName)
            )
        )
    )
    names(allObjsAsList) <- c(dataObjName, parObjName, mdlObjName, taskObjName, mogDefinitionName)
    
    json <- toJSON(list(allObjsAsList))
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

# Each element in the incoming JSON list can either represent a 'normal' variable
# (i.e. variable name mapping to either a named list of attributes, or mapping to
# an expression held in the special key ".expr"); or it can represent one of the
# special sub-blocks i.e. "DEQ" or "COMPARTMENT" which are stored in the JSON with
# a dot prefix to distinguish them from actual variable names. For each of these
# sub-blocks:
# They contain lists of variables so the only processing they require is
# the standard translation into named list of variables.
# Next, the sub-block name including the dot prefix, is stored in the special
# ".name" attribute.
# Once all the individual Model Prediction items have been processed, the
# resulting list is translated into a named list, comprising both variables
# the aforementioned sub-blocks.
.parseModelPredictionListFromJSON <- function(modPred) {
	
	translateIntoNamedList(lapply(modPred, function(e) {
		for (subBlockName in MODEL_PREDICTION_SUBBLOCKS) {
			if (is.list(e[[subBlockName]])) {
				e <- translateIntoNamedList(e[[subBlockName]])
				e$.name <- subBlockName
			}
		}
		e
	}))
	
}

# The reverse of .parseModelPredictionListFromJSON(), used when writing the JSON back out.
# The special sub-blocks i.e. "DEQ" and "COMPARTMENT" have translateNamedListIntoList() applied
# applied to them to 'move' the name of each list element onto an attribute named ".name" of
# the element instead, before translateNamedListIntoList() is applied to the Model Prediction
# named list itself.
.parseModelPredictionNamedListIntoJSON <- function(modPredNamedList) {
	
	lapply(translateNamedListIntoList(modPredNamedList), function(e) {
		for (subBlockName in MODEL_PREDICTION_SUBBLOCKS) {
			if (e$.name == subBlockName) {
				e$.name <- NULL
				e <- list(translateNamedListIntoList(e))
				names(e) <- subBlockName
				return(e)
			}
		}
		e
	})
	
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
	cmd <- URLencode(getMDLToPharmMLURL(fisServer))
	
	if (!file.exists(f)) {
		stop("Error, MDL file \"", f, "\" does not exist.");
	}
	
	postfield <- URLencode(paste0(
		"fileName=", normalizePath(f, winslash="/"),
		"&outputDir=", normalizePath(tempdir())
	))
	
	h = basicTextGatherer()
	RCurl:::curlPerform(url = cmd, postfields = postfield, writefunction = h$update)
	retVal <- h$value()

	if (is.null(retVal) || nchar(retVal) <= 1) {
		stop("Failed to convert MDL to PharmML; no PharmML file path was returned from the conversion service.")
	} else if (!file.exists(retVal)) {
		stop("Failed to convert MDL to PharmML; PharmML file path returned from the conversion service was \"", retVal, "\" but this file does not exist.");
	}
	retVal
}
