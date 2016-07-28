
MDL_FILE_EXT <- 'mdl'
JSON_FILE_EXT <- 'json'

MOG_OBJECT_TYPES <- c("dataObj", "parObj", "mdlObj", "taskObj", "priorObj", "designObj")


################################################################################
#' @name parseMDLFile
#' @aliases .parseMDLFile
#' @title Parse MDL File
#'
#' @description Calls the MDL parser, providing a specified path to an MDL file, and returns a list of
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
#' @include Classes.R
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

# @title Read MDL input from MDL or JSON file
# @description Returns object raw in \code{\link{getMDLObjects}}
# @inheritParams parseMDLFile
# @seealso \code{\link{readMDL}}, \code{\link{getMDLObjects}}
# @examples
# jpath <- system.file(package = "ddmore", "training", "data", 
#        "UseCase2.json")
# mockServer <- createMockFISServer(jobStatusPollingDelay = 1)
# DDMORE.setServer(mockServer)
# ddmore:::.parseMDLFile0(f = jpath, fisServer = DDMORE.getServer())

.parseMDLFile0 <- function(f, fisServer) {
    .precondition.checkArgument(is.FISServer(fisServer), "fisServer", "FIS Server instance is required.")
	if (file_ext(f) == MDL_FILE_EXT) {
		json <- readMDL(fisServer, f)
	} else if (file_ext(f) == JSON_FILE_EXT) { # For testing purposes
		json <- readLines(f, warn=FALSE)[[1]]
	} else {
		stop(paste("The file extension for the file being parsed into R objects should be .mdl; the filename was", f))
	}
	
	if (json == "") {
		# TODO: Write the conversionReport.log file to a suitable place, and tell the user where to find it
		stop("Unable to generate JSON representation of MDL file; it likely has syntax errors.")
	}
  
    fromJSON(json)

}


.extractNamedObject <- function(raw, name) {
	
    # Creates a list containing the single matching object
	val <- raw[sapply(raw, function(e) {e$name == name})] 
  
	.extractObjs(val)[[1]]
}


# Creates a list containing the matching objects
#
# @examples
# jpath <- system.file(package = "ddmore", "training", "data", 
#        "UseCase2.json")
# mockServer <- createMockFISServer(jobStatusPollingDelay = 1)
# DDMORE.setServer(mockServer)
# rawEg <- ddmore:::.parseMDLFile0(f = jpath, fisServer = DDMORE.getServer())
# ddmore:::.extractTypeObjects(raw = rawEg, type = "dataObj")
.extractTypeObjects <- function(raw, type) {
    val <- raw[sapply(raw, function(e) {e$type == type})]
	.extractObjs(val)
}


.extractObjs <- function(raw) {
	
	applyObjectNamesToListOfObjects(
		lapply(raw, function(b) {
	        if ("type" %in% names(b)) {
                if (all(c("blocks", "name") %in% names(b))) {
                    createObjFn <- switch(casefold(b$type),
                        dataobj = .createDataObj,
                        parobj = .createParObj,
                        mdlobj = .createMdlObj,
                        taskobj = .createTaskObj,
                        priorobj = .createPriorObj,
                        designobj = .createDesignObj
                    )
                    
                    createObjFn(b$blocks, b$name)
                } else {
                    warning("expected elements blocks and name in raw data passed to .extractObjs")
                }
            }
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
		VARIABILITY_LEVELS = as.list(mdlObjAsList$VARIABILITY_LEVELS), # removeExtraLayerOfNesting handles null
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
		ESTIMATE = if(!is.null(taskObjAsList$ESTIMATE)) as.list(taskObjAsList$ESTIMATE) else NULL, # as.list handles null
		SIMULATE = if(!is.null(taskObjAsList$SIMULATE)) as.list(taskObjAsList$SIMULATE) else NULL, # as.list handles null
		EVALUATE = if(!is.null(taskObjAsList$EVALUATE)) as.list(taskObjAsList$EVALUATE) else NULL,
		OPTIMISE = if(!is.null(taskObjAsList$OPTIMISE)) as.list(taskObjAsList$OPTIMISE) else NULL,
		name = name
	)
	
	taskObj
}


.createDesignObj <- function(objAsList, name) {
	
	if (is.null(objAsList)) {
		stop("Input argument objAsList is null")
	}
	
    designObj <- new("designObj",
		DECLARED_VARIABLES = as.list(objAsList$DECLARED_VARIABLES), # as.list handles null; TODO Transformation like translateIntoNamedList to be applied to this instead
		DESIGN_PARAMETERS = as.list(objAsList$DESIGN_PARAMETERS), # as.list handles null; TODO Transformation like translateIntoNamedList to be applied to this instead
		INTERVENTION = as.list(objAsList$INTERVENTION), # as.list handles null; TODO Transformation like translateIntoNamedList to be applied to this instead
		STUDY_DESIGN = as.list(objAsList$STUDY_DESIGN), # as.list handles null; TODO Transformation like translateIntoNamedList to be applied to this instead
		SAMPLING = as.list(objAsList$SAMPLING), # as.list handles null; TODO Transformation like translateIntoNamedList to be applied to this instead
		DESIGN_SPACES = as.list(objAsList$DESIGN_SPACES), # as.list handles null; TODO Transformation like translateIntoNamedList to be applied to this instead
		COVARIATES = as.list(objAsList$COVARIATES), # as.list handles null; TODO Transformation like translateIntoNamedList to be applied to this instead
		POPULATION = as.list(objAsList$POPULATION), # as.list handles null; TODO Transformation like translateIntoNamedList to be applied to this instead
		name = name
    )
	
	designObj
}


.createPriorObj <- function(objAsList, name) {
	
	if (is.null(objAsList)) {
		stop("Input argument objAsList is null")
	}
	# as.list handles null; 
    # TODO Transformation like translateIntoNamedList to be applied to this instead
    priorObj <- new("priorObj",
        PRIOR_PARAMETERS = as.list(objAsList$PRIOR_PARAMETERS),
        PRIOR_VARIABLE_DEFINITION = as.list(objAsList$PRIOR_VARIABLE_DEFINITION),
        PRIOR_SOURCE = as.list(objAsList$PRIOR_SOURCE),
        INPUT_PRIOR_DATA = as.list(objAsList$INPUT_PRIOR_DATA),
		name = name
    )
	priorObj
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
#' @include Classes.R
#' @rdname writeMogObj-methods

setGeneric("writeMogObj", function(object, f, fisServer=DDMORE.getServer()) { 
  standardGeneric("writeMogObj")
})

#' @rdname writeMogObj-methods
#' @aliases writeMogObj,mogObj,mogObj-method
setMethod("writeMogObj", "mogObj", function(object, f, fisServer=DDMORE.getServer()) {
    .precondition.checkArgument(is.FISServer(fisServer), "fisServer", "FIS Server instance is required.")
    json <- .generateJSON(object)

    .write.mclobj0(json, f, fisServer = fisServer)
})

setGeneric(".prepareForJSON", function(object) { 
  standardGeneric(".prepareForJSON")
})

setMethod(".prepareForJSON", "dataObj", function(object) {
  .precondition.checkArgument(validity.dataObj(object), "object", "valid dataObj required.")
  dataObjBlocksSOURCE <- lapply(object@SOURCE, function(srcfile) {
    # Enclose the file name in double quotes ready for writing back to MDL
    srcfile$file <- add_quotes(srcfile$file)
    # Similarly for the Ignore character
    srcfile$ignore <- add_quotes(srcfile$ignore)
    srcfile
  })
  
  dataObjBlocks <- .removeNullEntries(list(
    SOURCE = addExtraLayerOfNesting(dataObjBlocksSOURCE),
    DECLARED_VARIABLES = object@DECLARED_VARIABLES,
    DATA_INPUT_VARIABLES = addExtraLayerOfNesting(object@DATA_INPUT_VARIABLES),
    DATA_DERIVED_VARIABLES = object@DATA_DERIVED_VARIABLES
  ))
  dataObjName <- object@name
  list(name=dataObjName, type="dataObj", blocks=dataObjBlocks)
})

setMethod(".prepareForJSON", "priorObj", function(object) {
  .precondition.checkArgument(validity.priorObj(object), "object", "valid priorObj required.")
  
  objBlocks <- .removeNullEntries(list(
    PRIOR_PARAMETERS = object@PRIOR_PARAMETERS,
    PRIOR_VARIABLE_DEFINITION = object@PRIOR_VARIABLE_DEFINITION,
    PRIOR_SOURCE = object@PRIOR_SOURCE,
    INPUT_PRIOR_DATA = object@INPUT_PRIOR_DATA
  ))
  objName <- object@name
  list(name = objName, 
    type = "priorObj", 
    blocks = objBlocks)
})


setMethod(".prepareForJSON", "designObj", function(object) {
  .precondition.checkArgument(validity.designObj(object), "object", "valid designObj required.")
  
  objBlocks <- .removeNullEntries(list(
    DECLARED_VARIABLES = object@DECLARED_VARIABLES,
    DESIGN_PARAMETERS = object@DESIGN_PARAMETERS,
    INTERVENTION = object@INTERVENTION,
    STUDY_DESIGN = object@STUDY_DESIGN,
    SAMPLING = object@SAMPLING,
    POPULATION = object@POPULATION,
    DESIGN_SPACES = object@DESIGN_SPACES,
    COVARIATES = object@COVARIATES
  ))
  objName <- object@name
  list(name=objName, type="designObj", blocks=objBlocks)
})

setMethod(".prepareForJSON", "parObj", function(object) {
  .precondition.checkArgument(validity.parObj(object), "object", "valid parObj required.")
  parObjBlocks <- .removeNullEntries(list(
    DECLARED_VARIABLES = object@DECLARED_VARIABLES,
    STRUCTURAL = addExtraLayerOfNesting(object@STRUCTURAL),
    VARIABILITY = addExtraLayerOfNesting(object@VARIABILITY)
  ))
  parObjName <- object@name
  
  list(name=parObjName, type="parObj", blocks=parObjBlocks)
  })

setMethod(".prepareForJSON", "mdlObj", function(object) {
  .precondition.checkArgument(validity.mdlObj(object), "object", "valid mdlObj required.")
  mdlObjBlocks <- .removeNullEntries(list(
    IDV = object@IDV,
    COVARIATES = object@COVARIATES,
    VARIABILITY_LEVELS = object@VARIABILITY_LEVELS,
    STRUCTURAL_PARAMETERS = object@STRUCTURAL_PARAMETERS,
    VARIABILITY_PARAMETERS = object@VARIABILITY_PARAMETERS,
    RANDOM_VARIABLE_DEFINITION = object@RANDOM_VARIABLE_DEFINITION,
    INDIVIDUAL_VARIABLES = object@INDIVIDUAL_VARIABLES,
    MODEL_PREDICTION = object@MODEL_PREDICTION,
    OBSERVATION = object@OBSERVATION,
    GROUP_VARIABLES = object@GROUP_VARIABLES
  ))
  
  mdlObjName <- object@name
  
  list(name=mdlObjName, type="mdlObj", blocks=mdlObjBlocks)
})

setMethod(".prepareForJSON", "taskObj", function(object) {
  .precondition.checkArgument(validity.taskObj(object), "object", "valid taskObj required.")
  
  taskObjBlocks <- list()
  if(!is.null(object@ESTIMATE)) {
    taskObjBlocks$ESTIMATE <- object@ESTIMATE
  }
  if(!is.null(object@SIMULATE)) {
    taskObjBlocks$SIMULATE <- object@SIMULATE
  }
  if(!is.null(object@EVALUATE)) {
    taskObjBlocks$EVALUATE <- object@EVALUATE
  }
  
  if(!is.null(object@OPTIMISE)) {
    taskObjBlocks$OPTIMISE <- object@OPTIMISE
  }
  
  taskObjName <- object@name
  
  list(name=taskObjName, type="taskObj", blocks=taskObjBlocks)
})

.generateJSON <- function(object) {
    if (!validity.mogObj(object)) {
        stop("Object is not a valid MOG Object")
    }
    m = object
    allObjsAsList <- list()
    if(!is.null(m@dataObj)) {
      allObjsAsList <- c(allObjsAsList, list(.prepareForJSON(m@dataObj)))
    }
    if(!is.null(m@designObj)) {
      allObjsAsList <- c(allObjsAsList, list(.prepareForJSON(m@designObj)))
    }
    if(!is.null(m@parObj)) {
      allObjsAsList <- c(allObjsAsList, list(.prepareForJSON(m@parObj)))
    }
    if(!is.null(m@priorObj)) {
      allObjsAsList <- c(allObjsAsList, list(.prepareForJSON(m@priorObj)))
    }
    if(!is.null(m@mdlObj)) {
      allObjsAsList <- c(allObjsAsList, list(.prepareForJSON(m@mdlObj)))
    }
    if(!is.null(m@taskObj)) {
      allObjsAsList <- c(allObjsAsList, list(.prepareForJSON(m@taskObj)))
    }
    mogDefinitionName <- if (length(m@name) == 0 || m@name == '') "outputMog" else m@name
	
	# Extra layer of nesting needed for the JSON - ensures that order of items is maintained (although it doesn't actually matter for this block)
    mogObjBlocks <- list()
    mogObjBlocks$OBJECTS <- lapply(allObjsAsList, function(x) { return(list(list("type" = x$type)))} )
    for (i in seq(1:length(allObjsAsList))) {
      names(mogObjBlocks$OBJECTS[[i]]) <- c(allObjsAsList[[i]]$name)
    }
    
    if(length(object@info)>0) {
        mogObjBlocks$INFO <- object@info
    }
    allObjsAsList <- c(allObjsAsList, list(
        list(name=mogDefinitionName, type="mogObj", blocks=mogObjBlocks)))
    
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
as.PharmML <- function(f, fisServer = DDMORE.getServer()) {
    .precondition.checkArgument(is.FISServer(fisServer), "fisServer", "FIS Server instance is required.")
    .precondition.checkArgument(file.exists(f), "f", sprintf("MDL file %s must exist.",f))
	
	resultFile <- MDLToPharmML(fisServer, f)
	
	if (!file.exists(resultFile)) {
		stop("Failed to convert MDL to PharmML; PharmML file path returned from the conversion service was \"", 
            resultFile, "\" but this file does not exist.")
	}
	return(resultFile)
}
