
##############################################################
#' .parseMDLFile
#'
#' Calls the MDL parser, providing either a file path or URL, and returns
#' a list of all objects in the MDL file. For data objects, parameter objects and
#' model objects, R objects of class \code{dataObj}, \code{parObj} and \code{mdlObj} 
#' are returned. This function is not intended for use by the user. It is recommended that 
#' the functions \code{getDataObjects}, \code{getMOG}, \code{getParameterObjects} 
#' and/or \code{getModelObjects}
#' are used instead.
#'
#' @usage .parseMDLFile("myMDLFile")
#'
#' @param f File path or URL of the .mdl file containing the objects.
#' @param type String specifying the type of objects to extract. Possible values are
#' \code{dataobj}, \code{parobj}, \code{mdlobj}, \code{taskobj} and \code{mogobj}
#' @param name (Optional) Specifies the dataobj/parobj/mdlobj/taskobj/mogobj object
#' , by name, to be retrieved. If multiple data objects exist in the .mdl file 
#' then using the name argument helps users target a specific data object.
#' @param HOST hostname of the server running the FIS service, defaults to localhost
#' @param PORT port of the server running the FIS service, defaults to 9010
#'
#' @return A list of matching objects which are contained in the MDL file or URL.
#' @include telClasses.R

.parseMDLFile <- function(f, name, type, HOST='localhost', PORT='9010') {

  if (!type%in%c("parobj", "taskobj", "dataobj", "mdlobj", "mogObj")) {
    stop("Type specified is not one of 'parobj', 'taskobj', 'dataobj', 'mdlobj' or 'mogobj'")
  }
  
  # Call parser and read in the JSON data
  raw <- .parseMDLFile0(f, HOST, PORT);

  if (!missing(name)) {
  
    .extractNamedObject(raw, name)
    
  } else {
	  
    .extractTypeObject(raw, type)
  
  }
  
}

.parseMDLFile0 <- function(f, HOST='localhost', PORT='9010') {

  # Call parser and read in the JSON data:
  cmd <- URLencode(paste0("http://", HOST, ":", PORT, "/readmdl?fileName=", normalizePath(f, winslash="/")))

  json <- httpGET(cmd)
  fromJSON(json)[[1]]
}


.extractNamedObject <- function(raw, name) {
	
  val <- raw[name] # Creates a list containing the single matching object
  
  .extractTypeObject(val, type=val[[1]]$identifier)
}


.extractTypeObject <- function(raw, type) {
	
	switch (type,
		dataobj  = .extractAnyObj(raw, "dataobj", .createDataObj),
		parobj  = .extractAnyObj(raw, "parobj", .createParObj),
		mdlobj  = .extractAnyObj(raw, "mdlobj", .createMdlObj),
		taskobj  = .extractAnyObj(raw, "taskobj", .createTaskObj),
	)
  
}


.extractAnyObj <- function(raw, identifier, createObjFn) {
	# Extract identifiers
	logi <- sapply(raw, 
		function(x){
			x$identifier==identifier
		}
	)
	
	subList <- raw[logi]
	
	res <- lapply(subList, createObjFn)
	
	names(res) <- names(subList)
	
	return(res)
}


.createParObj <- function(dat){

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

	res <- new("parObj", 
		STRUCTURAL = translateIntoNamedList(dat$STRUCTURAL), # as.list done within the function
		VARIABILITY = lapply(as.list(dat$VARIABILITY), function(x) x[[1]]),
		# TODO: TBC - These need to be populated
		PRIOR_PARAMETERS = list(),
		TARGET_CODE = as.character(dat$TARGET_CODE)
	)
	
	diagCnt <- 0; matrixCnt <- 0; sameCnt <- 0;
	names(res@VARIABILITY) <- lapply(as.list(dat$VARIABILITY), function(x) {
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
	
	return(res)
} 


.createDataObj <- function(dat){

    res <- new("dataObj",
        DATA_INPUT_VARIABLES = translateIntoNamedList(dat$DATA_INPUT_VARIABLES), # as.list done within the function
        SOURCE = as.list(dat$SOURCE),
        # TODO: TBC - These need to be populated
        DATA_DERIVED_VARIABLES = list(),
		TARGET_CODE = as.character(dat$TARGET_CODE)
    )
    
    # Unquote the file name so that the file name within the R object is more easily manipulated
    res@SOURCE$file <- strip_quotes(res@SOURCE$file)
    # Similarly for the Ignore character
    res@SOURCE$ignore <- strip_quotes(res@SOURCE$ignore)

    res
}


.createMdlObj <- function(dat){

    res <- new("mdlObj",
        STRUCTURAL_PARAMETERS = translateIntoNamedList(dat$STRUCTURAL_PARAMETERS), # as.list done within the function
        VARIABILITY_PARAMETERS = translateIntoNamedList(dat$VARIABILITY_PARAMETERS), # as.list done within the function
        INDIVIDUAL_VARIABLES = translateIntoNamedList(dat$INDIVIDUAL_VARIABLES), # as.list done within the function
        RANDOM_VARIABLE_DEFINITION = translateIntoNamedList(dat$RANDOM_VARIABLE_DEFINITION),
		MODEL_OUTPUT_VARIABLES = translateIntoNamedList(dat$MODEL_OUTPUT_VARIABLES), # as.list done within the function
        MODEL_INPUT_VARIABLES = translateIntoNamedList(dat$MODEL_INPUT_VARIABLES), # as.list done within the function
        OBSERVATION = translateIntoNamedList(dat$OBSERVATION), # as.list done within the function
        MODEL_PREDICTION = new("modPred",
            ODE = as.character(dat$MODEL_PREDICTION$ODE),
            LIBRARY = as.character(dat$MODEL_PREDICTION$LIBRARY),
            content = as.character(dat$MODEL_PREDICTION$content)
        ),
		# TODO: TBC - These need to be populated
        GROUP_VARIABLES = list(),
		ESTIMATION = list(),
		SIMULATION = list(),
		TARGET_CODE = as.character(dat$TARGET_CODE)
    )

}


.createTaskObj <- function(dat){
	res <- new("taskObj",
		ESTIMATE = as.character(dat$ESTIMATE),
		SIMULATE = as.character(dat$SIMULATE),
		EVALUATE = as.character(dat$EVALUATE),
		OPTIMISE = as.character(dat$OPTIMISE),
		DATA = as.character(dat$DATA),
		MODEL = as.character(dat$MODEL),
		TARGET_CODE = as.character(dat$TARGET_CODE)
	)  
}



##############################################################
#' write
#'
#' Takes in an instance of R class mogObj comprising a single instance of each of:
#'  - dataObj class
#'  - parObj class
#'  - mdlObj class
#'  - taskObj class
#' and a specified file, and writes out the content of the MOG Object to that file as MDL.
#' It is recommended that the file not have an extension, whereby the .mdl extension will be appended.
#' 
#' @usage write(myMogObj, 'C:/Users/username/mymodel')
#'
#' @param m instance of R class mogObj
#' @param f file path to the .mdl file (optionally without the .mdl extension) that will be created
#' @param HOST hostname of the server running the FIS service, defaults to localhost
#' @param PORT port of the server running the FIS service, defaults to 9010
#'
#' @include telClasses.R
#' @export
#' @docType methods
#' @rdname write-methods

setGeneric("write", function(object, f, HOST='localhost', PORT='9010') { 
  standardGeneric("write")
})

#' @rdname write-methods
#' @aliases write,mogObj,mogObj-method
setMethod("write", "mogObj", function(object, f, HOST='localhost', PORT='9010') {

    if (!validity.mogObj(object)) {
        stop("Object is not a valid MOG Object")
    }
    m = object
	
	# See comment in .createParObj function re the extra transformation required on VARIABILITY slot
    # (the transformation here is the reverse of that in .createParObj function)
    parObjAsList <- .removeNullEntries(list(
      STRUCTURAL = translateNamedListIntoList(m@parObj@STRUCTURAL),
      VARIABILITY = lapply(m@parObj@VARIABILITY, list),
	  # TODO: TBC - These two slots need to be populated
      PRIOR_PARAMETERS = m@parObj@PRIOR_PARAMETERS,
	  TARGET_CODE = m@parObj@TARGET_CODE,
      identifier = "parobj"
	))
	lapply(1:length(m@parObj@VARIABILITY), function(i) {
		elemName <- names(m@parObj@VARIABILITY)[[i]]
		# Strip off the redundant count from the end of 'special' variability parameter elements
		elemName <- gsub("^(same|diag|matrix)_.*$", "\\1", elemName, fixed=FALSE)
		names(parObjAsList$VARIABILITY[[i]]) <<- elemName
	})
	names(parObjAsList$VARIABILITY) <- NULL
    
    dataObjAsList <- .removeNullEntries(list(
        DATA_INPUT_VARIABLES = translateNamedListIntoList(m@dataObj@DATA_INPUT_VARIABLES),
        SOURCE = .removeNullEntries(m@dataObj@SOURCE), # removeNullEntries() required since ignoreChar etc. might not be specified
		# TODO: TBC - These two slots need to be populated
        DATA_DERIVED_VARIABLES = m@dataObj@DATA_DERIVED_VARIABLES,
		TARGET_CODE = m@parObj@TARGET_CODE,
        identifier = "dataobj"
    ))
    
    # Enclose the file name in double quotes ready for writing back to MDL
	dataObjAsList$SOURCE$file <- add_quotes(dataObjAsList$SOURCE$file)
    # Similarly for the Ignore character
    dataObjAsList$SOURCE$ignore <- add_quotes(dataObjAsList$SOURCE$ignore)
    
    mdlObjAsList <- .removeNullEntries(list(
        STRUCTURAL_PARAMETERS = translateNamedListIntoList(m@mdlObj@STRUCTURAL_PARAMETERS),
        VARIABILITY_PARAMETERS = translateNamedListIntoList(m@mdlObj@VARIABILITY_PARAMETERS),
        INDIVIDUAL_VARIABLES = translateNamedListIntoList(m@mdlObj@INDIVIDUAL_VARIABLES),
        RANDOM_VARIABLE_DEFINITION = translateNamedListIntoList(m@mdlObj@RANDOM_VARIABLE_DEFINITION),
		MODEL_OUTPUT_VARIABLES = translateNamedListIntoList(m@mdlObj@MODEL_OUTPUT_VARIABLES),
        MODEL_INPUT_VARIABLES = translateNamedListIntoList(m@mdlObj@MODEL_INPUT_VARIABLES),
        OBSERVATION = translateNamedListIntoList(m@mdlObj@OBSERVATION),
        MODEL_PREDICTION = .removeNullEntries(list(
            ODE = m@mdlObj@MODEL_PREDICTION@ODE,
            LIBRARY = m@mdlObj@MODEL_PREDICTION@LIBRARY,
            content = m@mdlObj@MODEL_PREDICTION@content
        )),
		# TODO: TBC - These four slots need to be populated
        GROUP_VARIABLES = m@mdlObj@GROUP_VARIABLES,
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
    
    json <- toJSON(list(list(
        outputMog_task = taskObjAsList,
		outputMog_par = parObjAsList,
		outputMog_mdl = mdlObjAsList,
		outputMog_dat = dataObjAsList
    )))

    .write.mclobj0(json, f, HOST, PORT)
})

.write.mclobj0 <- function(json, f, HOST='localhost', PORT='9010') {

    fullPath <- normalizePath(f, winslash="/", mustWork=FALSE)

    wreq <- URLencode(toJSON(list(
        fileName = fullPath,
        fileContent = json
    )), reserved=TRUE) # ensures that & characters etc. get encoded too

    # Call parser and post the JSON data:
    cmd <- URLencode(paste0("http://", HOST, ":", PORT, "/writemdl"))

    postfield <- sprintf('%s%s','writeRequest=',wreq)
    
	h = basicTextGatherer()
    RCurl:::curlPerform(url = cmd, postfields = postfield, writefunction = h$update)
	retStatus <- h$value()

	if (fromJSON(retStatus)$status != "Successful") {
		stop("Failed to send write request. Details of the error: ", retStatus)
	}
	# Don't print out the JSON-format return status
	invisible(retStatus)
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

# Incoming (JSON->R) lists of variables etc. have their names as attributes of the list
# elements; use these as the names in the creation of a named list of these variables etc.,
# to go in the slots in the R classes.
# Also, remove the name attribute to avoid duplication and confusion.
# Named lists allow for easier access and manipulation of the R objects by R workflows.
# This function also handles null which gets converted into an empty list.
translateIntoNamedList <- function(x) {
	l <- as.list(x) # Handle null which gets converted into an empty list
	names(l) <- lapply(l, function(e) { e$name }) # 'e' is the list element
	lapply(l, function(e) { e$name <- NULL; e }) # 'e' is the list element
}

# When writing out JSON, the named lists from the R objects need to have their names
# stripped off, in order that they are written out as lists in the JSON rather than
# (unordered) maps.
# Each name is 'moved' onto an attribute named "name" of the list element instead.
translateNamedListIntoList <- function(l) {
	l <- lapply(names(l), function(n) { l[[n]]$name <- n; l[[n]] } ) # 'n' is the name of the list element
	names(l) <- NULL
	l
}
