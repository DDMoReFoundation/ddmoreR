
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
#' \code{parobj}, \code{taskobj}, \code{dataobj} and \code{mdlobj}
#' @param name (Optional) Specifies the data object item, by name, to be 
#' retrieved by getDataObjects. If multiple data objects exist in the .mdl file 
#' then using the name argument helps users target a specific data object.
#' @param HOST hostname of the server running the FIS service, defaults to localhost
#' @param PORT port of the server running the FIS service, defaults to 9010
#'
#' @return A list of objects which are contained in the MDL file or URL.
#' @include telClasses.R

.parseMDLFile <- function(f, name, type, HOST='localhost', PORT='9010') {

  if(!type%in%c("parobj", "taskobj", "dataobj", "mdlobj")){
    stop("Type specified is not one of 'parobj', 'taskobj', 'dataobj' or 'mdlobj'")
  }
  
  # Call parser and read in the JSON data
  raw <- .parseMDLFile0(f, HOST, PORT);

  if (!missing(name)) {
  
    .extractNamedObject(raw, name)
    return(res)
    
  } else {
  
    res <- .extractTypeObject(raw, type)
    return(res)
  
  }
  
}

.parseMDLFile0 <- function(f, HOST='localhost', PORT='9010') {

  # Call parser and read in the JSON data:
  cmd <- URLencode(paste0("http://", HOST, ":", PORT, "/readmdl?fileName=", normalizePath(f, winslash="/")))

  json <- httpGET(cmd)
  fromJSON(json)[[1]]
}


.extractNamedObject <- function(dat, name){
  
  val <- dat[name]
  
  res <- .extractTypeObject(val, type=val$identifier)
  
  return(res)

}


.extractTypeObject <- function(dat, type){

  switch(type, 
    parobj  = .extractParObj(dat), 
    mdlobj  = .extractMdlObj(dat), 
    dataobj = .extractDataObj(dat),
    taskobj = .extractTaskObj(dat)
  )
  
}

.extractParObj <- function(dat){
  #Extract identifiers
  logi <- sapply(dat, 
    function(x){
      x$identifier=="parobj"
    }
  )
  
  subList <- dat[logi]
  
  res <- lapply(subList, .createParObj)

  names(res) <- names(subList)
  
  return(res)

}

.extractDataObj <- function(dat){
  #Extract identifiers
  logi <- sapply(dat, 
    function(x){
      x$identifier=="dataobj"
    }
  )
  
  subList <- dat[logi]
  
  res <- lapply(subList, .createDataObj)
  
  names(res) <- names(subList)
  
  return(res)

}

.extractMdlObj <- function(dat){
  #Extract identifiers
  logi <- sapply(dat, 
    function(x){
      x$identifier=="mdlobj"
    }
  )
  
  subList <- dat[logi]
  
  res <- lapply(subList, .createMdlObj)
  
  names(res) <- names(subList)
  
  return(res)

}

.extractTaskObj <- function(dat){
  #Extract identifiers
  logi <- sapply(dat, 
    function(x){
      x$identifier=="taskobj"
    }
  )
  
  subList <- dat[logi]
  
  res <- lapply(subList, .createTaskObj)
  
  names(res) <- names(subList)
  
  return(res)

}


.createParObj <- function(dat){

  res <- new("parObj", 
      STRUCTURAL = translateIntoNamedList(dat$STRUCTURAL), # as.list done within the function
      PRIOR = as.list(dat$PRIOR), # TODO: TBC
      VARIABILITY = as.list(dat$VARIABILITY)
    )
  
  return(res)

} 


.createDataObj <- function(dat){

    res <- new("dataObj",
        DATA_INPUT_VARIABLES = translateIntoNamedList(dat$DATA_INPUT_VARIABLES), # as.list done within the function
        SOURCE = as.list(dat$SOURCE),
        # TODO: TBC - These need to be populated
        RSCRIPT = list(),
        HEADER = list(),
        FILE = list(),
        DESIGN = list(),
        DATA_DERIVED_VARIABLES = list()
    )
    
    # Unquote the file name so that the file name within the R object is more easily manipulated
    res@SOURCE$file <- strip_quotes(res@SOURCE$file)
    # Similarly for the Ignore character
    res@SOURCE$ignore <- strip_quotes(res@SOURCE$ignore)

    res
}


.createMdlObj <- function(dat){

    res <- new("mdlObj",
        MODEL_INPUT_VARIABLES = translateIntoNamedList(dat$MODEL_INPUT_VARIABLES), # as.list done within the function
        STRUCTURAL_PARAMETERS = translateIntoNamedList(dat$STRUCTURAL_PARAMETERS), # as.list done within the function
        VARIABILITY_PARAMETERS = translateIntoNamedList(dat$VARIABILITY_PARAMETERS), # as.list done within the function
        GROUP_VARIABLES = as.character(dat$GROUP_VARIABLES),
        RANDOM_VARIABLE_DEFINITION = translateIntoNamedList(dat$RANDOM_VARIABLE_DEFINITION),
        INDIVIDUAL_VARIABLES = as.character(dat$INDIVIDUAL_VARIABLES),
        MODEL_PREDICTION = new("modPred",
            ODE = as.character(dat$MODEL_PREDICTION$ODE),
            LIBRARY = as.character(dat$MODEL_PREDICTION$LIBRARY),
            content = as.character(dat$MODEL_PREDICTION$content)
        ),
        OBSERVATION = as.character(dat$OBSERVATION),
		ESTIMATION = as.character(dat$ESTIMATION),
		MODEL_OUTPUT_VARIABLES = translateIntoNamedList(dat$MODEL_OUTPUT_VARIABLES) # as.list done within the function
    )

}


.createTaskObj <- function(dat){
	res <- new("taskObj",
		content = as.character(dat$content)
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
    
    parObjAsList <- .removeNullEntries(list(
      STRUCTURAL = translateNamedListIntoList(m@parObj@STRUCTURAL),
      PRIOR = m@parObj@PRIOR,
      VARIABILITY = m@parObj@VARIABILITY,
      identifier = "parobj"
	))
    
    dataObjAsList <- .removeNullEntries(list(
        DATA_INPUT_VARIABLES = translateNamedListIntoList(m@dataObj@DATA_INPUT_VARIABLES),
        SOURCE = .removeNullEntries(m@dataObj@SOURCE), # removeNullEntries() required since ignoreChar etc. might not be specified
        RSCRIPT = m@dataObj@RSCRIPT,
        HEADER = m@dataObj@HEADER,
        FILE = m@dataObj@FILE,
        DESIGN = m@dataObj@DESIGN,
        DATA_DERIVED_VARIABLES = m@dataObj@DATA_DERIVED_VARIABLES,
        identifier = "dataobj"
    ))
    
    # Enclose the file name in double quotes ready for writing back to MDL
	dataObjAsList$SOURCE$file <- add_quotes(dataObjAsList$SOURCE$file)
    # Similarly for the Ignore character
    dataObjAsList$SOURCE$ignore <- add_quotes(dataObjAsList$SOURCE$ignore)
    
    mdlObjAsList <- .removeNullEntries(list(
        MODEL_INPUT_VARIABLES = translateNamedListIntoList(m@mdlObj@MODEL_INPUT_VARIABLES),
        STRUCTURAL_PARAMETERS = translateNamedListIntoList(m@mdlObj@STRUCTURAL_PARAMETERS),
        VARIABILITY_PARAMETERS = translateNamedListIntoList(m@mdlObj@VARIABILITY_PARAMETERS),
        GROUP_VARIABLES = m@mdlObj@GROUP_VARIABLES,
        RANDOM_VARIABLE_DEFINITION = translateNamedListIntoList(m@mdlObj@RANDOM_VARIABLE_DEFINITION),
        INDIVIDUAL_VARIABLES = m@mdlObj@INDIVIDUAL_VARIABLES,
        MODEL_PREDICTION = .removeNullEntries(list(
            ODE = m@mdlObj@MODEL_PREDICTION@ODE,
            LIBRARY = m@mdlObj@MODEL_PREDICTION@LIBRARY,
            content = m@mdlObj@MODEL_PREDICTION@content
        )),
        OBSERVATION = m@mdlObj@OBSERVATION,
		ESTIMATION = m@mdlObj@ESTIMATION,
		MODEL_OUTPUT_VARIABLES = translateNamedListIntoList(m@mdlObj@MODEL_OUTPUT_VARIABLES),
        identifier = "mdlobj"
    ))
    
    taskObjAsList <- .removeNullEntries(list(
        content = m@taskObj@content,
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
	retStatus
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
# This allows for easier access and manipulation of the R objects by R workflows.
# This function also handles null which gets converted into an empty list.
translateIntoNamedList <- function(x) {
	l <- as.list(x) # Handle null which gets converted into an empty list
	names(l) <- lapply(l, function(y) { y$name })
	l
}

# When writing out JSON, the named lists from the R objects need to have their names
# stripped off, in order that they are written out as lists in the JSON rather than
# (unordered) maps.
translateNamedListIntoList <- function(l) {
	names(l) <- NULL
	l
}
