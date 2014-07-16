
##############################################################
#' .parseMDLFile
#'
#' Calls the MDL parser, providing either a file path or URL, and returns
#' a list of all objects in the MDL file. For data objects, parameter objects and
#' model objects, R objects of class dataObj, parObj and mdlObj are returned. This
#' function is not intended for use by the user. It is recommended that 
#' the functions getDataObjects, getMOG, getParameterObjects and/or getModelObjects
#' are used instead.
#'
#' @usage .parseMDLFile("myMDLFile")
#'
#' @param f File path or URL of the .mdl file containing the objects.
#'
#' @param type String specifying the type of objects to extract. Possible values are
#' "parobj", "taskobj", "dataobj" and "mdlobj"
#'
#' @param name (Optional) Specifies the data object item, by name, to be 
#' retrieved by getDataObjects. If multiple data objects exist in the .mdl file 
#' then using the name argument helps users target a specific data object. 
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

  fromJSON(httpGET(cmd))[[1]]
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
      STRUCTURAL = as.list(dat$STRUCTURAL),
      PRIOR = as.list(dat$PRIOR),
      VARIABILITY = dat$VARIABILITY
    )
  
  return(res)

} 


.createDataObj <- function(dat){

    res <- new("dataObj",
        DATA_INPUT_VARIABLES = as.list(dat$DATA_INPUT_VARIABLES),
        SOURCE = as.list(dat$SOURCE),
        # TODO: TBC - These need to be populated
        RSCRIPT = list(),
        HEADER = list(),
        FILE = list(),
        DESIGN = list(),
        DATA_DERIVED_VARIABLES = character()
    )
    
    # Unquote the file name so that the file name within the R object is more easily manipulated
    res@SOURCE$file <- strip_quotes(res@SOURCE$file)
    # Similarly for the Ignore character
    res@SOURCE$ignore <- strip_quotes(res@SOURCE$ignore)

    res
}


.createMdlObj <- function(dat){
    
    res <- new("mdlObj",
        MODEL_INPUT_VARIABLES = list(dat$MODEL_INPUT_VARIABLES),
        STRUCTURAL_PARAMETERS = dat$STRUCTURAL_PARAMETERS,
        VARIABILITY_PARAMETERS = dat$VARIABILITY_PARAMETERS,
        GROUP_VARIABLES = dat$GROUP_VARIABLES,
        RANDOM_VARIABLE_DEFINITION = dat$RANDOM_VARIABLE_DEFINITION,
        INDIVIDUAL_VARIABLES = dat$INDIVIDUAL_VARIABLES,
        MODEL_PREDICTION = new("modPred",
            ODE = dat$MODEL_PREDICTION$ODE,
            LIBRARY = dat$MODEL_PREDICTION$LIBRARY,
            content = dat$MODEL_PREDICTION$content
        ),
        OBSERVATION = list(dat$OBSERVATION)
    )

}


.createTaskObj <- function(dat){
  res <- new("taskObj",
             content = dat$content
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
    
    parObjAsList <- list(
      STRUCTURAL = m@parObj@STRUCTURAL,
      PRIOR = m@parObj@PRIOR,
      VARIABILITY = m@parObj@VARIABILITY,
      identifier = "parobj"
    )
    
    dataObjAsList <- list(
        DATA_INPUT_VARIABLES = m@dataObj@DATA_INPUT_VARIABLES,
        SOURCE = m@dataObj@SOURCE,
        RSCRIPT = m@dataObj@RSCRIPT,
        HEADER = m@dataObj@HEADER,
        FILE = m@dataObj@FILE,
        DESIGN = m@dataObj@DESIGN,
        DATA_DERIVED_VARIABLES = m@dataObj@DATA_DERIVED_VARIABLES,
        identifier = "dataobj"
    )
    
    # Enclose the file name in double quotes ready for writing back to MDL
    dataObjAsList$SOURCE$file <- add_quotes(dataObjAsList$SOURCE$file)
    # Similarly for the Ignore character
    dataObjAsList$SOURCE$ignore <- add_quotes(dataObjAsList$SOURCE$ignore)
    
    mdlObjAsList <- list(
        MODEL_INPUT_VARIABLES = m@mdlObj@MODEL_INPUT_VARIABLES,
        STRUCTURAL_PARAMETERS = m@mdlObj@STRUCTURAL_PARAMETERS,
        VARIABILITY_PARAMETERS = m@mdlObj@VARIABILITY_PARAMETERS,
        GROUP_VARIABLES = m@mdlObj@GROUP_VARIABLES,
        RANDOM_VARIABLE_DEFINITION = m@mdlObj@RANDOM_VARIABLE_DEFINITION,
        INDIVIDUAL_VARIABLES = m@mdlObj@INDIVIDUAL_VARIABLES,
        MODEL_PREDICTION = list(
            ODE = m@mdlObj@MODEL_PREDICTION@ODE,
            LIBRARY = m@mdlObj@MODEL_PREDICTION@LIBRARY,
            content = m@mdlObj@MODEL_PREDICTION@content
        ),
        OBSERVATION = m@mdlObj@OBSERVATION,
        identifier = "mdlobj"
    )
    
    taskObjAsList <- list(
        content = m@taskObj@content,
        identifier = "taskobj"
    )
    
    json <- toJSON(list(list(
        mymodel_task = taskObjAsList,
        mymodel_par = parObjAsList,
        mymodel_mdl = mdlObjAsList,
        mymodel_dat = dataObjAsList
    )))
 
    .write.mclobj0(json, f, HOST, PORT)
})

.write.mclobj0 <- function(json, f, HOST='localhost', PORT='9010') {

    fullPath <- normalizePath(f, winslash="/", mustWork=FALSE)

    wreq <- URLencode(toJSON(list(
        fileName = fullPath,
        fileContent = json
    )))

    # Call parser and post the JSON data:
    cmd <- URLencode(paste0("http://", HOST, ":", PORT, "/writemdl"))

    postfield <- sprintf('%s%s','writeRequest=',wreq)
    
    res <- RCurl:::curlPerform(url = cmd, postfields = postfield )

}

