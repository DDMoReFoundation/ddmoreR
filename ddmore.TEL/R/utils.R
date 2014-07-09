##############################################################
#' .callParser
#'
#' Calls to the MDL parser, providing either a file path or URL, and returns
#' a list of all objects in the MDL file. For data objects, parameter objects and
#' model objects, R objects of class dataObj, parObj and mdlObj are returned. This
#' function is not intended for use by the user. It is recommended that 
#' the functions getDataObjects, getMOG, getParameterObjects and/or getModelObjects
#' are used instead.
#'
#' @usage .callParser("myMDLFile")
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

.callParser <- function(f, name, type, HOST='localhost', PORT='9010') {

  if(!type%in%c("parobj", "taskobj", "dataobj", "mdlobj")){
    stop("Type specified is not one of 'parobj', 'taskobj', 'dataobj' or 'mdlobj'")
  }
  
  # Call parser and read in the JSON data
  raw <- .callParser0(f, HOST, PORT);

  if (!missing(name)) {
  
    .extractNamedObject(raw, name)
    return(res)
    
  } else {
  
    res <- .extractTypeObject(raw, type)
    return(res)
  
  }
  
}

.callParser0 <- function(f, HOST='localhost', PORT='9010') {

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
    dataobj = .extractDataObj(dat)
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


.createTaskObj <- function(){error("No functionality has been implemented to extract task objects")}



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
#' @usage .write.mclobj(myMogObj, 'C:/Users/username/mymodel')
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
    
    # TODO: Implement the Task object
    taskObjAsList <- list(
        #m@taskObj
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

    wreq <- toJSON(list(
        fileName = fullPath,
        fileContent = json
    ))


    # Call parser and post the JSON data:
    cmd <- URLencode(paste0("http://", HOST, ":", PORT, "/writemdl"))

    res <- RCurl:::curlPerform(url = cmd, postfields = sprintf('%s%s','writeRequest=',wreq))

}






##############################################################
#' .assignFun
#'
#' Assigns column x of a data frame into an environment
.assignFun <- function(x, dat, env){
  assign(x, dat[x], envir=env)
}


##############################################################
#' .rowEvaluate
#'
#' Evaluates a string containing R code on each row of a data frame and returns
#' an updated data frame
.rowEvaluate <- function(dat, codeString){
  
  # Create empty environment in which to evaluate codeString
  env1 <- new.env()
  
  # Loop along each row of the data frame
  for(ii in seq_along(dat[,1])){
    
    # Extract desired row and col names
    temp <- dat[ii,]
    
    nam <- names(temp)

    # Assign the values to the environment
    sapply(nam, .assignFun, dat=temp, env=env1)
      # Evaluate the code in the new environment
      eval(parse(text=codeString), envir=env1)
      # Bring back the updated values and put back into temp
      for(nn in objects(env1)){
        temp[nn] <- eval(parse(text=paste0("env1$", nn)))
      }
     
    # Overwrite the original row with the updated values 
    dat[ii,] <- temp
  }

  # Remove the environment
  rm(env1)
  
  return(dat)
}

##############################################################
#' .importCSV
#'
#' Imports the CSV files from a data object (class dataObj) and returns either
#' a single data frame, or a list of data frames if there is more than one.
.importCSV <- function(object, categoricalAsFactor, ...){

# extract information from the "SOURCE" slot, and unlist to create a vector
  allInf <- unlist(object@SOURCE)
  
  # pick out the csv files in the vector
  dataFiles <- allInf[grep("[[:print:]]+.csv", allInf)]
  if(length(dataFiles)==0){
    stop("No csv data files were found in the data object")
  }
  
  if(length(dataFiles)==1){
    # return directly if there is only one data file
    res <- read.csv(dataFiles, stringsAsFactors=categoricalAsFactor, ...)
  }else{ # create a list of the results
    res <- lapply(dataFiles, read.csv, stringsAsFactors=categoricalAsFactor, ...)
  }

  return(res)

}

