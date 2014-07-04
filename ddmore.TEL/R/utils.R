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
#' @param x File path or URL of the .mdl file containing the objects.
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

.callParser <- function(x, name, type, HOST='localhost', PORT='9010') {

  if(!type%in%c("parobj", "taskobj", "dataobj", "mdlobj")){
    stop("Type specified is not one of 'parobj', 'taskobj', 'dataobj' or 'mdlobj'")
  }
  
  # Call parser and read in the JSON data
  raw <- .callParser0(x, HOST, PORT);

  if (!missing(name)) {
  
    .extractNamedObject(raw, name)
    return(res)
    
  } else {
  
    res <- .extractTypeObject(raw, type)
    return(res)
  
  }
  
}

.callParser0 <- function(x, HOST='localhost', PORT='9010') {

  # Call parser and read in the JSON data:
  cmd <- URLencode(paste0("http://", HOST, ":", PORT, "/readmdl?fileName=", normalizePath(x, winslash="/")))

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
        # TODO: TBC - These need to be populated
        MODEL_INPUT_VARIABLES = list(),
        STRUCTURAL_PARAMETERS = character(),
        VARIABILITY_PARAMETERS = character(),
        GROUP_VARIABLES = character(),
        RANDOM_VARIABLE_DEFINITION = list(),
        INDIVIDUAL_VARIABLES = character(),
        MODEL_PREDICTION = new("modPred",
            ODE = character(),
            LIBRARY = character()
        ),
        OBSERVATION = list()
    )

}


.createTaskObj <- function(){error("No functionality has been implemented to extract task objects")}
  
  
  


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

