##############################################################
#' .callParser
#'
#' Calls to the MDL parser, providing either a file path or URL, and returns
#' a list of all objects in the MDL file. For data objects, parameter objects and
#' model objects, R objects of class dataObj, parObj and modObj are returned. This
#' function is not intended for use by the user. It is recommended that 
#' the functions getDataObjects, getMOG, getParameterObjects and/or getModelObjects
#' are used instead.
#'
#' @usage .callParser("myMDLFile")
#'
#' @param x File path or URL of the .mdl file containing the data object.
#'
#' @param name – (Optional) Specifies the data object item, by name, to be 
#' retrieved by getDataObjects. If multiple data objects exist in the .mdl file 
#' then using the name argument helps users target a specific data object. 
#'
#' @return A list of objects which are contained in the MDL file or URL.
#' @include telClasses.R

.callParser <- function(x, name){
  #command to call parser
  #par <-   #command to call parser. Returns list of all objects

  if(!missing(name)){ par <- par[name]}
  return(par)
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
    # Try to evaluate the code
    try(
      # Evaluate the code in the new environment
      eval(parse(text=codeString), envir=env1))
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



