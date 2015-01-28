################################################################################
#' read
#'
#' A "read" method for Data Objects and MOG objects  (S4 objects of class 
#' \code{dataObj} and \code{mogObj}. This uses information within the dataObj
#' object to define where and how to retrieve the data, attributes of dataset
#' columns (naming, type, units where applicable) and, if specified, how to
#' derive additional dataset columns.
#'
#' @param object Object of class \code{dataObj} or \code{mogObj}.
#' @param sourceDir If provided, the directory in which the data file(s) can be found.
#'        Defaults to the current directory.
#' @param deriveVariables (Boolean) Whether to apply any code specified within the
#'        DATA_DERIVED_VARIABLES block. Defaults to TRUE. Please note that the code
#'        provided in the block must be valid R syntax. It must also be written in
#'        a way that allows the code to be applied to each row of a data frame in
#'        turn. For example, if a row "WEIGHT" exists in the data frame,
#'        \code{WEIGHT>5} would be valid, whereas \code{data$WEIGHT>5} would not.
#' @param categoricalAsFactor (Boolean) Whether to convert any dataset variables
#'        defined as categorical in DATA_INPUT_VARIABLES, to factor. Defaults to TRUE.
#' @param recode (Boolean) Whether to apply any recode attributes defined within the
#'        DATA_INPUT_VARIABLES block. Defaults to TRUE.
#' @param asRaw (Boolean) If TRUE, equivalent to setting deriveVariables,
#'        categoricalAsFactor and recode all to FALSE.
#' @param ... Other named arguments to pass on to \link{read.csv}
#'
#' @return Returns a data frame or list of data frames containing the dataset(s)
#'         as described by the Data Object SOURCE, DATA_INPUT_VARIABLES and
#'         DATA_DERIVED_VARIABLES information.
#'
#' @export
#' @docType methods
#' @rdname read-methods
#'
#' @examples
#' mydata <- read(warfMOG@dataObj, sourceDir='C:\\SEE\\MDL_IDE\\workspace\\Product1\\models')
#' head(myData)
#'
#' @include telClasses.R
#' @include utils.R

setGeneric("read", function(object, sourceDir=getwd(), deriveVariables=TRUE, categoricalAsFactor=TRUE, recode=TRUE, asRaw=FALSE, ...) { 
  standardGeneric("read")
})


#' @rdname read-methods
#' @aliases read,dataObj,dataObj-method
setMethod("read", "dataObj", function(object, sourceDir=getwd(), deriveVariables=TRUE, categoricalAsFactor=TRUE, recode=TRUE, asRaw=FALSE, ...) {
  # if asRaw=TRUE, set the following arguments as FALSE
  if (asRaw) {
    deriveVariables <- FALSE
    categoricalAsFactor <- FALSE
    recode <- FALSE
  }
  
  res <- .importCSV(object, sourceDir=sourceDir, ...)
  
  # Overwrite the names from the csv with names from DATA_INPUT_VARIABLES
  names(res) <- names(object@DATA_INPUT_VARIABLES)
  
  # Apply code from DATA_DERIVED_VARIABLES:
  if (deriveVariables) {
    env1 <- new.env()
    codeString <- object@DATA_DERIVED_VARIABLES
    
    # Test the codeString on one row of the data to see if it is valid R code.
    # Only continue to apply to the rest of the data if no error is returned.
    test <- try(.rowEvaluate(res[1,], codeString), silent=TRUE)
    if (inherits(test, "try-error")) {
      warning("Unable to apply code from DATA_DERIVED_VARIABLES to the data. The code
        may not be valid R syntax, or it may not be written to function on a row-by-row
        basis. Please see ?read for further information. The data has been imported without
        applying the code.")
    } else {
    
      if (is.data.frame(res)) {
		# return directly if there is only one data file
		res <- .rowEvaluate(res, codeString)
      } else { # create a list of the results
		res <- lapply(res, .rowEvaluate, codeString)
      }
    }
  }
  
  # Apply code from DATA_INPUT_VARIABLES
  if (recode) {
    input <- object@DATA_INPUT_VARIABLES
    vars <- names(input)
    
    for (ii in vars) {
      type <- input[[ii]]$type
      if (type=="categorical" && categoricalAsFactor) {
		  try(res[, ii] <- as.factor(res[, ii]))
	  }
      if (type=="continuous") {
		  try(res[, ii] <- as.numeric(res[, ii]))
	  }
	  # TODO: add more options here 
    }
  }
  
  return(res)
})


#' @rdname read-methods
#' @aliases read,mogObj,mogObj-method
setMethod("read", "mogObj", function(object, deriveVariables=TRUE, categoricalAsFactor=TRUE, recode=TRUE, asRaw=FALSE, ...) {
  # extract dataObj
  ob <- object@dataObj
  # pass to method for dataObj
  read(ob,  deriveVariables=deriveVariables, categoricalAsFactor=categoricalAsFactor, recode=recode, asRaw=asRaw, ... )
})


################################################################################
#' .importCSV
#'
#' Imports the CSV files from a data object (class dataObj) and returns either
#' a single data frame, or a list of data frames if there is more than one.
#
#' @param dataObj object of class "dataObj"
#' @param sourceDir if specified, the directory in which the data files are held,
#'                  in case they are not in the current directory
#'
.importCSV <- function(dataObj, sourceDir=getwd(), ...) {

  # extract information from the "SOURCE" slot, and unlist to create a vector
  allInf <- unlist(dataObj@SOURCE)
  
  # pick out the csv files in the vector
  dataFiles <- allInf[grep("[[:print:]]+.csv", allInf)]
  if (length(dataFiles)==0) {
    stop("No csv data files were found in the data object")
  }

  # Prepend the source directory (the current directory if not specified) to the data files
  dataFiles <- file.path(sourceDir, dataFiles)
  
  if (length(dataFiles)==1) {
    # return directly if there is only one data file
    res <- read.csv(dataFiles, stringsAsFactors=FALSE, ...)
  } else { # create a list of the results
    res <- lapply(dataFiles, read.csv, stringsAsFactors=FALSE, ...)
  }

  return(res)

}
