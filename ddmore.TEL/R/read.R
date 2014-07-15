##############################################################
#' read
#'
#' A "read" method for Data Objects and MOG objects  (S4 objects of class 
#' "dataObj" and "mogObj"). This uses information within the dataObj object to 
#' define where and how to retrieve the data, attributes of dataset columns 
#' (naming, type, units where applicable) and, if specified, how to derive 
#' additional dataset columns.
#'
#' @usage read(dataObj, deriveVariables=TRUE, categoricalAsFactor=TRUE, recode=TRUE, ...)
#' @usage read(mogObj, deriveVariables=TRUE, categoricalAsFactor=TRUE, recode=TRUE, ...)
#'
#' @param object Object of class "dataObj" or "mogObj"
#' @param sourceDir if provided, the directory in which the data file(s) can be found; defaults to the current directory.
#' @param deriveVariables (Boolean) apply any code specified within the DATA_DERIVED_VARIABLES block. Default=TRUE. Please
#'  note that the code provided in the block must be valid R syntax. It must also be written in a way that allows the code to 
#'  be applied to each row of a data frame in turn. For example, if a row "WEIGHT" exists in the data frame, "WEIGHT>5" would be
#' valid, whereas "data$WEIGHT>5" would not.
#' @param categoricalAsFactor (Boolean) convert any dataset variables defined as categorical to factor.
#' @param recode (Boolean) apply any recode attributes defined within the DATA_INPUT_VARIABLES block.
#' @param asRaw (Boolean) If TRUE, equivalent to setting deriveVariables, categoricalAsFactor and recode to FALSE.
#' @param ... Other named arguments to pass on to read.csv
#'
#' @return Returns a data frame or list of data frames containing the dataset(s) as described in the Data 
#' Object SOURCE and HEADER information.
#'
#' @export
#' @docType methods
#' @rdname read-methods
#'
#' @examples
#' myData <- read(myMOG$tumour_size_dat) 
#' head(myData)
#'
#' @include telClasses.R
#' @include utils.R

setGeneric("read", function(object, sourceDir=getwd(), deriveVariables=TRUE, categoricalAsFactor=TRUE, recode=TRUE, asRaw=FALSE, ...) { 
  standardGeneric("read")
})

#' @rdname read-methods
#' @aliases read,dataObj,dataObj-method
setMethod("read", "dataObj", function(object, sourceDir=getwd(), deriveVariables=TRUE, categoricalAsFactor=TRUE, recode=TRUE, asRaw=FALSE, ...){
  # if asRaw=TRUE, set the following arguments as FALSE
  if(asRaw){
    deriveVariables <- FALSE
    categoricalAsFactors <- FALSE
    recode <- FALSE
  }
  
  res <- .importCSV(object, sourceDir=sourceDir, categoricalAsFactor, ...)
  
  # Overwrite the names from the csv with names from DATA_DERIVED_VARIABLES
  names(res) <- names(object@DATA_INPUT_VARIABLES)
  
  # Apply code from DATA_DERIVED_VARIABLES:
  if(deriveVariables){
    env1 <- new.env()
    codeString <- object@DATA_DERIVED_VARIABLES
    
    # Test the codeString on one row of the data to see if it is valid R code.
    # Only continue to apply to the rest of the data if no error is returned.
    test <- try(.rowEvaluate(res[1,], codeString), silent=TRUE)
    if(inherits(test, "try-error")){
      warning("Unable to apply code from DATA_DERIVED_VARIABLES to the data. The code
        may not be valid R syntax, or it may not be written to function on a row-by-row
        basis. Please see ?read for further information. The data has been imported without
        applying the code.")
    } else{
    
      if(is.data.frame(res)){
        # return directly if there is only one data file
          res <- .rowEvaluate(res, codeString)
      }else{ # create a list of the results
          res <- lapply(res, .rowEvaluate, codeString)
      }
    }
  }
  
  # Apply code from DATA_INPUT_VARIABLES
  if(recode){
    input <- object@DATA_INPUT_VARIABLES
    vars <- names(input)
    
    for(ii in vars){
      type <- input[[ii]]$type
      if(type=="categorical"){try(res[, ii] <-as.factor(res[, ii]))}
      if(type=="continuous"){try(res[, ii] <-as.numeric(res[, ii]))}
            #TODO: add more options here 
    }
  }
  
  return(res)
})


#' @rdname read-methods
#' @aliases read,mogObj,mogObj-method
setMethod("read", "mogObj", function(object, deriveVariables=TRUE, categoricalAsFactor=TRUE, recode=TRUE, asRaw=FALSE, ...){
  # extract dataObj
  ob <- object@dataObj
  # pass to method for dataObj
  read(ob,  deriveVariables=deriveVariables, categoricalAsFactor=categoricalAsFactor, 
    recode=recode, asRaw=asRaw, ... )
})

