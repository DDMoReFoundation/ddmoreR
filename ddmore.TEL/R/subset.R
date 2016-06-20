################################################################################
#' @title subset a dataObj
#'
#' @description A subset method for objects of class \code{dataObj}. The filter 
#' method is applied to an object of class\code{dataObj} and is used to accept 
#' or omit rows of a dataset for subsequent tasks according to a indexing variable. 
#' This is a wrapper to the base R \code{subset} function.
#'
#' @usage subset(x, subset, sourceDir = getwd(), deriveVariables = TRUE, 
#' categoricalAsFactor = TRUE, recode = TRUE, asRaw = FALSE, ...) 
#'
#' @param x an object of class \code{dataObj}
#' @param subset an expression defining logical indexing of a variable from the dataset, 
#' or a Boolean vector of length nrow(data).
#' @param sourceDir if provided, the directory in which the data file(s) can be found; defaults to the current directory.
#' @param deriveVariables (Boolean) apply any code specified within the DATA_DERIVED_VARIABLES block. Default=TRUE. Please
#'  note that the code provided in the block must be valid R syntax. It must also be written in a way that allows the code to 
#'  be applied to each row of a data frame in turn. For example, if a row "WEIGHT" exists in the data frame, "WEIGHT>5" would be
#' valid, whereas \code{data$WEIGHT>5} would not.
#' @param categoricalAsFactor (Boolean) convert any dataset variables defined as categorical to factor.
#' @param recode (Boolean) apply any recode attributes defined within the DATA_INPUT_VARIABLES block.
#' @param asRaw (Boolean) If TRUE, equivalent to setting deriveVariables, categoricalAsFactor and recode to FALSE.
#' @param ... additional arguments to be passed to the R base subset function
#'
#' @seealso R base function \code{subset}, \code{\link{readDataObj}}
#'
#' @return A data frame with \code{sum(subset)} rows 
#'
#' @examples 
#' ## Create myData based on ThamDataObject
#' ThamDataObject <- getDataObjects("Tham2008.mdl", name="tumour_size_dat")
#' subData <- subset(x = ThamDataObject, subset = AMT!=0)
#'
#' ## Using information in the Task Properties object DATA block
#' warfData <- 	getDataObjects("warfarin_PK_CONC.mdl",name = "warf_PK_CONC_dat")
#' warfTaskObject <- getTaskObjects("warfarin_PK_CONC.mdl")
#' ignore <- createIndex(warfData, 
#'     criteria=slot(warfTaskObject, name = "DATA")$IGNORE)
#' newWarfData <- subset(warfData, subset = ignore)
#'
#' @include Classes.R
#' @export
#' @docType methods
#' @rdname subset-methods
setGeneric("subset", 
  function(x, subset, sourceDir = getwd(), deriveVariables = TRUE, 
    categoricalAsFactor = TRUE, recode = TRUE, asRaw = FALSE, ...) {
      standardGeneric("subset")
})

#' @rdname subset-methods
#' @aliases subset,mogObj,mogObj-method
setMethod("subset", signature = signature(x = "mogObj"), 
  function(x, subset, sourceDir = getwd(), deriveVariables = TRUE, 
    categoricalAsFactor = TRUE, recode = TRUE, asRaw = FALSE, ...){
    
  if (missing(subset)) { stop("subset is missing") }
  
  # Extract dataObj:
  x <- slot(object = x, name = "dataObj")
  
  # Then call the dataObj method
  subset(x = x, subset = subset, sourceDir = sourceDir, 
    deriveVariables = deriveVariables, categoricalAsFactor = categoricalAsFactor, 
    recode = recode, asRaw = asRaw, ... )
})


#' @rdname subset-methods
#' @aliases subset,dataObj,dataObj-method

setMethod("subset", signature = signature(x = "dataObj"), 
  function(x, subset, sourceDir = getwd(), deriveVariables = TRUE, 
    categoricalAsFactor = TRUE, recode = TRUE, asRaw = FALSE, ...) {
  
  if (missing(subset)) { stop("subset is missing") }
  
  dat <- readDataObj(object = x, sourceDir = sourceDir, 
    deriveVariables = deriveVariables, categoricalAsFactor = categoricalAsFactor, 
    recode = recode, asRaw = asRaw)
  
  ex <- substitute(subset)
  logi <- eval(expr = ex, envir = dat, enclos = parent.frame())
  
  if (length(logi) != nrow(dat)) { stop("length of subset must equal rows of data") }
  
  res <- base::subset.data.frame(x = dat, subset = logi, ...)
  
  return(res)
})

