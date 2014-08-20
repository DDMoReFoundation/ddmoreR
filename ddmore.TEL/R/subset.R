#' subset
#'
#' A subset method for objects of class \code{dataObj}. The filter method is 
#' applied to an object of class\code{dataObj} and is used to accept or omit rows of a 
#' dataset for subsequent tasks according to a indexing variable. This is a 
#' wrapper to the R subset function.
#'
#' @usage subset(dataObject, by, sourceDir=getwd(), deriveVariables=TRUE, 
#' categoricalAsFactor=TRUE, recode=TRUE, asRaw=FALSE, …) 
#'
#' @param dataObject  an object of class \code{dataObj}
#' @param by an indexing variable, Boolean vector of length nrow(data) 
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
#' @seealso R base function \code{subset}
#'
#' @return A data frame with \code{sum(by)} rows 
#'
#' @examples 
#' ## Create myData based on ThamDataObject
#' ThamDataObject <- getDataObjects("Tham2008.mdl", name="tumour_size_dat")
#' ## Create subsetting variable
#' onlyObservations <- createIndex(myData,criteria="AMT!=0")
#' myNewData <- subset(ThamDataObject, by=onlyObservations)
#'
#' ## Using information in the Task Properties object DATA block
#' warfData <- 	getDataObjects("warfarin_PK_CONC.mdl",name="warf_PK_CONC_dat")
#' warfTaskObject <- getTaskObjects("warfarin_PK_CONC.mdl")
#' ignore <- createIndex(warfData,criteria=warfTaskObject@DATA$IGNORE)
#' newWarfData <- subset(warfData, by = ignore)
#'
#' @include telClasses.R
#' @export
#' @docType methods
#' @rdname subset-methods
setGeneric("subset", 
  function(dataObject, by, sourceDir=getwd(), deriveVariables=TRUE, 
    categoricalAsFactor=TRUE, recode=TRUE, asRaw=FALSE, ...){
      standardGeneric("subset")
})
#' @rdname subset-methods
#' @aliases subset,dataObj,dataObj-method

setMethod("subset", signature=signature(dataObject="dataObj"), 
  function(dataObject, by, sourceDir=getwd(), deriveVariables=TRUE, 
    categoricalAsFactor=TRUE, recode=TRUE, asRaw=FALSE, ...){
    
  temp <- read(object=dataObject, sourceDir, deriveVariables, categoricalAsFactor, recode, asRaw)
 
  e <- substitute(by)
  x <- eval(e, temp, parent.frame())
  
  res <- base:::subset(temp, subset=x, ...)

  return(res)
  

})

