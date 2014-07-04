##############################################################
#' getDataObjects
#'
#' Retrieves Data Object(s) (MCL object of class "dataObj") from a locally stored 
#' MDL file or from a URL and returns an S4 object of class "dataObj". 
#' Slots within this object are  "DATA_INPUT_VARIABLES", "SOURCE", "DATA_DERIVED_VARIABLES" 
#' and "DESIGN" which are named lists of lists containing parsed information from 
#' the relevant MCL Data Object blocks and, optionally, the "RAW" vector of 
#' character strings corresponding to the lines of MCL code within the MCL Data Object.
#'
#' @usage getDataObjects(file, name)
#'
#' @param x File path, URL of the .mdl file containing the task object or a MOG (object of class "mogObj".
#'
#' @param name (Optional) Specifies the data object item, by name, to be 
#' retrieved by getDataObjects. If multiple data objects exist in the .mdl file 
#' then using the name argument helps users target a specific data object. 
#'
#' @details
#' getDataObjects only retrieves the MCL code, it does not read any data file 
#' identified within the "SOURCE" block. A "read" method should be applied to 
#' the resulting object which interprets the SOURCE, FILE and HEADER information 
#' and reads the specified data file into a data frame.
#'
#' @return An S4 Object of class "dataObj".
#'
#' @export
#' @docType methods
#' @rdname getDataObjects-methods
#'
#' @examples
#' ## Retrieve from the DDMoRe Library
#' ThamDataObject <- getDataObjects(
#'   file="http://ddmore.eu/model-repository/model/download/127.17?filename=2008ThamJCCR.mdl")
#'
#' ## Retrieve from a local file
#' ThamDataObject <- getDataObjects("2008ThamJCCR.mdl") 
#'
#' ## Retrieve named data object from a local file
#' tumourSizeDataObject <- getDataObjects("2008ThamJCCR.mdl",
#'  				name="tumour_size_dat")
#'
#' ThamDataObject[[1]] ## first data object within ThamDataObject
#'
#' ## Using data object name from MCL code
#' tumourSizeDataObject<-ThamDataObject$tumour_size_dat
#'
#' @include telClasses.R

setGeneric("getDataObjects", function(x, name){ 
  # create object in R from parser:
  res <- .callParser(x, type="dataobj")
  return(res)
  standardGeneric("getDataObjects")
})

#' @rdname getDataObjects-methods
#' @aliases getDataObjects,mogObj,mogObj-method
setMethod("getDataObjects", signature=signature(x="mogObj"), function(x){
   return(x@dataObj)
})






