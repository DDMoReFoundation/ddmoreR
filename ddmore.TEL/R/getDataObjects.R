##############################################################
#' getDataObjects
#'
#' Retrieves Data Object(s) (MCL object of class \code{dataObj}) from a locally stored 
#' MDL file or from a URL and returns an S4 object of class \code{dataObj}. 
#' Slots within this object are  \code{DATA_INPUT_VARIABLES}, \code{SOURCE}, 
#' \code{DATA_DERIVED_VARIABLES}
#' and \code{DESIGN} which are named lists of lists containing parsed information from 
#' the relevant MCL Data Object blocks and, optionally, the \code{RAW} vector of 
#' character strings corresponding to the lines of MCL code within the MCL Data Object.
#'
#' @usage getDataObjects(file, object, name, HOST="localhost", PORT="9010")
#'
#' @param file File path or URL of the .mdl file containing the task object
#' @param object MOG (object of class \code{mogObj}. This argument should not be used
#' in conjunction with the \code{file} argument
#'
#' @param name (Optional) Specifies the data object item, by name, to be 
#' retrieved by getDataObjects. If multiple data objects exist in the .mdl file 
#' then using the name argument helps users target a specific data object.
#' @param HOST hostname of the server running the FIS service, defaults to "localhost"
#' @param PORT port of the server running the FIS service, defaults to "9010"
#'
#' @details
#' getDataObjects only retrieves the MCL code, it does not read any data file 
#' identified within the \code{SOURCE} block. A \code{read} method should be applied to 
#' the resulting object which interprets the SOURCE, FILE and HEADER information 
#' and reads the specified data file into a data frame. See \code{read} method.
#'
#' @return An S4 Object of class \code{dataObj}.
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

setGeneric("getDataObjects", function(file, object, name, HOST="localhost", PORT="9010"){ 
	# create object in R from parser:
	if (!missing(name)) {
		res <- .parseMDLFile(file, name=name, type="dataobj", HOST=HOST, PORT=PORT)
	} else{
		res <- .parseMDLFile(file, type="dataobj", HOST=HOST, PORT=PORT)
	}
  	return(res)
  standardGeneric("getDataObjects")
})

#' @rdname getDataObjects-methods
#' @aliases getDataObjects,mogObj,mogObj-method
setMethod("getDataObjects", signature=signature(object="mogObj"), 
  function(file, object, name, HOST="localhost", PORT="9010") {
	if (!missing(file)) {
		warning("You have specified the file argument in addition to a mogObj. The file argument will be ignored.")
	}
    return(x@dataObj)
})


