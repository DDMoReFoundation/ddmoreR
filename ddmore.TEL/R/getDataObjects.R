################################################################################
#' getDataObjects
#'
#' Parses the specified MDL file, extracting the \code{dataObj} MCL Data Object
#' top-level blocks and converting them to S4 objects of class
#' \code{\linkS4class{dataObj}}.
#' 
#' Slots within this object contain the parsed information from the corresponding
#' MCL Data Object sub-blocks; see \linkS4class{dataObj}.
#'
#' @param file File path to the .mdl file containing the data object(s).
#' @param object TODO Not currently used.
#' @param name (Optional) Specifies the data object item, by name, to be retrieved
#'        by getDataObjects. If multiple data objects exist in the .mdl file then
#'        using the name argument allows the user to target a specific data object.
#' @param fisServer FISServer instance.
#' @return List of S4 Objects of class \code{dataObj}. If name is specified, only the 
#'          single specified object is returned.
#' 
#' @examples
#' # Retrieve all data objects
#' dataObjsList <- getDataObjects('Warfarin-ODE-latest.mdl')
#' # Retrieve a known data object by name
#' warfDataObj <- getDataObjects('Warfarin-ODE-latest.mdl', name='warfarin_PK_ODE_dat')[[1]]
#'
#' @details
#' \code{getDataObjects()} only parses the MCL code, it does not read any data file 
#' identified within the \code{SOURCE} block. A \link{read} method should be applied to 
#' the resulting object which interprets the \code{SOURCE} information and reads the
#' specified data file into a data frame.
#'
#' @export
#' @docType methods
#' @rdname getDataObjects-methods
#'
#' @include telClasses.R

setGeneric("getDataObjects", function(file, object, name, fisServer = TEL.getServer()) { 
	# create object in R from parser:
	if (!missing(name)) {
		res <- .parseMDLFile(file, name=name, type="dataObj", fisServer = fisServer)
	} else{
		res <- .parseMDLFile(file, type="dataObj", fisServer = fisServer)
	}
  	return(res)
  standardGeneric("getDataObjects")
})

#' @rdname getDataObjects-methods
#' @aliases getDataObjects,mogObj,mogObj-method
setMethod("getDataObjects", signature=signature(object="mogObj"), 
  function(file, object, name, fisServer = TEL.getServer()) {
	if (!missing(file)) {
		warning("You have specified the file argument in addition to a mogObj. The file argument will be ignored.")
	}
    return(x@dataObj)
})


