##############################################################
#' getParameterObjects
#'
#' Retrieves Parameter Object(s) (MCL object of type "parObj") from a locally 
#' stored MDL file, from a URL or an object of class "mogObj" and returns an S4 object of class "parObj". 
#' Slots within this object are  "STRUCTURAL" and "VARIABILITY" which are named 
#' lists of lists containing parsed information from the relevant MCL Parameter 
#' Object blocks, and, optionally, "RAW" – a vector of character strings 
#' corresponding to the lines of MCL code within the MCL Parameter Object.
#' The user should be able to specify a named parameter object from the MDL file 
#' as an argument, so that the nominated parameter objects can be returned.
#'
#' @usage getParameterObjects(file, name)
#'
#' @param x File path, URL of the .mdl file containing the task object or a MOG (object of class "mogObj".
#'
#' @param name (Optional) Specifies the parameter object item, by name, to be 
#' retrieved by getParameterObjects. If multiple parameter objects exist in the 
#' .mdl file then using the name argument helps users target a specific parameter 
#' object.
#'
#' @param HOST hostname of the server running the FIS service, defaults to localhost
#' @param PORT port of the server running the FIS service, defaults to 9010 
#'
#' @return An S4 Object of class "parObj".
#'
#' @export
#' @docType methods
#' @rdname getParameterObjects-methods
#'
#' @examples
#' ## Retrieve from the DDMoRe Library
#' ThamParObject <- getParameterObjects(file="http://ddmore.eu/model-repository/model/download/127.17?filename=2008ThamJCCR.mdl")
#' 
#' ## retrieve from a local .mdl file
#' ThamParObject <- getParameterObjects("2008ThamJCCR.mdl") 
#' 
#' ## retrieve a named item from a local .mdl file
#' tumourSizeParObject<-getParameterObjects("2008ThamJCCR.mdl",
#'   				name=" tumour_size_par")
#' 
#' ThamParObject[[1]] ## first parameter object within ThamParObject
#' ## Using parameter object name from MCL code
#' tumourSizeParObject<-ThamParObject$tumour_size_par
#' 
#' ## Change the initial values for structural parameters
#' ## Uses the “update” method
#' ThamParObject2<-update(ThamParObject@STRUCTURAL,
#' 				what=”value”,
#' 				with=list(POP_SIZE0=7, 
#' 					    POP_TOVER=15, 
#' 					    POP_AE50=10000, 
#' 					    POP_TEQ=5)
#' 				)
#' 
#'
#' @include telClasses.R

setGeneric("getParameterObjects", function(file, object, name, HOST='localhost', PORT='9010') {
	# create object in R from parser:
	if (!missing(name)) {
		res <- .parseMDLFile(file, name=name, type="parobj", HOST=HOST, PORT=PORT)
	} else{
		res <- .parseMDLFile(file, type="parobj", HOST=HOST, PORT=PORT)
	}
	return(res)
})

#' @rdname getDataObjects-methods
#' @aliases getParameterObjects,mogObj,mogObj-method
setMethod("getParameterObjects", signature=signature(object="mogObj"),
  function(file, object, name, HOST="localhost", PORT="9010") {
    return(x@parObj)
})


