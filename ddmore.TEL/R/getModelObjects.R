################################################################################
#' getModelObjects
#'
#' Parses the specified MDL file, extracting the \code{mdlobj} MCL Model Object
#' top-level blocks and converting them to S4 objects of class
#' \code{\linkS4class{mdlObj}}.
#' 
#' Slots within this object contain the parsed information from the corresponding
#' MCL Model Object sub-blocks; see \linkS4class{mdlObj}.
#'
#' @param file File path to the .mdl file containing the model object(s).
#' @param object TODO Not currently used.
#' @param name (Optional) Specifies the model object item, by name, to be retrieved
#'        by getModelObjects. If multiple model objects exist in the .mdl file then
#'        using the name argument allows the user to target a specific model object.
#' @param HOST (Optional) Hostname of the server running the FIS service; defaults
#'        to "localhost".
#' @param PORT (Optional) Port of the server running the FIS service, defaults to 9010.
#' @return List of S4 Objects of class \code{mdlObj}.
#' 
#' @examples
#' # Retrieve all model objects
#' mdlObjsList <- getModelObjects('Warfarin-ODE-latest.mdl')
#' # Retrieve a known model object by name
#' warfMdlObj <- getModelObjects('Warfarin-ODE-latest.mdl', name='warfarin_PK_ODE_mdl')[[1]]
#' 
#' @details Users are not generally expected to change the model object via TEL, except in
#'          very few cases e.g. stepwise covariate model building via a configuration file 
#'          (as in the "scm" method within Perl speaks NONMEM, PsN).
#'
#' @export
#' @docType methods
#' @rdname getModelObjects-methods
#'
#' @include telClasses.R

setGeneric("getModelObjects", function(file, object, name, HOST='localhost', PORT='9010') { 
	# create object in R from parser:
	if (!missing(name)) {
		res <- .parseMDLFile(file, name=name, type="mdlobj", HOST=HOST, PORT=PORT)
	} else{
		res <- .parseMDLFile(file, type="mdlobj", HOST=HOST, PORT=PORT)
	}
	return(res)
})

#' @rdname getModelObjects-methods
#' @aliases getModelObjects,mogObj,mogObj-method
setMethod("getModelObjects", signature=signature(object="mogObj"),
  function(file, object, name, HOST="localhost", PORT="9010") {
    return(x@mdlObj)
})


