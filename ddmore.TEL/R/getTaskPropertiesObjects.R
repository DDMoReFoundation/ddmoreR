################################################################################
#' getTaskPropertiesObjects
#'
#' Parses the specified MDL file, extracting the \code{taskobj} MCL Task Properties
#' Object top-level blocks and converting them to S4 objects of class
#' \code{\linkS4class{taskObj}}.
#' 
#' Slots within this object contain the parsed information from the corresponding
#' MCL Task Properties Object sub-blocks; see \linkS4class{taskObj}.
#'
#' @param file File path to the .mdl file containing the task properties object(s).
#' @param object TODO Not currently used.
#' @param name (Optional) Specifies the task properties object item, by name, to be
#'        retrieved by getTaskPropertiesObjects. If multiple task properties objects
#'        exist in the .mdl file then using the name argument allows the user to target
#'        a specific task properties object.
#' @param HOST (Optional) Hostname of the server running the FIS service; defaults
#'        to "localhost".
#' @param PORT (Optional) Port of the server running the FIS service, defaults to 9010.
#' @return List of S4 Objects of class \code{taskObj}.
#' 
#' @examples
#' # Retrieve all task properties objects
#' taskPropsList <- getTaskPropertiesObjects('Warfarin-ODE-latest.mdl')
#' # Retrieve a known task properties object by name
#' warfTaskProps <- getTaskPropertiesObjects('Warfarin-ODE-latest.mdl', name='warfarin_PK_ODE_task')[[1]]
#'
#' @export
#' @docType methods
#' @rdname getTaskPropertiesObjects-methods
#'
#' @include telClasses.R

setGeneric("getTaskPropertiesObjects", function(file, object, name, HOST='localhost', PORT='9010') { 
  # create object in R from parser:
  if (!missing(name)) {
	  res <- .parseMDLFile(file, name=name, type="taskobj", HOST=HOST, PORT=PORT)
  } else{
	  res <- .parseMDLFile(file, type="taskobj", HOST=HOST, PORT=PORT)
  }
  return(res)
})

#' @rdname getTaskPropertiesObjects-methods
#' @aliases getTaskPropertiesObjects,mogObj,mogObj-method
setMethod("getTaskPropertiesObjects", signature=signature(object="mogObj"),
  function(file, object, name, HOST="localhost", PORT="9010") {
    return(x@taskObj)
})


