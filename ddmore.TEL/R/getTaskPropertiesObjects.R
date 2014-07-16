##############################################################
#' getTaskPropertiesObjects
#'
#' Retrieves TaskProperties Object(s) (MCL object of type "taskObj") from a 
#' locally stored MDL file , URL r object of class "mogObj" and returns an S4 object of class 
#' "taskObj". 
#' The user should be able to specify a named parameter object from the MDL file 
#' as an argument, so that the nominated parameter objects can be returned.
#'
#' @usage getTaskPropertiesObjects(x, name)
#'
#' @param x File path, URL of the .mdl file containing the task object or a MOG (object of class "mogObj".
#'
#' @param HOST hostname of the server running the FIS service, defaults to localhost
#' @param PORT port of the server running the FIS service, defaults to 9010
#'
#' @return An S4 Object of class "taskObj".
#'
#' @export
#' @docType methods
#' @rdname getTaskPropertiesObjects-methods
#'
#' @examples
## Retrieve from DDMoRe repository
#' getTaskPropertiesObjects(file="http://ddmore.eu/model-repository/model/download/127.17?filename=2008ThamJCCR.mdl")
#' 
#' ## Retrieve from local .mdl file
#' ThamTaskObject<- getTaskPropertiesObjects ("2008ThamJCCR.mdl") 
#' 
#' ## Retrieve named object from local .mdl file
#' tumourSizeTaskObject<- getTaskPropertiesObjects ("2008ThamJCCR.mdl",
#'   				name=" tumour_size_task")
#' 
#' ThamTaskObject[[1]] ## first task object within ThamTaskObject
#' 
## Using task properties object name from MCL code
#' tumourSizeTaskObject<-ThamTaskObject$tumour_size_task
#' ## TODO
#' ## Add example of altering Task Object
#'
#' @include telClasses.R
setGeneric("getTaskPropertiesObjects", function(x, name, HOST='localhost', PORT='9010') { 
  # create object in R from parser:
  res <- .parseMDLFile(x, type="taskobj", HOST=HOST, PORT=PORT)
  
  return(res)
})

#' @rdname getModelObjects-methods
#' @aliases getModelObjects,mogObj,mogObj-method
setMethod("getTaskPropertiesObjects", signature=signature(x="mogObj"), function(x){
   return(x@taskObj)
})






