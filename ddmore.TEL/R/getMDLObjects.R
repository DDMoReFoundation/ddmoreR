##############################################################
#' getMDLObjects
#'
#' Retrieves all MCL Objects from a locally stored MDL file or from a URL and 
#' returns a list of objects of type "dataObj", "parObj", "mdlObj" and "taskObj". 
#' If a vector of object names are given in the "names" argument, then only these 
#' items are returned.
#'
#' @usage getMDLObjects(x, type, name)
#'
#' @param x File path or URL of the .mdl file containing the task object.
#'
#' @return A list of objects of class "dataObj", "parObj", "taskObj" and "modObj".
#'
#' @export
#' @docType methods
#' @rdname getMDLObjects-methods
#' @examples
## Retrieve from the DDMoRe Library
#' ThamDataObject <- getMDLObjects(file="http://ddmore.eu/model-repository/model/download/127.17?filename=2008ThamJCCR.mdl")
#' ## Retrieve the all Objects from the .mdl file
#' ThamDataObject <- getMDLObjects("2008ThamJCCR.mdl", type="All")
#' ## Retrieve the named Objects from the .mdl file
#' ThamMDLObjects<- getMDLObjects ("2008ThamJCCR.mdl",
#'   				names=c("tumour_size_dat","tumour_size_par",
#' 					 "tumour_size_mdl", "tumour_size_task"))
#' ## Convert the retrieved items to be a Model Object Group (MOG)
#' ## Checks for one object of each type.
#' myThamMOG <- as.MOG(ThamMDLObjects)
#' ## Substitute a user-defined Design and simulation based Task Properties Object
#' ## TO DO: 
#' ## Define myDesignBlock and mySimulationTaskObject
#' myThamMOG@dataobj$DESIGN <- myDesignBlock
#' myThamMOG@taskobj <- mySimulationTaskObject
#'
#' @include telClasses.R
setGeneric("getMDLObjects", function(x, name){ 
  
  if(!is.character(x)){stop("x must be a string containing either the file name or URL of the MDL file")}
  if(!missing(name)){
    if(!is.vector(name)){stop("argument 'name' must be a vector of strings")}
  }
  
  dataList <- getDataObjects(x)
  modList <- getModelObjects(x)
  paramList <- getParameterObjects(x)
  taskList <- getTaskPropertiesObjects(x)
  
  # Combine all objects into one list
  res <- c(dataList, modList, paramList, taskList)
  
  # Only return the object with given names if names is specified
  if(!missing(name)){
    stopifnot(all(name %in% names(res)))
    res <- res[names(res)%in%name]
  }
  
  return(res)
  
  standardGeneric("getMDLObjects")
})






