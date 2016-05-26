################################################################################
#' getMDLObjects
#'
#' Retrieves all MCL Objects from a locally stored MDL file or from a URL and 
#' returns a list of objects of type "dataObj", "parObj", "mdlObj" and "taskObj". 
#' If a vector of object names are given in the "names" argument, then only these 
#' items are returned.
#'
#' @usage getMDLObjects(x, name)
#'
#' @param x File path or URL of the .mdl file containing the task object.
#'
#' @param fisServer FISServer instance.
#'
#' @return A list of objects of class "dataObj", "parObj", "taskObj" and "mdlObj". 
#'          If name is specified, only the single specified object is returned.
#'
#' @export
#' @docType methods
#' @rdname getMDLObjects-methods
#' @examples
#' \dontrun{
#'  Retrieve from the ddmore Library
#'    ThamDataObject <- getMDLObjects(file="http://ddmore.eu/model-repository/model/download/127.17?filename=2008ThamJCCR.mdl")
#'    }
#'    
#' ## Retrieve the all Objects from the .mdl file
#' myMDLObj <- getMDLObjects("UseCase2.mdl")
#' ## Retrieve the named Objects from the .mdl file
#' warfPKdat_obj <- getMDLObjects ("UseCase2.mdl",names=c("warfarin_PK_ANALYTIC_dat"))
#'
#' @include Classes.R FISServer.R
getMDLObjects <- function(x, name, fisServer = DDMORE.getServer()) { 
  
  if(!is.character(x)){
    stop("x must be a string containing either the file name or URL of the MDL file")}
  if(!missing(name)){
    if(!is.vector(name)){stop("argument 'name' must be a vector of strings")}
  }
  
  # Call parser and read in the JSON data
  raw <- .parseMDLFile0(x, fisServer=fisServer);
  
  allObjs <- list()
  sapply(MOG_OBJECT_TYPES, function(mog_object_type) {
	allObjs <<- c(allObjs, .extractTypeObjects(raw = raw, type = mog_object_type))
  })

  if (length(allObjs) == 0) {
	stop("No objects found in the parsed MDL file")
  }

  # Only return the single object with given name if name is specified
  if (!missing(name)) {
	
	# Extract names
	logi <- sapply(allObjs,
	  function(x) {
		x@name==name
	  }
	)
	subList <- allObjs[logi]
	
	if (length(subList) == 0) {
		stop(paste0("No object named \"", name, "\" found in the parsed MDL file"))
	}
	return(subList[[1]])
  }
  
  # Otherwise return a list of all the objects
  return(allObjs)
  
}

