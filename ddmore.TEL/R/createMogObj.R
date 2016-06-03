##############################################################
#' createMogObj
#'
#' Function to create an object of class mogObj from a
#' data object (dataObj), parameter object (parObj), model object (mdlObj)
#' and task object (taskObj). The mog name can be specified if desired.
#'
#' @usage createMogObj(dataObj=myDataObj, parObj=myParObj, mdlObj=myMdlObj, taskObj=myTaskObj)
#' @usage createMogObj(dataObj=myDataObj, parObj=myParObj, mdlObj=myMdlObj, taskObj=myTaskObj, mogName="myNewMog")
#'
#' @param dataObj (Optional) An object of class dataObj
#' @param parObj An object of class parObj
#' @param mdlObj An object of class mdlObj
#' @param taskObj An object of class taskObj
#' @param priorObj (Optional) An object of class priorObj
#' @param designObj (Optional) An object of class designObj
#' @param mogName (Optional) The name to assign to the new mogObj
#'
#' @return An S4 Object of class "mogObj".
#'
#' @export
#' @docType methods
#' @rdname createMogObj
#' 
createMogObj <- function(dataObj = NULL, parObj, mdlObj, taskObj, priorObj = NULL, 
    designObj = NULL, mogName = "outputMog") {
	if (missing(mogName)) {
		new("mogObj", 
			dataObj = dataObj,
			parObj = parObj,
			mdlObj = mdlObj,
			taskObj = taskObj,
            priorObj = priorObj,
			designObj = designObj
		)
	} else {
		new("mogObj", 
			dataObj = dataObj,
			parObj = parObj,
			mdlObj = mdlObj,
			taskObj = taskObj,
            priorObj = priorObj,
			designObj = designObj,
			name = mogName
		)
	}
}
