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
#' @param dataObj An object of class dataObj
#' @param parObj An object of class parObj
#' @param mdlObj An object of class mdlObj
#' @param taskObj An object of class taskObj
#' @param designObj An object of class designObj
#' @param mogName (Optional) The name to assign to the new mogObj
#'
#' @return An S4 Object of class "mogObj".
#'
#' @export
#' @docType methods
#' @rdname createMogObj
#' 
createMogObj <- function(dataObj=NULL, parObj, mdlObj, taskObj, designObj=NULL, mogName = "outputMog") {
	if (missing(mogName)) {
		new("mogObj", 
			dataObj = dataObj,
			parObj = parObj,
			mdlObj = mdlObj,
			taskObj = taskObj,
			designObj = designObj
		)
	} else {
		new("mogObj", 
			dataObj = dataObj,
			parObj = parObj,
			mdlObj = mdlObj,
			taskObj = taskObj,
			designObj = designObj,
			name = mogName
		)
	}
}
