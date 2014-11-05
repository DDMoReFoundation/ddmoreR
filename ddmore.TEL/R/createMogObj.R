##############################################################
#' createMogObj
#'
#' Function to create an object of class mogObj from a
#' data object (dataObj), parameter object (parObj), model object (mdlObj)
#' and task object (taskObj). The mog name can be specified if desired.
#'
#' @usage createMogObj(myDataObj, myParObj, myMdlObj, myTaskObj)
#' @usage createMogObj(myDataObj, myParObj, myMdlObj, myTaskObj, mogName="myNewMog")
#'
#' @param dataObj An object of class dataObj
#' @param parObj An object of class parObj
#' @param mdlObj An object of class mdlObj
#' @param taskObj An object of class taskObj
#' @param mogName (Optional) The name to assign to the new mogObj
#'
#' @return An S4 Object of class "mogObj".
#'
#' @export
#' @docType methods
#' @rdname createMogObj
createMogObj <- function(dataObj, parObj, mdlObj, taskObj, mogName = "outputMog") {

	if (missing(mogName)) {
		new("mogObj", 
			dataObj = dataObj,
			parObj = parObj,
			mdlObj = mdlObj,
			taskObj = taskObj
		)
	} else {
		new("mogObj", 
			dataObj = dataObj,
			parObj = parObj,
			mdlObj = mdlObj,
			taskObj = taskObj,
			name = mogName
		)
	}
}
