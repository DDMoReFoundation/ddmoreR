##############################################################
#' createMogObj
#'
#' Function to create an object of class mogObj from a task object (taskObj), 
#' data object (dataObj), model object (mdlObj) and parameter object (parObj).
#'
#' @usage createMogObj(dataObj, taskObj, parObj, mdlObj)
#'
#' @param dataObj An object of class dataObj
#' @param taskObj An object of class taskObj
#' @param parObj An object of class parObj
#' @param mdlObj An object of class mdlObj
#'
#' @return An S4 Object of class "mogObj".
#'
#' @export
#' @docType methods
#' @rdname createMogObj
createMogObj <- function(dataObj, taskObj, parObj, mdlObj){

  res <- new("mogObj", 
    dataObj = dataObj,
    taskObj = taskObj,
    parObj = parObj,
    mdlObj = mdlObj
  )
  return(res)
}