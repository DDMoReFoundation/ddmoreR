# ====================== #
# Standard Output Object #
# ====================== #
# 
# Author: cmusselle

###############################################################################
#' The Standard Output Object Class (S4) 
#'
#' The initial design here is to have a differnt subclasss for each SO section: 
#' 
#' @include StandardOutputSubClasses.R 
#'
#'
#' @slot ToolSettings A flexible object to capture settings from executed tool. Kept as a list for now
#' @slot RawResults An object of S4 class "RawResults"
#' @slot Estimation An object of S4 class "Estimation" to house all data associated 
#' with population and individual estimates, precision, rediduals, predictions, 
#' liklihoods and output messages (and error messages) from individual modeling 
#' software. 
#' @slot ModelDiagnosticEvaluation An object of S4 class "ModelDiagnosticEvaluation" 
#' to house all data associated with model diagnostics and model comparisons.
#' @slot SimulationExploration An object of S4 class "SimulationExploration" 
#' to house all data associated with ... 
#' @slot OptimalDesign An object of S4 class "OptimalDesign" 
#' to house all data associated with ...  
#' 
#' @author cmusselle
#' @include StandardOutputSubClasses.R 
setClass("StandardOutputObject", 
  # Define the slots
  slots=c(
    ToolSettings = "list", 
    RawResults = "RawResults",
    Estimation = "Estimation",
 #   ModelDiagnosticEvaluation = "ModelDiagnosticEvaluation",
    SimulationExploration = "SimulationExploration",
    OptimalDesign = "OptimalDesign"
    ), 
  # Set Default Values to blank instances of the subclases
  prototype = list(
    ToolSettings = list(),
    RawResults = new("RawResults"),
  	Estimation = new("Estimation"),
  #	ModelDiagnosticEvaluation = new("ModelDiagnosticEvaluation"),
  	SimulationExploration = new("SimulationExploration"),
	OptimalDesign = new("OptimalDesign")
  	),
  # Validity Checking Function 
  validity = function(object) {
    stopifnot(class(object@ToolSettings)=="list")
    stopifnot(class(object@RawResults)=="RawResults")
	  stopifnot(class(object@Estimation)=="Estimation")
	 # stopifnot(class(object@ModelDiagnosticEvaluation)=="ModelDiagnosticEvaluation")
	  stopifnot(class(object@SimulationExploration)=="SimulationExploration")
	  stopifnot(class(object@OptimalDesign)=="OptimalDesign")
	  return(TRUE)
	}
)

#' createSOObject
#'
#' Create a new empty StandardOutput object 
#'
#' @return An S4 Object of class "StandardOutput".
#'
#' @export
#' @docType methods
#' @rdname createMogObj
createSOObject <- function(...) {

  SO <- new("StandardOutputObject")
  # if (missing()) { 
  # } 
  return(SO)
}


LoadRawResults <- function(RawResultsNode) {



}


# #
# #
# setMethod("LoadResults", signature(object = "Polygon"), function(object) {
#   object@sides
# })
# #
# #
# setGeneric("parseRawResults", function(x, name) { 
#   # create object in R from parser:
#   res <- .parseMDLFile(x, type="mdlobj", HOST=HOST, PORT=PORT)
  
#   return(res)
# })

# #' @rdname getModelObjects-methods
# #' @aliases getModelObjects,mogObj,mogObj-method
# setMethod("getModelObjects", signature=signature(x="mogObj"), function(x){
#    return(x@mdlObj)
# })



