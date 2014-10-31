# ====================================== #
# Sub classes for Standard Output Object #
# ====================================== # 
#
## Author: cmusselle

##############################################################################
#' The RawResults Object Class (S4) 
#' 
#' An object to house pointers to the raw data files.
#' 
#' @slot Files A list for now ...
#' 
#' @author cmusselle
#' @exportClass RawResults
setClass("RawResults", 
  # Define the slots
  slots=c(
    Files="list"
    ),
  # Set Default Values to blank lists with names in place
  prototype = list(
    Files = list()
    ),
  # Validity Checking Function 
  validity = function(object) {
  stopifnot(class(object@Files)=="list")
  return(TRUE)
  }
)


##############################################################################
#' The Estimation Object Class (S4) 
#'
#' An object to house all data associated with population and individual estimates, 
#' precision, rediduals, predictions, liklihoods and output messages (and error
#' messages) from individual modeling software. 
#'
#' As the input data is not well defined at current, the slots for most of this class
#' are currently defined as lists for flexibility
#' 
#' @slot PopulationEstimate A list for now ...
#' @slot PopulationEstimatePrecision A list for now ...
#' @slot IndividualEstimate A list for now ...
#' @slot IndividualEstimatePrecision A list for now ...
#' @slot ResidualsNPDE A list for now ...
#' @slot Predictions A list for now ...
#' @slot Liklihood A list for now ...
#' @slot OutputMessages A list for now ...
#' 
#' @author cmusselle
setClass("Estimation", 
  # Define the slots
  slots=c(
    PopulationEstimate="list",
    PopulationEstimatePrecision="list",
    IndividualEstimate="list", 
    IndividualEstimatePrecision="list", 
    ResidualsNPDE="list",
    Predictions="list",
    Likelihood="list",
    OutputMessages="list"
    ),
  # Set Default Values to blank lists with names in place
  prototype = list(
  	PopulationEstimate = list(MLE=NULL, Bayesian=NULL),
  	PopulationEstimatePrecision = list(FIM=NULL, InverseFIM=NULL, MLEStdErr=NULL, 
  		MLERelStdErr=NULL, BEStdDev=NULL, BootstrapStdDev=NULL, BEPostDist=NULL, 
  		BootstrapThetaHat=NULL, IntervalEstimates=NULL),
  	IndividualEstimate = list(MLEConditional=NULL, BEPostMean=NULL, 
  		BEPostMedian=NULL, BEPostMode=NULL, Shrinkage=NULL, MLE_ETAs=NULL, BE_ETAs=NULL,
  		ETAPostMean=NULL, ETAPostMedian=NULL, ETAPostMode=NULL,
  		RandomEffects=NULL),
  	IndividualEstimatePrecision = list(PostDist=NULL),
  	ResidualsNPDE = list(MLE_NPDE=NULL, BE_NPDE=NULL, PD=NULL, Population=NULL, 
  		Individual=NULL, Conditional=NULL),
	Predictions = list(Individual=NULL, Population=NULL), 
	Likelihood = list(LogLikelihood=NULL, Deviance=NULL, AIC=NULL, BIC=NULL),
	OutputMessages = list(TerminationMsg=NULL, Warnings=NULL, Errors=NULL, 
		RunTime=NULL, PharmMLOutputPath=NULL, MCMCChains=NULL, Iterations=NULL)
  	),
  # Validity Checking Function 
  validity = function(object) {
	stopifnot(class(object@PopulationEstimate)=="list")
    stopifnot(class(object@PopulationEstimatePrecision)=="list")
    stopifnot(class(object@IndividualEstimate)=="list") 
    stopifnot(class(object@IndividualEstimatePrecision)=="list") 
    stopifnot(class(object@ResidualsNPDE)=="list")
    stopifnot(class(object@Predictions)=="list")
    stopifnot(class(object@Likelihood)=="list")
    stopifnot(class(object@OutputMessages)=="list")
	return(TRUE)
	}
)

##############################################################################
#' The ModelDiagnosticEvaluation Object Class (S4) 
#' 
#' An object to house all data associated with model diagnostics and model comparisons
#' 
#' As the input data is not well defined at current, the slots for most of this class
#' are currently defined as lists for flexibility
#' 
#' @slot ModelDiagnostic A list for now ...
#' @slot ModelComparison A list for now ...
#' 
#' @author cmusselle
setClass("ModelDiagnosticEvaluation", 
  # Define the slots
  slots=c(
    ModelDiagnostic="list",
    ModelComparison="list"
    ),
  # Set Default Values to blank lists with names in place
  prototype = list(
  	ModelDiagnostic = list(Plots=NULL, Tests=NULL),
  	ModelComparison = list(Parentage=NULL, Comparisons=NULL, Tests=NULL)
  	),
  # Validity Checking Function 
  validity = function(object) {
	stopifnot(class(object@ModelDiagnostic)=="list")
    stopifnot(class(object@ModelComparison)=="list")
	return(TRUE)
	}
)


##############################################################################
#' The SimulationExploration Object Class (S4) 
#' 
#' An object to house all data associated with model simulation and exploration
#' 
#' As the input data is not well defined at current, the slots for most of this class
#' are currently defined as lists for flexibility
#' 
#' @slot Simulation A list for now ...
#' @slot Exploration A list for now ...
#' 
#' @author cmusselle
setClass("SimulationExploration", 
  # Define the slots
  slots=c(
    Simulation="list",
    Exploration="list"
    ),
  # Set Default Values to blank lists with names in place
  prototype = list(
  	Simulation = list(Population=NULL, Individual=NULL, Samples=NULL),
  	Exploration = list(Plots=NULL, Tables=NULL)
  	),
  # Validity Checking Function 
  validity = function(object) {
	stopifnot(class(object@Simulation)=="list")
    stopifnot(class(object@Exploration)=="list")
	return(TRUE)
	}
)

##############################################################################
#' The OptimalDesign Object Class (S4) 
#' 
#' An object to house all data associated with design evaluation and optimisation
#' 
#' As the input data is not well defined at current, the slots for most of this class
#' are currently defined as lists for flexibility
#' 
#' @slot ODE A list for now ... 
#' @slot AOD A list for now ... 
#' 
#' @author cmusselle
setClass("OptimalDesign", 
  # Define the slots
  slots=c(
    ODEEvaluation="list",
    ODEOptimisation="list",
    AODEvaluation="list",
    AODOptimisation="list"
    ),
  # Set Default Values to blank lists with names in place
  prototype = list(
  	ODEEvaluation = list(FIM=NULL, SE=NULL, RSE=NULL, Shrinkage=NULL, 
  		Criteria=NULL, Plots=NULL, Tables=NULL, Power=NULL, CI=NULL, Comparisons=NULL,
  		Window=NULL, RunTime=NULL, SimulatedData=NULL),
  	ODEOptimisation = list(FIM=NULL, OptimisedDesign=NULL, Improvement=NULL, 
  		Parameters=NULL, Shrinkage=NULL, Criteria=NULL, Plots=NULL, RunTime=NULL, 
  		Power=NULL, CI=NULL, Comparisons=NULL, Window=NULL, SimulatedData=NULL), 
  	AODEvaluation = list(), 
  	AODOptimisation = list()
  	),
  # Validity Checking Function 
  validity = function(object) {
	stopifnot(class(object@Simulation)=="list")
    stopifnot(class(object@Exploration)=="list")
	return(TRUE)
	}
)
