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
    DataFiles="list", 
    GraphicsFiles="list"
    ),
  # Set Default Values to blank lists with names in place
  prototype = list(
    DataFiles = list(), 
    GraphicsFiles = list()
    ),
  # Validity Checking Function 
  validity = function(object) {
  stopifnot(class(object@DataFiles)=="list")
  stopifnot(class(object@GraphicsFiles)=="list")
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
#' @slot PopulationEstimates A list for now ...
#' @slot PrecisionPopulationEstimates A list for now ...
#' @slot IndividualEstimates A list for now ...
#' @slot PrecisionIndividualEstimates A list for now ...
#' @slot ResidualsNPDE A list for now ...
#' @slot Predictions A list for now ...
#' @slot Likelihood A list for now ...
#' @slot SoftwareMessages A list for now ...
#' 
#' @author cmusselle
setClass("Estimation", 
  # Define the slots
  slots=c(
    PopulationEstimates="list",
    PrecisionPopulationEstimates="list",
    IndividualEstimates="list", 
    PrecisionIndividualEstimates="list", 
    Residuals="list",
    Predictions="list",
    Likelihood="list",
    SoftwareMessages="list"
    ),
  # Set Default Values to blank lists with names in place
  prototype = list(
   PopulationEstimates = list(),
   PrecisionPopulationEstimates = list(),
   IndividualEstimates = list(),
   PrecisionIndividualEstimates = list(),
   Residuals = list(),
   Predictions = list(),
   Likelihood = list()
   ),
#  prototype = list(
#  	PopulationEstimates = list(MLE=NULL, Bayesian=NULL),
#  	PrecisionPopulationEstimates = list(MLE=NULL, Bayesian=NULL),
#  	IndividualEstimates = list(Estimates=NULL, EtaShrinkage=NULL, RandomEffects=NULL),
#  	PrecisionIndividualEstimates = list(PostDist=NULL),
#  	Residuals = list(MLE_NPDE=NULL, BE_NPDE=NULL, PD=NULL, Population=NULL, 
#  		Individual=NULL, Conditional=NULL),
#	Predictions = list(),
#	Likelihood = list(LogLikelihood=NULL, Deviance=NULL, AIC=NULL, BIC=NULL),
#	SoftwareMessages = list(TerminationMsg=NULL, Warnings=NULL, Errors=NULL, 
#		RunTime=NULL, PharmMLOutputPath=NULL, MCMCChains=NULL, Iterations=NULL)
#  	),
  # Validity Checking Function 
  validity = function(object) {
	stopifnot(class(object@PopulationEstimates)=="list")
    stopifnot(class(object@PrecisionPopulationEstimates)=="list")
    stopifnot(class(object@IndividualEstimates)=="list") 
    stopifnot(class(object@PrecisionIndividualEstimates)=="list") 
    stopifnot(class(object@Residuals)=="list")
    stopifnot(class(object@Predictions)=="list")
    stopifnot(class(object@Likelihood)=="list")
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
  	ModelDiagnostic = list(),
  	ModelComparison = list()
  	),
#  prototype = list(
#    ModelDiagnostic = list(Plots=NULL, Tests=NULL),
#    ModelComparison = list(Parentage=NULL, Comparisons=NULL, Tests=NULL)
#    ),
  # Validity Checking Function 
  validity = function(object) {
	stopifnot(class(object@ModelDiagnostic)=="list")
    stopifnot(class(object@ModelComparison)=="list")
	return(TRUE)
	}
)


##############################################################################
#' The Simulation Object Class (S4) 
#' 
#' An object to house all data associated with model simulation runs.
#'  
#' @slot Description A character vector describing the simulation experiment
#' @slot OriginalData Details and reference to the original data file.
#' @slot Replicates A list of ReplicateBlock S4 classes that contain details of 
#'   each individual simulation run.
#' 
#' @author cmusselle
setClass("Simulation", 
  # Define the slots
  slots=c(
    Description="character",
    OriginalDataset="list", 
    Replicates = "list"
    ),
  # Set Default Values to blank lists with names in place
  prototype = list(
    Description=vector(mode="character"),
    OriginalDataset=list(), 
    Replicates = list()
),
 # prototype = list(
 #   Simulation = list(Population=NULL, Individual=NULL, Samples=NULL),
 #   Exploration = list(Plots=NULL, Tables=NULL)
 #   ),
  # Validity Checking Function 
  validity = function(object) {
	  stopifnot(class(object@Description) == "character")
    stopifnot(class(object@OriginalDataset) == "list")
    stopifnot(class(object@Replicates) == "list")
	return(TRUE)
	}
)

##############################################################################
#' The ReplicateBlock Object Class (S4) 
#' 
#' An object to house all data associated with a single simulation run
#' 
#' @slot SimulatedProfiles A list containing the data as a dataframe and a description header 
#' @slot IndivParameters A list containing the data as a dataframe and a description header
#' @slot Covariates A list containing the data as a dataframe and a description header
#' @slot PopulationParameters A list containing the data as a dataframe and a description header
#' @slot Dosing A list containing the data as a dataframe and a description header
#' @slot RawResultsFile A list containing the data as a dataframe and a description header
#' 
#' @author cmusselle
setClass("ReplicateBlock", 
  # Define the slots
  slots=c(
    SimulatedProfiles="list",
    IndivParameters="list", 
    Covariates = "list",
    PopulationParameters = "list",
    Dosing = "list",
    RawResultsFile = "list"
    ),
  # Set Default Values to blank lists with names in place
  prototype = list(
    SimulatedProfiles = list(),
    IndivParameters = list(), 
    Covariates = list(),
    PopulationParameters = list(),
    Dosing = list(),
    RawResultsFile = list()
),
 # prototype = list(
 #   Simulation = list(Population=NULL, Individual=NULL, Samples=NULL),
 #   Exploration = list(Plots=NULL, Tables=NULL)
 #   ),
  # Validity Checking Function 
  validity = function(object) {
    stopifnot(class(object@SimulatedProfiles) == "list")
    stopifnot(class(object@IndivParameters) == "list")
    stopifnot(class(object@Covariates) == "list")
    stopifnot(class(object@PopulationParameters) == "list")
    stopifnot(class(object@Dosing) == "list")
    stopifnot(class(object@RawResultsFile) == "list")
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
  	ODEEvaluation = list(),
  	ODEOptimisation = list(), 
  	AODEvaluation = list(), 
  	AODOptimisation = list()
  	),
# prototype = list(
#    ODEEvaluation = list(FIM=NULL, SE=NULL, RSE=NULL, Shrinkage=NULL, 
#        Criteria=NULL, Plots=NULL, Tables=NULL, Power=NULL, CI=NULL, Comparisons=NULL,
#        Window=NULL, RunTime=NULL, SimulatedData=NULL),
#    ODEOptimisation = list(FIM=NULL, OptimisedDesign=NULL, Improvement=NULL, 
#        Parameters=NULL, Shrinkage=NULL, Criteria=NULL, Plots=NULL, RunTime=NULL, 
#        Power=NULL, CI=NULL, Comparisons=NULL, Window=NULL, SimulatedData=NULL), 
#    AODEvaluation = list(), 
#    AODOptimisation = list()
#    ),
  # Validity Checking Function 
  validity = function(object) {
	  stopifnot(class(object@ODEEvaluation)=="list")
    stopifnot(class(object@ODEOptimisation)=="list")
    stopifnot(class(object@AODEvaluation)=="list")
    stopifnot(class(object@AODOptimisation)=="list")
	return(TRUE)
	}
)
