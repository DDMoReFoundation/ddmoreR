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
#' @slot OFMeasures A list for now ...
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
    OFMeasures="list",
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
   OFMeasures = list()
   ),
  # Validity Checking Function 
  validity = function(object) {
	stopifnot(class(object@PopulationEstimates)=="list")
    stopifnot(class(object@PrecisionPopulationEstimates)=="list")
    stopifnot(class(object@IndividualEstimates)=="list") 
    stopifnot(class(object@PrecisionIndividualEstimates)=="list") 
    stopifnot(class(object@Residuals)=="list")
    stopifnot(class(object@Predictions)=="list")
    stopifnot(class(object@OFMeasures)=="list")
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
setClass("ModelDiagnostic", 
  # Define the slots
  slots=c(
    DiagnosticPlotsStructuralModel="list",
    DiagnosticPlotsIndividualParams="list"
    ),
 # Set Default Values to blank lists with names in place
  prototype = list(
  	DiagnosticPlotsStructuralModel = list(),
  	DiagnosticPlotsIndividualParams = list()
  	),
  # Validity Checking Function 
  validity = function(object) {
	stopifnot(class(object@DiagnosticPlotsStructuralModel)=="list")
    stopifnot(class(object@DiagnosticPlotsIndividualParams)=="list")
	return(TRUE)
	}
)


##############################################################################
#' The Simulation Object Class (S4) 
#' 
#' An object to house all data associated with model simulation runs.
#'  
#' @slot OriginalData Details and reference to the original data file.
#' @slot SimulationBlock A list of SimulationBlock S4 classes that contain
#'       details of each individual simulation run.
#' 
#' @author cmusselle
setClass("Simulation", 
  # Define the slots
  slots=c(
    OriginalDataset="list", 
    SimulationBlock = "list"
    ),
  # Set Default Values to blank lists with names in place
  prototype = list(
    OriginalDataset=list(), 
    SimulationBlock = list()
),
  # Validity Checking Function 
  validity = function(object) {
      stopifnot(class(object@OriginalDataset) == "list")
      stopifnot(class(object@SimulationBlock) == "list")
	  return(TRUE)
	}
)

##############################################################################
#' The SimulationBlock Object Class (S4) 
#' 
#' An object to house all data associated with a single simulation run
#' 
#' @slot SimulatedProfiles A list containing the data as a dataframe and a description header 
#' @slot IndivParameters A list containing the data as a dataframe and a description header
#' @slot Covariates A list containing the data as a dataframe and a description header
#' @slot PopulationParameters A list containing the data as a dataframe and a description header
#' @slot Dosing A list containing the data as a dataframe and a description header
#' @slot RawResultsFile containing the path to the file containing the raw results
#' 
#' @author cmusselle
setClass("SimulationBlock", 
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
#' @slot OptimalDesignBlock A list of all OptimalDesignBlock elements from the SO
#' 
#' @author cmusselle
setClass("OptimalDesign", 
  # Define the slots
  slots=c(
    OptimalDesignBlock="list"
    ),
  # Set Default Values to blank lists with names in place
  prototype = list(
  	OptimalDesignBlock = list()
  	),
  # Validity Checking Function 
  validity = function(object) {
	  stopifnot(class(object@OptimalDesignBlock)=="list")
	return(TRUE)
	}
)


##############################################################################
#' The OptimalDesignBlock Object Class (S4) 
#' 
#' An object to house all data associated with a single simulation run
#' 
#' @slot SimulatedProfiles A list containing the data as a dataframe and a description header 
#' @slot IndivParameters A list containing the data as a dataframe and a description header
#' @slot Covariates A list containing the data as a dataframe and a description header
#' @slot PopulationParameters A list containing the data as a dataframe and a description header
#' @slot Dosing A list containing the data as a dataframe and a description header
#' @slot RawResultsFile containing the path to the file containing the raw results
#' 
#' @author cmusselle


# XXXX: Still working on this, may not work

# setClass("OptimalDesignBlock", 
#   # Define the slots
#   slots=c(
#     FIM="matrix",
#     CovarianceMatrix="matrix", 
#     Parameters = "list",
#     Criteria = "list",
#     Tests = "list",
#     SimulatedDatasets = "list",
#     Design = "list"
#     ),
#   # Set Default Values to blank lists with names in place
#   prototype = list(
#     FIM = "matrix",
#     CovarianceMatrix = "matrix", 
#     Parameters = "list",
#     Criteria = "list",
#     Tests = "list",
#     SimulatedDatasets = "list",
#     Design = "list"
# ),
#   # Validity Checking Function 
#   validity = function(object) {
#     stopifnot(class(object@SimulatedProfiles) == "list")
#     stopifnot(class(object@IndivParameters) == "list")
#     stopifnot(class(object@Covariates) == "list")
#     stopifnot(class(object@PopulationParameters) == "list")
#     stopifnot(class(object@Dosing) == "list")
#     stopifnot(class(object@RawResultsFile) == "list")
#     return(TRUE)
#   }
# )
