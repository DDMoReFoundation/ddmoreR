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
# The Estimation Object Class (S4) 
# 
# Moved to Estimation-Class.R
#
##############################################################################


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


#
# Common class definitions shared between higher-level classes within the SO class structure.
#


#' The DataSet Object Class (S4) 
#'
#' An object to house all data associated with the data set
#' 
#' @slot description header information for dataset
#' @slot data matrix information with one column for each column in description
#' 
#' @name DataSet-class
#' @rdname DataSet-class
#' @exportClass DataSet
#' @aliases DataSet
#' @examples
#' dat <- new(Class = "DataSet", 
#'     description = data.frame(A = c("1", "unknown", "real"), B = c("2", "unknown", "real"), 
#'         row.names = c("columnNum", "columnType", "valueType"), stringsAsFactors = FALSE), 
#'     data = matrix(1:10, ncol = 2))
#' print(dat)
#' validObject(dat)

setClass(Class = "DataSet",
    	slots = c("description", "data"), 
    	prototype = list(description = data.frame(Col1 = c("1", "unknown", "real")), 
        		data = matrix(data = NA_real_, ncol = 1, nrow = 1)), 
    	validity = function(object) { 
        	stopifnot(ncol(object@description) == ncol(object@data))
        	stopifnot(all(rownames(object@description) == c("columnNum", "columnType", "valueType")))
        	return(TRUE) })

#' Instantiate a new object of class DataSet
#' 
#' Calls \code{new} trivially
#' @param data optional list containing "description" dataframe, "data" dataframe to populate with
#' @param dots arguments to \code{new}
#' @return object of class DataSet
#' @export

DataSet <- function(data = NULL, ...) {
    newObj <- new(Class = "DataSet", ...)
	
	if (!is.null(data)) {
		newObj@description <- data$description
		newObj@data <- data$data
	}
	
	newObj
}


#' Coerce DataSet object to data.frame
#' 
#' Make Data useable in R session
#' @param x DataSet object
#' @param dots
#' @return data.frame
#' @examples
#' dat <- new(Class = "DataSet", 
#'     description = data.frame(A = c("1", "unknown", "real"), B = c("2", "unknown", "real"), 
#'         row.names = c("columnNum", "columnType", "valueType"), stringsAsFactors = FALSE), 
#'     data = matrix(1:10, ncol = 2))
#' as.data.frame(x = dat)

setGeneric("as.data.frame")

setMethod(f = "as.data.frame", 
	signature = "DataSet", 
	definition = function(x, ...) {
		validObject(x)
		out <- as.data.frame(x@data, stringsAsFactors = FALSE)
		
		# update order
		index <- as.numeric(unlist(x@description["columnNum", , drop = FALSE]))
		out <- out[, index, drop = FALSE]
		colnames(out) <- colnames(x@description)
		for (i in seq_along(x@description)) {
    		# manually parse valueType
    		asType <- switch(x@description["valueType", i],
        			"real" = as.numeric, 
        			"Real" = as.numeric, 
        			"integer" = as.integer, 
        			"Int" = as.integer, 
        			"Char" = as.character, 
        			"String" = as.character, 
        			"Text" = as.character, {
            			warning("type not recognized in DataSet, treating as Real")
            			as.numeric
        			})
    		out[[i]] <- asType(out[[i]])
		}
		return(out)
	})

