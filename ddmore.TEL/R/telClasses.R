# 
# Author: khanley, mwise
################################################################################


#### Data object class


validity.dataObj <- function(object)
{
  stopifnot(is.list(object@SOURCE))
  stopifnot(is.list(object@DECLARED_VARIABLES))
  stopifnot(is.list(object@DATA_INPUT_VARIABLES))
  stopifnot(is.list(object@DATA_DERIVED_VARIABLES))
  stopifnot(is.character(object@TARGET_CODE))
  return(TRUE)
}

################################################################################
#' Data Object S4 Class Definition.
#' 
#' Objects of this class map to occurrences of the \code{dataobj} top-level block
#' in an MDL file. They are created by parsing an MDL file using
#' \link{getDataObjects} or \link{getMDLObjects}.
#' 
#' @slot SOURCE Named list of parameter values keyed by their parameter names.
#'       Example parameter names are \code{file}, \code{inputformat}, \code{ignore}.
#' @slot DECLARED_VARIABLES Named list of variables where the names are the variable
#' 		 names but there are no attributes of these variables to map to.
#' @slot DATA_INPUT_VARIABLES Named list of variable names mapping to lists
#'       of attribute values keyed by their names, for each variable.
#' @slot DATA_DERIVED_VARIABLES Named list of variable names mapping to lists
#'       of attribute values keyed by their names, for each variable.
#' @slot TARGET_CODE TODO TBC
#' @slot name The name assigned to the \code{dataobj} in the MDL file.
#' 
#' @note Wherever a list of attribute values is mentioned for a Slot, this can instead
#'       be a special case of a single key '.expr', which maps to a string and
#'       represents a variable being an expression in the MDL e.g. "logtWT = log(WT/70)"
#'       rather than a set of attributes e.g. "DV : {level=1, type=observation}".
#' 
#' @author khanley, mwise
setClass("dataObj", 
  slots = c(
    SOURCE = "list",
	DECLARED_VARIABLES = "list",
    DATA_INPUT_VARIABLES = "list",
    DATA_DERIVED_VARIABLES = "list",
	TARGET_CODE = "character",
	name = "character"
  ), 
  validity = validity.dataObj
)


##############################################################
#' is.dataObj
#'
#' Determines if an object is of class "dataObj"
#'
#' @usage is.dataObj(object)
#'
#' @return TRUE or FALSE 
is.dataObj <- function(obj){

  class(obj)=="dataObj"

}


#### Task object class


validity.taskObj <- function(object)
{
	stopifnot(is.character(object@ESTIMATE))
	stopifnot(is.character(object@SIMULATE))
	stopifnot(is.character(object@EVALUATE))
	stopifnot(is.character(object@OPTIMISE))
	stopifnot(is.character(object@DATA))
	stopifnot(is.character(object@MODEL))
	stopifnot(is.character(object@TARGET_CODE))
	return(TRUE)
}

################################################################################
#' Task Object S4 Class Definition.
#' 
#' Objects of this class map to occurrences of the \code{taskobj} top-level block
#' in an MDL file. They are created by parsing an MDL file using
#' \link{getTaskPropertiesObjects} or \link{getMDLObjects}.
#' 
#' @slot ESTIMATE Character vector content of this sub-block "as-is".
#' @slot SIMULATE Character vector content of this sub-block "as-is".
#' @slot EVALUATE Character vector content of this sub-block "as-is".
#' @slot OPTIMISE Character vector content of this sub-block "as-is".
#' @slot DATA Character vector content of this sub-block "as-is".
#' @slot MODEL Character vector content of this sub-block "as-is".
#' @slot TARGET_CODE TODO TBC
#' @slot name The name assigned to the \code{taskobj} in the MDL file.
#' 
#' @author khanley, mwise
setClass("taskObj", 
  slots = c(
	ESTIMATE = "character",
	SIMULATE = "character",
	EVALUATE = "character",
	OPTIMISE = "character",
	DATA = "character",
	MODEL = "character",
	TARGET_CODE = "character",
	name = "character"
  ),
  validity = validity.taskObj
)

##############################################################
#' is.taskObj
#'
#' Determines if an object is of class "taskObj"
#'
#' @usage is.taskObj(object)
#'
#' @return TRUE or FALSE 
is.taskObj <- function(obj){

  class(obj)=="taskObj"

}


#### Parameter object class


validity.parObj <- function(object)
{
  stopifnot(is.list(object@DECLARED_VARIABLES))
  stopifnot(is.list(object@STRUCTURAL))
  stopifnot(is.list(object@VARIABILITY))
  stopifnot(is.list(object@PRIOR_PARAMETERS))
  stopifnot(is.character(object@TARGET_CODE))
  return(TRUE)
}

################################################################################
#' Parameter Object S4 Class Definition.
#' 
#' Objects of this class map to occurrences of the \code{parobj} top-level block
#' in an MDL file. They are created by parsing an MDL file using
#' \link{getParameterObjects} or \link{getMDLObjects}.
#'
#' @slot DECLARED_VARIABLES Named list of variables where the names are the variable
#' 		 names but there are no attributes of these variables to map to.
#' @slot STRUCTURAL Named list of variable names mapping to lists of attribute
#'       values keyed by their names, for each variable.
#' @slot VARIABILITY Named list of variable names mapping to lists of attribute
#'       values keyed by their names, for each variable.
#' @slot PRIOR_PARAMETERS TODO TBC
#' @slot TARGET_CODE TODO TBC
#' @slot name The name assigned to the \code{parobj} in the MDL file.
#' 
#' @note Wherever a list of attribute values is mentioned for a Slot, this can instead
#'       be a special case of a single key '.expr', which maps to a string and
#'       represents a variable being an expression in the MDL e.g. "logtWT = log(WT/70)"
#'       rather than a set of attributes e.g. "DV : {level=1, type=observation}".
#' 
#' @author khanley, mwise
#
# NB: VARIABILITY slot description was originally the following. This functionality
# might be reinstated in a future Product.
#       A named list, comprising a mixture of zero or more occurrences
#       of any, some or all of the following 'types' of Variability element:
#       \itemize{
#         \item{Variable name mapping to a list of attribute values keyed by their names}
#         \item{Matrix block, keyed as \code{matrix_X} where X is an incrementing number,
#               that maps to a list comprising elements with names \code{name},
#               \code{type} and \code{content}}
#         \item{Same block, keyed as \code{same_Y} where Y is an incrementing number, that
#               maps to a list comprising elements with names \code{name} and \code{content}}
#         \item{Diag block, keyed as \code{diag_Z} where Z is an incrementing number, that
#               maps to a list comprising elements with names \code{name}, \code{type}
#               and \code{content}}
#       }
setClass("parObj", 
  slots = c(
	DECLARED_VARIABLES = "list",
  	STRUCTURAL = "list",
  	VARIABILITY = "list",
  	PRIOR_PARAMETERS = "list",
	TARGET_CODE = "character",
	name = "character"
  ),
  validity = validity.parObj
)

#' is.parObj
#'
#' Determines if an object is of class "parObj"
#'
#' @usage is.parObj(object)
#'
#' @return TRUE or FALSE 
is.parObj <- function(obj){

  class(obj)=="parObj"

}


#### Model object class


validity.mdlObj <- function(object)
{
	stopifnot(is.list(object@IDV))
	stopifnot(is.list(object@COVARIATES))
	stopifnot(is.list(object@VARIABILITY_LEVELS))
	stopifnot(is.list(object@STRUCTURAL_PARAMETERS))
	stopifnot(is.list(object@VARIABILITY_PARAMETERS))
	stopifnot(is.list(object@RANDOM_VARIABLE_DEFINITION))
	stopifnot(is.list(object@INDIVIDUAL_VARIABLES))
	stopifnot(is.list(object@MODEL_PREDICTION))
    stopifnot(is.list(object@OBSERVATION))
	stopifnot(is.list(object@GROUP_VARIABLES))
	stopifnot(is.list(object@MODEL_OUTPUT_VARIABLES))
	stopifnot(is.list(object@ESTIMATION))
	stopifnot(is.list(object@SIMULATION))
	stopifnot(is.character(object@TARGET_CODE))
  return(TRUE)
}

### Create mdlObj class:

################################################################################
#' Model Object S4 Class Definition.
#' 
#' Objects of this class map to occurrences of the \code{mdlobj} top-level block
#' in an MDL file. They are created by parsing an MDL file using
#' \link{getModelObjects} or \link{getMDLObjects}.
#' 
#' @slot IDV Named list of variables where the names are the variable names
#' 		 but there are no attributes of these variables to map to.
#' @slot COVARIATES Named list of variable names mapping to lists
#'       of attribute values keyed by their names, for each variable.
#' @slot VARIABILITY_LEVELS Named list of variable names mapping to lists
#'       of attribute values keyed by their names, for each variable.
#' @slot STRUCTURAL_PARAMETERS Named list of variable names mapping to lists
#'       of attribute values keyed by their names, for each variable.
#' @slot VARIABILITY_PARAMETERS Named list of variable names mapping to lists
#'       of attribute values keyed by their names, for each variable.
#' @slot RANDOM_VARIABLE_DEFINITION A named list, keyed by variable name,
#' 		 each of whose elements is itself a named list containing
#'       \itemize{
#'         \item{.random_var_attrs - maps to a list of attribute values keyed
#' 			     by their values, pertaining to the random variable}
#'         \item{.random_var_distribution - maps to a string defining the
#' 				 distribution type of the random variable, e.g. "Normal"}
#' 		 }
#' @slot INDIVIDUAL_VARIABLES Named list of variable names mapping to lists
#'       of attribute values keyed by their names, for each variable.
#' @slot MODEL_PREDICTION Named list, comprising some or all of the following:
#'       \itemize{
#'         \item{Variable names mapping to lists of attribute values keyed by
#' 	             their names, for each variable.}
#'         \item{DEQ sub-block, represented by key '.DEQ'. This maps to a named
#'               list of variable names mapping to lists of attribute values
#'               keyed by their names, for each variable.}
#'         \item{COMPARTMENT sub-block, represented by key '.COMPARTMENT'.
#'               This maps to a named list of variable names mapping to lists of
#'               attribute values keyed by their names, for each variable.}
#'       }
#' @slot OBSERVATION Named list of variable names mapping to lists
#'       of attribute values keyed by their names, for each variable.
#' @slot GROUP_VARIABLES Named list of variable names mapping to lists
#'       of attribute values keyed by their names, for each variable.
#' @slot MODEL_OUTPUT_VARIABLES Named list of variables where the names are the
#' 		 variable names but there are no attributes of these variables to map to.
#' @slot ESTIMATION TODO TBC
#' @slot SIMULATION TODO TBC
#' @slot TARGET_CODE TODO TBC
#' @slot name The name assigned to the \code{mdlobj} in the MDL file.
#' 
#' @note Wherever a list of attribute values is mentioned for a Slot, this can instead
#'       be a special case of a single key '.expr', which maps to a string and
#'       represents a variable being an expression in the MDL e.g. "logtWT = log(WT/70)"
#'       rather than a set of attributes e.g. "DV : {level=1, type=observation}".
#' 
#' @author khanley, mwise
setClass("mdlObj", 
  slots = c(
	IDV = "list",
	COVARIATES = "list",
	VARIABILITY_LEVELS = "list",
    STRUCTURAL_PARAMETERS = "list",
    VARIABILITY_PARAMETERS = "list",
    RANDOM_VARIABLE_DEFINITION ="list",
    INDIVIDUAL_VARIABLES = "list",
    MODEL_PREDICTION = "list",
    OBSERVATION = "list",
    GROUP_VARIABLES = "list",
	MODEL_OUTPUT_VARIABLES = "list",
	ESTIMATION = "list",
	SIMULATION = "list",
	TARGET_CODE = "character",
	name = "character"
  ),
  validity = validity.mdlObj
)

#' is.mdlObj
#'
#' Determines if an object is of class "mdlObj"
#'
#' @usage is.mdlObj(object)
#'
#' @return TRUE or FALSE 
is.mdlObj <- function(obj){

  class(obj)=="mdlObj"

}



#### MOG class


validity.mogObj<- function(object)
{
	stopifnot(validity.dataObj(object@dataObj))
	stopifnot(validity.parObj(object@parObj))
	stopifnot(validity.mdlObj(object@mdlObj))
	stopifnot(validity.taskObj(object@taskObj))
  return(TRUE)
}


### Create mogObj class:

################################################################################
#' MOG (Model Object Group) S4 Class Definition.
#'
#' 'Aggregator' class comprising exactly one occurrence of each of the following
#' types of object as parsed from an MDL file:
#' \itemize{
#'   \item{\link{dataObj}}
#'   \item{\link{parObj}}
#'   \item{\link{mdlObj}}
#'   \item{\link{taskObj}}
#' }
#' 
#' @slot dataObj Object of class \code{dataObj}.
#' @slot parObj Object of class \code{parObj}.
#' @slot mdlObj Object of class \link{mdlOb}.
#' @slot taskObj Object of class \link{taskObj}.
#' @slot name A name to be assigned to the MOG; used when writing back out to MDL.
#' 
#' @author khanley
setClass("mogObj", 
  slots = c(
	dataObj = "dataObj",
	parObj = "parObj",
	mdlObj = "mdlObj", 
	taskObj = "taskObj",
	name = "character"
  ),
  validity = validity.mogObj
)


#' is.mogObj
#'
#' Determines if an object is of class "mogObj"
#'
#' @usage is.mogObj(object)
#'
#' @return TRUE or FALSE 
is.mogObj <- function(obj){

  class(obj)=="mogObj"

}

#' as.mogObj
#'
#' Creates a mogObj from a list of dataObj, parObj, mdlObj and taskObj objects. Note
#' that only one of each type may be included, and all types need to be present.
#'
#' @usage as.mogObj(list)
#' @export
#' @return An object of class mogObj
as.mogObj <- function(list){

  classes <- sapply(list, function(x){class(x)})

  nDat <- sum(classes=="dataObj")
  nPar <- sum(classes=="parObj")
  nMdl <- sum(classes=="mdlObj")
  nTask <- sum(classes=="taskObj")
  
  if (nDat!=1 | nPar!=1 | nMdl!=1 | nTask!=1) {
	  stop("The list provided must contain exactly one of each type of object: dataObj, parObj, mdlObj and taskObj")
  }
  
  dat <- list[classes=="dataObj"][[1]]
  par <- list[classes=="parObj"][[1]]
  mdl <- list[classes=="mdlObj"][[1]]
  task <- list[classes=="taskObj"][[1]]
  
  
  res <- new("mogObj", 
    dataObj = dat,
    parObj = par,
    mdlObj = mdl, 
    taskObj = task 
  )
  
  return(res)

}

