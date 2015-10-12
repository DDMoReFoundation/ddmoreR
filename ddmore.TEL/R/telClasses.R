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
  return(TRUE)
}

################################################################################
#' Data Object S4 Class Definition.
#' 
#' Objects of this class map to occurrences of the \code{dataObj} top-level block
#' in an MDL file. They are created by parsing an MDL file using
#' \link{getDataObjects} or \link{getMDLObjects}.
#' 
#' @slot SOURCE Named list of sets of name-value-pair attributes, each set referencing
#'       a particular data file. Example attribute names are
#'       \code{file}, \code{inputformat}, \code{ignore}.
#' @slot DECLARED_VARIABLES List of sets of name-value=pair attributes, each set
#'       encoding the definition of a variable.
#' @slot DATA_INPUT_VARIABLES Named list of variable names mapping to sets of
#'       name-value-pair attributes of each variable.
#' @slot DATA_DERIVED_VARIABLES List of sets of name-value-pair attributes, each
#'       set encoding the definition of a variable.
#' @slot name The name assigned to the \code{dataObj} in the MDL file.
#' 
#' @author mwise
setClass("dataObj", 
  slots = c(
    SOURCE = "list",
	DECLARED_VARIABLES = "list",
    DATA_INPUT_VARIABLES = "list",
    DATA_DERIVED_VARIABLES = "list",
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
	stopifnot(is.list(object@ESTIMATE))
	#stopifnot(is.character(object@SIMULATE))
	#stopifnot(is.character(object@EVALUATE))
	#stopifnot(is.character(object@OPTIMISE))
	#stopifnot(is.character(object@DATA))
	#stopifnot(is.character(object@MODEL))
	#stopifnot(is.character(object@TARGET_CODE))
	return(TRUE)
}

################################################################################
#' Task Object S4 Class Definition.
#' 
#' Objects of this class map to occurrences of the \code{taskObj} top-level block
#' in an MDL file. They are created by parsing an MDL file using
#' \link{getTaskPropertiesObjects} or \link{getMDLObjects}.
#' 
#' @slot ESTIMATE List of sets of name-value-pair attributes
#' @slot name The name assigned to the \code{taskObj} in the MDL file.
#' 
#' @author mwise
setClass("taskObj", 
  slots = c(
	ESTIMATE = "list",
	#SIMULATE = "character",
	#EVALUATE = "character",
	#OPTIMISE = "character",
	#DATA = "character",
	#MODEL = "character",
	#TARGET_CODE = "character",
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
  return(TRUE)
}

################################################################################
#' Parameter Object S4 Class Definition.
#' 
#' Objects of this class map to occurrences of the \code{parObj} top-level block
#' in an MDL file. They are created by parsing an MDL file using
#' \link{getParameterObjects} or \link{getMDLObjects}.
#'
#' @slot DECLARED_VARIABLES List of sets of name-value-pair attributes, each
#'       set encoding the definition of a variable.
#' @slot STRUCTURAL Named list of variable names mapping to sets of name-value-pair
#'       attributes of each variable.
#' @slot VARIABILITY Named list of variable names mapping to sets of name-value-pair
#'       attributes of each variable.
#' @slot name The name assigned to the \code{parObj} in the MDL file.
#' 
#' @author mwise
setClass("parObj", 
  slots = c(
	DECLARED_VARIABLES = "list",
  	STRUCTURAL = "list",
  	VARIABILITY = "list",
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
	stopifnot(is.vector(object@IDV))
	stopifnot(is.vector(object@COVARIATES) || is.list(object@COVARIATES))
	stopifnot(is.list(object@VARIABILITY_LEVELS))
	stopifnot(is.vector(object@STRUCTURAL_PARAMETERS))
	stopifnot(is.vector(object@VARIABILITY_PARAMETERS))
	stopifnot(is.list(object@RANDOM_VARIABLE_DEFINITION))
	stopifnot(is.list(object@INDIVIDUAL_VARIABLES))
	stopifnot(is.list(object@MODEL_PREDICTION))
    stopifnot(is.list(object@OBSERVATION))
	stopifnot(is.list(object@GROUP_VARIABLES))
  return(TRUE)
}

### Create mdlObj class:

################################################################################
#' Model Object S4 Class Definition.
#' 
#' Objects of this class map to occurrences of the \code{mdlObj} top-level block
#' in an MDL file. They are created by parsing an MDL file using
#' \link{getModelObjects} or \link{getMDLObjects}.
#' 
#' @slot IDV Vector of variable names.
#' @slot COVARIATES List of sets of name-value-pair attributes, each
#'       set encoding the definition of a variable.
#' @slot VARIABILITY_LEVELS Named list of variable names mapping to
#'       sets of name-value-pair attributes of each variable.
#' @slot STRUCTURAL_PARAMETERS Vector of variable names.
#' @slot VARIABILITY_PARAMETERS Vector of variable names.
#' @slot RANDOM_VARIABLE_DEFINITION List of sets of name-value-pair
#'       attributes, each set encoding the definition of a variable.
#' @slot INDIVIDUAL_VARIABLES List of sets of name-value-pair
#'       attributes, each set encoding the definition of a variable.
#' @slot MODEL_PREDICTION List of sets of items, each being either,
#'       \itemize{
#'         \item{A set of name-value-pair attributes, encoding the
#'               definition of a variable}
#'         \item{A DEQ sub-block, i.e. a named list containing "DEQ"
#'               mapping to a list of sets of name-value-pair attributes,
#'               each set encoding the definition of a variable}
#'         \item{A COMPARTMENT sub-block, i.e. a named list containing
#'               "COMPARTMENT" mapping to a list of sets of name-value-pair
#'               attributes, each set encoding the definition of a variable}
#'       }
#' @slot OBSERVATION List of sets of name-value-pair attributes, each
#'       set encoding the definition of a variable.
#' @slot GROUP_VARIABLES List of sets of name-value-pair attributes, each
#'       set encoding the definition of a variable.
#' @slot name The name assigned to the \code{mdlObj} in the MDL file.
#' 
#' @author mwise
setClass("mdlObj", 
  slots = c(
	IDV = "vector",
	COVARIATES = "list",
	VARIABILITY_LEVELS = "list",
    STRUCTURAL_PARAMETERS = "vector",
    VARIABILITY_PARAMETERS = "vector",
    RANDOM_VARIABLE_DEFINITION ="list",
    INDIVIDUAL_VARIABLES = "list",
    MODEL_PREDICTION = "list",
    OBSERVATION = "list",
    GROUP_VARIABLES = "list",
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


validity.mogObj <- function(object)
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

