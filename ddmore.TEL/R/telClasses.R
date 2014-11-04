# 
# Author: khanley, mwise
###############################################################################

#### Data object class
validity.dataObj <- function(object)
{
  stopifnot(is.list(object@DATA_INPUT_VARIABLES))
  stopifnot(is.list(object@SOURCE))
  stopifnot(is.list(object@DATA_DERIVED_VARIABLES))
  stopifnot(is.character(object@TARGET_CODE))
  return(TRUE)
}

#' @slot DATA_INPUT_VARIABLES A named list of variables keyed by their names
#' @slot SOURCE A list of attribute values keyed by their attribute names
#' @slot DATA_DERIVED_VARIABLES TODO TBC
#' @slot TARGET_CODE TODO TBC
#' @author khanley

setClass("dataObj", 
  slots = c(
    DATA_INPUT_VARIABLES="list",
    SOURCE = "list",
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

#' @slot ESTIMATE A character vector content of this sub-block "as-is"
#' @slot SIMULATE A character vector content of this sub-block "as-is"
#' @slot EVALUATE A character vector content of this sub-block "as-is"
#' @slot OPTIMISE A character vector content of this sub-block "as-is"
#' @slot DATA A character vector content of this sub-block "as-is"
#' @slot MODEL A character vector content of this sub-block "as-is"
#' @slot TARGET_CODE TODO TBC
#' @author khanley
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
  stopifnot(is.list(object@STRUCTURAL))
  stopifnot(is.list(object@VARIABILITY))
  stopifnot(is.list(object@PRIOR_PARAMETERS))
  stopifnot(is.character(object@TARGET_CODE))
  return(TRUE)
}

#' @slot STRUCTURAL A named list of variables keyed by their names
#' @slot VARIABILITY A named list of variables keyed by their names
#' @slot PRIOR_PARAMETERS TODO TBC
#' @slot TARGET_CODE TODO TBC
#' @author khanley
setClass("parObj", 
  slots = c(
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



#### Model prediction object class


validity.modPred <- function(object)
{
  stopifnot(is.character(object@ODE))
  stopifnot(is.character(object@LIBRARY))
  stopifnot(is.character(object@content))
  return(TRUE)
}


# Create modPred class:

#' @slot ODE A character vector content of this sub-block "as-is"
#' @slot LIBRARY A character vector content of this sub-block "as-is"
#' @slot content A character vector of the remaining content of the Model Prediction block "as-is"
#' @author khanley
setClass("modPred", 
  slots = c(
	ODE = "character",
	LIBRARY = "character",
	content = "character"
  ),
  validity = validity.modPred
)


#' is.modPred
#'
#' Determines if an object is of class "modPred"
#'
#' @usage is.modPred(object)
#'
#' @return TRUE or FALSE 
is.modPred <- function(obj){

  class(obj)=="modPred"

}



#### Model object class


validity.mdlObj <- function(object)
{
	stopifnot(is.list(object@STRUCTURAL_PARAMETERS))
	stopifnot(is.list(object@VARIABILITY_PARAMETERS))
	stopifnot(is.list(object@INDIVIDUAL_VARIABLES))
	stopifnot(is.list(object@RANDOM_VARIABLE_DEFINITION))
	stopifnot(is.list(object@MODEL_OUTPUT_VARIABLES))
	stopifnot(is.list(object@MODEL_INPUT_VARIABLES))
    stopifnot(is.list(object@OBSERVATION))
	stopifnot(is.modPred(object@MODEL_PREDICTION))
	stopifnot(is.list(object@GROUP_VARIABLES))
	stopifnot(is.list(object@ESTIMATION))
	stopifnot(is.list(object@SIMULATION))
	stopifnot(is.character(object@TARGET_CODE))
  return(TRUE)
}

### Create mdlObj class:

#' @slot STRUCTURAL_PARAMETERS A named list of variables keyed by their names
#' @slot VARIABILITY_PARAMETERS A named list of variables keyed by their names
#' @slot INDIVIDUAL_VARIABLES A named list of variables keyed by their names
#' @slot RANDOM_VARIABLE_DEFINITION A named list of random-distribution-type variables keyed by their names
#' @slot MODEL_OUTPUT_VARIABLES A named list of variables keyed by their names
#' @slot MODEL_INPUT_VARIABLES A named list of variables keyed by their names
#' @slot OBSERVATION A named list of variables, a mixture of 'standard' and random-distribution-type, keyed by their names
#' @slot MODEL_PREDICTION An object of class "modPred"
#' @slot GROUP_VARIABLES TODO TBC
#' @slot ESTIMATION TODO TBC
#' @slot SIMULATION TODO TBC
#' @slot TARGET_CODE TODO TBC
#' @author khanley
setClass("mdlObj", 
  slots = c(
    STRUCTURAL_PARAMETERS = "list",
    VARIABILITY_PARAMETERS = "list",
    INDIVIDUAL_VARIABLES = "list",
    RANDOM_VARIABLE_DEFINITION ="list",
	MODEL_OUTPUT_VARIABLES = "list",
    MODEL_INPUT_VARIABLES = "list",
    OBSERVATION = "list",
    MODEL_PREDICTION = "modPred",
    GROUP_VARIABLES = "list",
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

#' @slot dataObj Object of class "dataObj"
#' @slot parObj Object of class "parObj"
#' @slot mdlObj Object of class "mdlObj"
#' @slot taskObj Object of class "taskObj"
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

