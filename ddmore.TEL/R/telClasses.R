# $LastChangedDate: 2014-06-19 $
# $LastChangedBy: khanley $
# 
# Author: khanley
###############################################################################

#### Data object class
validity.dataObj <- function(object)
{
	stopifnot(is.list(object@DATA_INPUT_VARIABLES))
  stopifnot(is.list(object@SOURCE))
  stopifnot(is.list(object@RSCRIPT))
  stopifnot(is.list(object@HEADER))
  stopifnot(is.list(object@FILE))
  stopifnot(is.list(object@DESIGN))
  stopifnot(is.vector(object@DATA_DERIVED_VARIABLES))
  return(TRUE)
}

#' @slot DATA_INPUT_VARIABLES A named list
#' @slot SOURCE A list of parsed sections of the control file
#' @slot RSCRIPT TBC 
#' @slot HEADER TBC
#' @slot FILE String containing name of the data file
#' @slot DESIGN  Named list of lists describing the design of the experiment
#' @slot DATA_DERIVED_VARIABLES TBC - vector of strings?
#' @author khanley

setClass("dataObj", 
  slots=c(
    DATA_INPUT_VARIABLES="list",
    SOURCE = "list",
    RSCRIPT = "list",
    HEADER = "list",
    FILE = "list",
    DESIGN = "list",
    DATA_DERIVED_VARIABLES = "vector"
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
#' @returns TRUE or FALSE 
is.dataObj <- function(obj){

  class(obj)=="dataObj"

}


#### Task object class
validity.taskObj <- function(object)
{
	stopifnot(is.list(object@IMPORT))
  stopifnot(is.list(object@DATA))
  stopifnot(is.list(object@PARAMETER))
  stopifnot(is.list(object@MODEL))
  stopifnot(is.list(object@TASK_FUNCTION))
  stopifnot(is.list(object@TARGET_CODE))
  return(TRUE)
}

#' @slot IMPORT A vector
#' @slot DATA A named list
#' @slot PARAMETER A named list
#' @slot MODEL A named list
#' @slot TASK_FUNCTION A named list
#' @slot TARGET_CODE A named list
#' @author khanley
setClass("taskObj", 
  slots= c(
  IMPORT = "list",
  DATA = "list",
  PARAMETER = "list",
  MODEL = "list",
  TASK_FUNCTION = "list",
  TARGET_CODE = "list"
  )
)

##############################################################
#' is.taskObj
#'
#' Determines if an object is of class "taskObj"
#'
#' @usage is.taskObj(object)
#'
#' @returns TRUE or FALSE 
is.taskObj <- function(obj){

  class(obj)=="taskObj"

}


#### Parameter object class

validity.parObj <- function(object)
{
	stopifnot(is.list(object@STRUCTURAL))
  stopifnot(is.list(object@PRIOR))
  stopifnot(is.list(object@VARIABILITY))
  return(TRUE)
}

#' @slot STRUCTURAL A vector
#' @slot PRIOR A named list
#' @slot VARIABILITY A named list
#' @author khanley
setClass("parObj", 
  slots= c(
  STRUCTURAL = "list",
  PRIOR = "list",
  VARIABILITY = "list"
  )
)

#' is.parObj
#'
#' Determines if an object is of class "parObj"
#'
#' @usage is.parObj(object)
#'
#' @returns TRUE or FALSE 
is.parObj <- function(obj){

  class(obj)=="parObj"

}



#### Model prediction object class


validity.modPred <- function(object)
{
	stopifnot(is.vector(object@ODE))
  stopifnot(is.vector(object@LIBRARY))
  return(TRUE)
}


# Create modPred class:

#' @slot ODE A vector
#' @slot LIBRARY A vector
#' @author khanley
setClass("modPred", 
  slots= c(
  ODE = "vector",
  LIBRARY = "vector"
  )
)


#' is.modPred
#'
#' Determines if an object is of class "modPred"
#'
#' @usage is.modPred(object)
#'
#' @returns TRUE or FALSE 
is.modPred <- function(obj){

  class(obj)=="modPred"

}



#### Model object class


validity.modObj<- function(object)
{
	stopifnot(is.list(object@MODEL_INPUT_VARIABLES))
	stopifnot(is.vector(object@STRUCTURAL_PARAMETERS))
	stopifnot(is.vector(object@VARIABILITY_PARAMETERS))
	stopifnot(is.vector(object@GROUP_VARIABLES))
	stopifnot(is.vector(object@RANDOM_VARIABLE_DEFINITION))
	stopifnot(is.vector(object@INDIVIDUAL_VARIABLES))
	stopifnot(is.modPred(object@MODEL_PREDICTION))
  stopifnot(is.list(object@OBSERVATION))
  return(TRUE)
}

### Create modObj class:

#' @slot MODEL_INPUT_VARIABLES A list
#' @slot STRUCTURAL_PARAMETERS A vector
#' @slot VARIABILITY_PARAMETERS A vector
#' @slot GROUP_VARIABLES A vector
#' @slot RANDOM_VARIABLE_DEFINITION A vector
#' @slot INDIVIDUAL_VARIABLES A vector
#' @slot MODEL_PREDICTION An object of class "modPred"
#' @slot OBSERVATION A list
#' @author khanley
setClass("modObj", 
  slots= c(
    MODEL_INPUT_VARIABLES = "list",
    STRUCTURAL_PARAMETERS = "vector",
    VARIABILITY_PARAMETERS = "vector",
    GROUP_VARIABLES = "vector",
    RANDOM_VARIABLE_DEFINITION ="vector",
    INDIVIDUAL_VARIABLES = "vector",
    MODEL_PREDICTION = "modPred",
    OBSERVATION = "list"
  )
)

#' is.modObj
#'
#' Determines if an object is of class "modObj"
#'
#' @usage is.modPObj(object)
#'
#' @returns TRUE or FALSE 
is.modObj <- function(obj){

  class(obj)=="modObj"

}

#### MOG class



validity.mogObj <- function(object)
{
	stopifnot(validity.dataObj(object@dataObj))
	stopifnot(validity.parObj(object@parObj))
	stopifnot(validity.modObj(object@modObj))
	stopifnot(validity.taskObj(object@taskObj))
  return(TRUE)
}


### Create mogObj class:

#' @slot dataObj Object of class "dataObj"
#' @slot parObj Object of class "parObj"
#' @slot modObj Object of class "modObj"
#' @slot taskObj Object of class "taskObj"
#' @author khanley
setClass("mogObj", 
  slots= c(
  dataObj = "dataObj",
  parObj = "parObj",
  modObj = "modObj", 
  taskObj = "taskObj"
  )
)


#' is.mogObj
#'
#' Determines if an object is of class "mogObj"
#'
#' @usage is.mogObj(object)
#'
#' @returns TRUE or FALSE 
is.mogObj <- function(obj){

  class(obj)=="mogObj"

}



