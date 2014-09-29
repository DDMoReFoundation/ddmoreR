# 
# Author: khanley, mwise
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
  stopifnot(is.list(object@DATA_DERIVED_VARIABLES))
  return(TRUE)
}

#' @slot DATA_INPUT_VARIABLES A list
#' @slot SOURCE A named list of parsed sections of the control file
#' @slot RSCRIPT TBC 
#' @slot HEADER TBC
#' @slot FILE String containing name of the data file - TBC
#' @slot DESIGN  Named list of lists describing the design of the experiment - TBC
#' @slot DATA_DERIVED_VARIABLES TBC - vector of strings?
#' @author khanley

setClass("dataObj", package="DDMoRe.TEL", 
  slots=c(
    DATA_INPUT_VARIABLES="list",
	# TODO: TBC - These need to be populated
    SOURCE = "list",
    RSCRIPT = "list",
    HEADER = "list",
    FILE = "list",
    DESIGN = "list",
    DATA_DERIVED_VARIABLES = "list"
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
	stopifnot(is.character(object@content))
	return(TRUE)
}

#' @slot content A character vector containing the full content of the block "as-is"
#' @author khanley
setClass("taskObj", package="DDMoRe.TEL", 
  slots= c(
    content = "character"
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
  stopifnot(is.list(object@PRIOR))
  stopifnot(is.list(object@VARIABILITY))
  return(TRUE)
}

#' @slot STRUCTURAL A list
#' @slot PRIOR A named list
#' @slot VARIABILITY A named list
#' @author khanley
setClass("parObj", package="DDMoRe.TEL", 
  slots= c(
  STRUCTURAL = "list",
  PRIOR = "list",
  VARIABILITY = "list"
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

#' @slot ODE A character vector
#' @slot LIBRARY A character vector
#' @slot content A character vector
#' @author khanley
setClass("modPred", package="DDMoRe.TEL", 
  slots= c(
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
	stopifnot(is.list(object@MODEL_INPUT_VARIABLES))
	stopifnot(is.list(object@STRUCTURAL_PARAMETERS))
	stopifnot(is.list(object@VARIABILITY_PARAMETERS))
	stopifnot(is.character(object@GROUP_VARIABLES))
	stopifnot(is.list(object@RANDOM_VARIABLE_DEFINITION))
	stopifnot(is.character(object@INDIVIDUAL_VARIABLES))
	stopifnot(is.modPred(object@MODEL_PREDICTION))
    stopifnot(is.character(object@OBSERVATION))
	stopifnot(is.character(object@ESTIMATION))
	stopifnot(is.list(object@MODEL_OUTPUT_VARIABLES))
  return(TRUE)
}

### Create mdlObj class:

#' @slot MODEL_INPUT_VARIABLES A list
#' @slot STRUCTURAL_PARAMETERS A list
#' @slot VARIABILITY_PARAMETERS A list
#' @slot GROUP_VARIABLES A character vector
#' @slot RANDOM_VARIABLE_DEFINITION A list
#' @slot INDIVIDUAL_VARIABLES A character vector
#' @slot MODEL_PREDICTION An object of class "modPred"
#' @slot OBSERVATION  A character vector
#' @slot ESTIMATION A character vector
#' @slot MODEL_OUTPUT_VARIABLES A list
#' @author khanley
setClass("mdlObj", package="DDMoRe.TEL", 
  slots= c(
    MODEL_INPUT_VARIABLES = "list",
    STRUCTURAL_PARAMETERS = "list",
    VARIABILITY_PARAMETERS = "list",
    GROUP_VARIABLES = "character",
    RANDOM_VARIABLE_DEFINITION ="list",
    INDIVIDUAL_VARIABLES = "character",
    MODEL_PREDICTION = "modPred",
    OBSERVATION = "character",
	ESTIMATION = "character",
	MODEL_OUTPUT_VARIABLES = "list"
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
setClass("mogObj", package="DDMoRe.TEL", 
  slots= c(
  dataObj = "dataObj",
  parObj = "parObj",
  mdlObj = "mdlObj", 
  taskObj = "taskObj"
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
#' Creates a mogObj from a list of dataObj, parObj, mdlObk and taskObj objects. Note
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
  
  if(nDat!=1 | nPar!=1 | nMdl!=1 | nTask!=1){stop("The list provided must contain exactly one
    of each type of object: dataObj, parObj, mdlObj and taskObj")}
  
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

