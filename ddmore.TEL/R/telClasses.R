# $LastChangedDate: 2014-06-19 $
# $LastChangedBy: khanley $
# 
# Author: khanley
###############################################################################


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


setClass("parObj", 
  slots= c(
  STRUCTURAL = "list",
  PRIOR = "list",
  VARIABILITY = "list"
  )
)

# Create modPred class:
setClass("modPred", 
  slots= c(
  ODE = "vector",
  LIBRARY = "vector"
  )
)


### Create mdlObj class:

setClass("mdlObj", 
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

### Create mogObj class:

setClass("mogObj", 
  slots= c(
  dataObj = "dataObj",
  parObj = "parObj",
  mdlObj = "mdlObj"
  )
)

