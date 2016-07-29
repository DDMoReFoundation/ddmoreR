################################################################################
# Copyright (C) 2016 Mango Business Solutions Ltd, http://www.mango-solutions.com
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU Affero General Public License as published by the
# Free Software Foundation, version 3.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
# for more details.
#
# You should have received a copy of the GNU Affero General Public License along
# with this program. If not, see <http://www.gnu.org/licenses/agpl-3.0.html>.
################################################################################
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
#' @include FISServerMock.R
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
  is(obj, class2 = "dataObj")
}


#### Task object class


validity.taskObj <- function(object)
{
	stopifnot(is.null(object@ESTIMATE) || is.list(object@ESTIMATE))
	stopifnot(is.null(object@SIMULATE) || is.list(object@SIMULATE))
	stopifnot(is.null(object@EVALUATE) || is.list(object@EVALUATE))
	stopifnot(is.null(object@OPTIMISE) || is.list(object@OPTIMISE))
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
#' @slot ESTIMATE Set of name-value-pair attributes, i.e. named list
#' @slot SIMULATE Set of name-value-pair attributes, i.e. named list
#' @slot EVALUATE Set of name-value-pair attributes, i.e. named list
#' @slot OPTIMISE Set of name-value-pair attributes, i.e. named list
#' @slot name The name assigned to the \code{taskObj} in the MDL file.
#' 
#' @author mwise
setClass("taskObj", 
  slots = c(
	ESTIMATE = "ANY",
	SIMULATE = "ANY",
	EVALUATE = "ANY",
	OPTIMISE = "ANY",
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
  is(obj, class2 = "taskObj")
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
#' @usage is.parObj(obj = object)
#'
#' @return TRUE or FALSE 
is.parObj <- function(obj){
  is(obj, class2 = "parObj")
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
#' @usage is.mdlObj(obj = object)
#'
#' @return TRUE or FALSE 
is.mdlObj <- function(obj){
  is(obj, class2 = "mdlObj")
}

#### Design object class


validity.designObj <- function(object)
{
  stopifnot(is.list(object@DECLARED_VARIABLES))
  stopifnot(is.list(object@INTERVENTION))
  stopifnot(is.list(object@DESIGN_PARAMETERS))
  stopifnot(is.list(object@STUDY_DESIGN))
  stopifnot(is.list(object@SAMPLING))
  stopifnot(is.list(object@DESIGN_SPACES))
  stopifnot(is.list(object@POPULATION))
  stopifnot(is.list(object@COVARIATES))
  return(TRUE)
}

################################################################################
#' Design Object S4 Class Definition.
#' 
#' Objects of this class map to occurrences of the \code{designObj} top-level block
#' in an MDL file. They are created by parsing an MDL file using
#' \link{getParameterObjects} or \link{getMDLObjects}.
#'
#' @slot DECLARED_VARIABLES List of sets of name-value-pair attributes, each
#'       set encoding the definition of a variable.
#' @slot INTERVENTION Named list of intervention names mapping to interventions definitions.
#' @slot STUDY_DESIGN Named list of study arms names mapping to their design
#' @slot SAMPLING Named list of sampling definitions
#' @slot name The name assigned to the \code{parObj} in the MDL file.
#' 
#' @author mrogalski
setClass("designObj", 
  slots = c(
    DECLARED_VARIABLES = "list",
    DESIGN_PARAMETERS = "list",
    INTERVENTION = "list",
    STUDY_DESIGN = "list",
    SAMPLING = "list",
    DESIGN_SPACES = "list",
    POPULATION = "list",
    COVARIATES = "list",
    name = "character"
  ),
  validity = validity.designObj
)

#' is.designObj
#'
#' Determines if an object is of class "designObj"
#'
#' @usage is.designObj(obj = object)
#'
#' @return TRUE or FALSE 
is.designObj <- function(obj){
  is(obj, class2 = "designObj")
}



################################################################################
# Prior Object Specification.

validity.priorObj <- function(object) {
  stopifnot(is.list(object@PRIOR_PARAMETERS) || is.null(object@PRIOR_PARAMETERS))
  stopifnot(is.list(object@PRIOR_VARIABLE_DEFINITION))
  stopifnot(is.list(object@PRIOR_SOURCE))
  stopifnot(is.list(object@INPUT_PRIOR_DATA))
  return(TRUE)
}


#' Prior Object S4 Class Definition.
#' 
#' Objects of this class map to occurrences of the \code{priorObj} top-level block
#' in an MDL file. They are created by parsing an MDL file using
#' \link{getPriorObjects} or \link{getMDLObjects}.
#'
#' @slot DECLARED_VARIABLES List of sets of name-value-pair attributes, each
#'       set encoding the definition of a variable.
#' @slot INTERVENTION Named list of intervention names mapping to interventions definitions.
#' @slot STUDY_DESIGN Named list of study arms names mapping to their design
#' @slot SAMPLING Named list of sampling definitions
#' @slot name The name assigned to the \code{priorObj} in the MDL file.

setClass("priorObj", 
  slots = c(
    PRIOR_PARAMETERS = "ANY",
    PRIOR_VARIABLE_DEFINITION = "list",
    PRIOR_SOURCE = "list",
    INPUT_PRIOR_DATA = "list",
    name = "character"
  ),
  validity = validity.priorObj
)

#' is.priorObj
#'
#' Determines if an object is of class "priorObj"
#'
#' @usage is.priorObj(obj = object)
#'
#' @return TRUE or FALSE
is.priorObj <- function(obj) {
  is(obj, class2 = "priorObj")
}


################################################################################
#### MOG class


validity.mogObj <- function(object)
{
	stopifnot(is.null(object@dataObj) || validity.dataObj(object@dataObj))
	stopifnot(is.null(object@parObj) || validity.parObj(object@parObj))
	stopifnot(validity.mdlObj(object@mdlObj))
	stopifnot(validity.taskObj(object@taskObj))
    stopifnot(is.null(object@priorObj) || validity.priorObj(object@priorObj))
	stopifnot(is.null(object@designObj) || validity.designObj(object@designObj))
	stopifnot(is.list(object@info))
    return(TRUE)
}


### Create mogObj class:

################################################################################
#' MOG (Model Object Group) S4 Class Definition.
#'
#' 'Aggregator' class comprising no more than one occurrence of each of the 
#' following types of object as parsed from an MDL file:
#' \itemize{
#'   \item{\link{dataObj} or \link{designObj}}
#'   \item{\link{parObj} or \link{priorObj}}
#'   \item{\link{mdlObj}}
#'   \item{\link{taskObj}}}
#' }
#' 
#' @slot dataObj (Optional) Object of class \code{dataObj}.
#' @slot parObj (Optional) Object of class \code{parObj}.
#' @slot mdlObj Object of class \link{mdlObj}.
#' @slot taskObj Object of class \link{taskObj}.
#' @slot priorObj (Optional) Object of class \code{priorObj}.
#' @slot designObj (Optional) Object of class \link{designObj}.
#' @slot name A name to be assigned to the MOG; used when writing back out to MDL.
#' 
#' @author khanley
setClass("mogObj", 
  slots = c(
	dataObj = "ANY",
	parObj = "ANY",
	mdlObj = "mdlObj", 
	taskObj = "taskObj",
    priorObj = "ANY",
	designObj = "ANY",
	info = "list",
	name = "character"
  ),
  validity = validity.mogObj
)


#' is.mogObj
#'
#' Determines if an object is of class "mogObj"
#'
#' @usage is.mogObj(obj = object)
#'
#' @return TRUE or FALSE 
is.mogObj <- function(obj){
    is(obj, class2 = "mogObj")
}

#' as.mogObj
#'
#' Creates a mogObj from a list of mdlObj and taskObj objects, 
#' and optionally dataObj, parObj, priorObj and designObj. Note
#' that only one of each type may be included.
#'
#' @usage as.mogObj(list = list(myMdlObj, myTaskObj))
#' @export
#' @return An object of class mogObj
as.mogObj <- function(list){

  classes <- sapply(list, function(x){class(x)})
  
  nDat <- sum(classes=="dataObj")
  nPar <- sum(classes=="parObj")
  nMdl <- sum(classes=="mdlObj")
  nTask <- sum(classes=="taskObj")
  nPrior <- sum(classes=="priorObj")
  nDesign <- sum(classes=="designObj")
  
  if (nMdl!=1 | nTask!=1 | nPar>1 | nDat>1 | nPrior>1 | nDesign>1) {
    stop("The list provided must contain exactly one of each mdlObj and taskObj objects,",
        " exactly one parObj or priorObj and exaclty one designObj or dataObj.")
  }
  dat <- NULL
  if(nDat==1) {
    dat <- list[classes=="dataObj"][[1]]
  }
  par <- NULL
  if(nPar==1) {
    par <- list[classes=="parObj"][[1]]
  }
  mdl <- list[classes=="mdlObj"][[1]]
  task <- list[classes=="taskObj"][[1]]
  prior <- NULL
  if(nPrior==1) {
    prior <- list[classes=="priorObj"][[1]]
  }
  design <- NULL
  if(nDesign==1) {
    design <- list[classes=="designObj"][[1]]
  }
  
  res <- new("mogObj", 
             dataObj = dat,
             parObj = par,
             mdlObj = mdl, 
             taskObj = task, 
             priorObj = prior,
             designObj = design,
			 info = list()
  )
  return(res)
}
