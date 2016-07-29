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

################################################################################
#' getTaskPropertiesObjects
#'
#' Parses the specified MDL file, extracting the \code{taskObj} MCL Task Properties
#' Object top-level blocks and converting them to S4 objects of class
#' \code{\linkS4class{taskObj}}.
#' 
#' Slots within this object contain the parsed information from the corresponding
#' MCL Task Properties Object sub-blocks; see \linkS4class{taskObj}.
#'
#' @param file File path to the .mdl file containing the task properties object(s).
#' @param object TODO Not currently used.
#' @param name (Optional) Specifies the task properties object item, by name, to be
#'        retrieved by getTaskPropertiesObjects. If multiple task properties objects
#'        exist in the .mdl file then using the name argument allows the user to target
#'        a specific task properties object.
#' @param fisServer FISServer instance.
#' @return List of S4 Objects of class \code{taskObj}. If name is specified, only the 
#'         single specified object is returned.
#' 
#' @examples
#' # Retrieve all task properties objects
#' taskPropsList <- getTaskPropertiesObjects("UseCase2.mdl")
#' # Retrieve a known task properties object by name
#' warfTaskProps <- getTaskPropertiesObjects("UseCase2.mdl", name='warfarin_PK_ODE_task')
#'
#' @export
#' @docType methods
#' @rdname getTaskPropertiesObjects-methods
#'
#' @include Classes.R

setGeneric("getTaskPropertiesObjects", function(file, object, name, fisServer = DDMORE.getServer()) { 
  # create object in R from parser:
  if (!missing(name)) {
	  res <- .parseMDLFile(file, name=name, type="taskObj", fisServer = fisServer)
  } else{
	  res <- .parseMDLFile(file, type="taskObj", fisServer = fisServer)
  }
  return(res)
})

#' @rdname getTaskPropertiesObjects-methods
#' @aliases getTaskPropertiesObjects,mogObj,mogObj-method
setMethod("getTaskPropertiesObjects", signature=signature(object="mogObj"),
  function(file, object, name, fisServer = DDMORE.getServer()) {
    return(x@taskObj)
})


