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
#' getParameterObjects
#'
#' Parses the specified MDL file, extracting the \code{parObj} MCL Parameter
#' Object top-level blocks and converting them to S4 objects of class
#' \code{\linkS4class{parObj}}.
#' 
#' Slots within this object contain the parsed information from the corresponding
#' MCL Parameter Object sub-blocks; see \linkS4class{parObj}.
#' 
#' To update the parameter values within the object the
#' \link[ddmore:updateParObj-methods]{updateParObj} function should be used.
#'
#' @param file File path to the .mdl file containing the parameter object(s).
#' @param object TODO Not currently used.
#' @param name (Optional) Specifies the parameter object item, by name, to be
#'        retrieved by getParameterObjects. If multiple parameter objects exist
#'        in the .mdl file then using the name argument allows the user to target
#'        a specific parameter object.
#' @param fisServer FISServer instance.
#' @return List of S4 Objects of class \code{parObj}. If name is specified, only the 
#'         single specified object is returned.
#' 
#' @examples
#' # Retrieve all parameter objects
#' paramObjsList <- getParameterObjects("UseCase2.mdl")
#' # Retrieve a known parameter object by name
#' warfParamObj <- getParameterObjects("UseCase2.mdl", name="warfarin_PK_ODE_par")
#' 
#' @seealso \link[ddmore:updateParObj-methods]{updateParObj}
#'
#' @export
#' @docType methods
#' @rdname getParameterObjects-methods
#'
#' @include Classes.R

setGeneric("getParameterObjects", function(file, object, name, fisServer = DDMORE.getServer()) {
	# create object in R from parser:
	if (!missing(name)) {
		res <- .parseMDLFile(file, name=name, type="parObj", fisServer = fisServer)
	} else{
		res <- .parseMDLFile(file, type="parObj", fisServer = fisServer)
	}
	return(res)
})

#' @rdname getParameterObjects-methods
#' @aliases getParameterObjects,mogObj,mogObj-method
setMethod("getParameterObjects", signature=signature(object="mogObj"),
  function(file, object, name, fisServer = DDMORE.getServer()) {
    return(x@parObj)
})


