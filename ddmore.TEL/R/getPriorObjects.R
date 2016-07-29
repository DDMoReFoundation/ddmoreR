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
#' getPriorObjects
#'
#' Parses the specified MDL file, extracting the \code{priorObj} MCL Prior
#' Object top-level blocks and converting them to S4 objects of class
#' \code{\linkS4class{priorObjj}}.
#' 
#' Slots within this object contain the parsed information from the corresponding
#' MCL Prior Object sub-blocks; see \linkS4class{priorObj}.
#'
#' @param file File path to the .mdl file containing the prior object(s).
#' @param object TODO Not currently used.
#' @param name (Optional) Specifies the prior object item, by name, to be
#'        retrieved by getPriorObjects If multiple prior objects exist
#'        in the .mdl file then using the name argument allows the user to target
#'        a specific prior object.
#' @param fisServer FISServer instance.
#' @return List of S4 Objects of class \code{priorObj}. If name is specified, only the 
#'         single specified object is returned.
#' 
#' @examples
#' # Retrieve all prior objects
#' priorObjsList <- getPriorObjects("UseCase2.mdl")
#' 
#' @export
#' @docType methods
#' @rdname getPriorObjects-methods
#'
#' @include Classes.R

setGeneric("getPriorObjects", function(file, object, name, fisServer = DDMORE.getServer()) {
	# create object in R from parser:
	if (!missing(name)) {
		res <- .parseMDLFile(file, name=name, type="priorObj", fisServer = fisServer)
	} else{
		res <- .parseMDLFile(file, type="priorObj", fisServer = fisServer)
	}
	return(res)
})

#' @rdname getPriorObjects-methods
#' @aliases getPriorObjects,mogObj,mogObj-method
setMethod("getPriorObjects", signature=signature(object="mogObj"),
  function(file, object, name, fisServer = DDMORE.getServer()) {
    return(x@priorObj)
})


