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
#' getDesignObjects
#'
#' Parses the specified MDL file, extracting the \code{designObj} MCL Design
#' Object top-level blocks and converting them to S4 objects of class
#' \code{\linkS4class{designObjj}}.
#' 
#' Slots within this object contain the parsed information from the corresponding
#' MCL Design Object sub-blocks; see \linkS4class{designObj}.
#'
#' @param file File path to the .mdl file containing the parameter object(s).
#' @param object TODO Not currently used.
#' @param name (Optional) Specifies the design object item, by name, to be
#'        retrieved by getDesignObjects If multiple design objects exist
#'        in the .mdl file then using the name argument allows the user to target
#'        a specific design object.
#' @param fisServer FISServer instance.
#' @return List of S4 Objects of class \code{designObj}. If name is specified, only the 
#'         single specified object is returned.
#' 
#' @examples
#' # Retrieve all design objects
#' paramObjsList <- getDesignObjects("UseCase2.mdl")
#' 
#' @export
#' @docType methods
#' @rdname getDesignObjects-methods
#'
#' @include Classes.R

setGeneric("getDesignObjects", function(file, object, name, fisServer = DDMORE.getServer()) {
	# create object in R from parser:
	if (!missing(name)) {
		res <- .parseMDLFile(file, name=name, type="designObj", fisServer = fisServer)
	} else{
		res <- .parseMDLFile(file, type="designObj", fisServer = fisServer)
	}
	return(res)
})

#' @rdname getDesignObjects-methods
#' @aliases getDesignObjects,mogObj,mogObj-method
setMethod("getDesignObjects", signature=signature(object="mogObj"),
  function(file, object, name, fisServer = DDMORE.getServer()) {
    return(x@parObj)
})


