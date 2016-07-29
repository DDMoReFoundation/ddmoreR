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

##############################################################
#' createMogObj
#'
#' Function to create an object of class mogObj from a
#' data object (dataObj), parameter object (parObj), model object (mdlObj)
#' and task object (taskObj). The mog name can be specified if desired.
#'
#' @usage createMogObj(mdlObj=myMdlObj, taskObj=myTaskObj)
#' @usage createMogObj(dataObj=myDataObj, parObj=myParObj, mdlObj=myMdlObj, taskObj=myTaskObj, mogName="myNewMog")
#'
#' @param dataObj (Optional) An object of class dataObj
#' @param parObj (Optional) An object of class parObj
#' @param mdlObj An object of class mdlObj
#' @param taskObj An object of class taskObj
#' @param priorObj (Optional) An object of class priorObj
#' @param designObj (Optional) An object of class designObj
#' @param mogName (Optional) The name to assign to the new mogObj
#' @param info (Optional) Additional information which should be included in the 'INFO' sub-block of the new mogObj
#'
#' @return An S4 Object of class "mogObj".
#'
#' @export
#' @docType methods
#' @rdname createMogObj
#' 
createMogObj <- function(dataObj = NULL, parObj = NULL, mdlObj, taskObj, priorObj = NULL, 
    designObj = NULL, mogName = "outputMog", info = list()) {
	if (missing(mogName)) {
		new("mogObj", 
			dataObj = dataObj,
			parObj = parObj,
			mdlObj = mdlObj,
			taskObj = taskObj,
            priorObj = priorObj,
			designObj = designObj,
            info = .createInfo(info)
		)
	} else {
		new("mogObj", 
			dataObj = dataObj,
			parObj = parObj,
			mdlObj = mdlObj,
			taskObj = taskObj,
            priorObj = priorObj,
			designObj = designObj,
			name = mogName,
            info = .createInfo(info)
		)
	}
}

#'
#' Creates representation of the mogObj's INFO sub-block from list of key-value pairs
#' 
.createInfo <- function(info = list()) {
	if(is.null(info)) {
		return(list())
	}
	lapply(names(info), function(x) {
				def <- list()
				def[[x]]<-sprintf("\"%s\"", info[[x]])
				res <- list(".subtype"="PropertyStmt", "def" = def)
				res
			})
}