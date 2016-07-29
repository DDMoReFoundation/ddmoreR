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

#' The TaskInformation Object Class (S4) 
#'
#' An object to house all data associated with Task Information,
#' primarily messages emitted by the target tool.
#' 
#' @slot ErrorMessages list of named lists containing attributes Toolname, Name, Content, Severity (integer)
#' @slot WarningMessages list of named lists containing attributes Toolname, Name, Content, Severity (integer)
#' @slot TerminationMessages list of named lists containing attributes Toolname, Name, Content, Severity (integer)
#' @slot InformationMessages list of named lists containing attributes Toolname, Name, Content, Severity (integer)
#' @slot OutputFilePath string
#' @slot RunTime real
#' @slot NumberChains integer
#' @slot NumberIterations integer
#' 
#' @name TaskInformation-class
#' @rdname TaskInformation-class
#' @exportClass TaskInformation
#' @aliases TaskInformation
#' @examples
#' tinfo <- new(Class = "TaskInformation")
#' print(tinfo)
#' validObject(tinfo)
#' 
#' @include StandardOutputObjectXmlParsers.R
#' @include utils.R

setClass(Class = "TaskInformation",
	slots = c(
		ErrorMessages = "list",
		WarningMessages = "list",
		TerminationMessages = "list",
		InformationMessages = "list",
		OutputFilePath = "character",
		RunTime = "numeric",
		NumberChains = "integer",
		NumberIterations = "integer"
	),
	validity = function(object) {
		# TODO implement checking
		return(TRUE)
	}
)

#' Initialisation function / Constructor for TaskInformation S4 class
#' @param .Object new instance of the class
#' @param xmlNodeTaskInformation XML Node representation of the block
#' @include StandardOutputObjectXmlParsers.R

setMethod("initialize", "TaskInformation", function(.Object, xmlNodeTaskInformation = NULL) {
	
	if (!is.null(xmlNodeTaskInformation)) {
		for (child in .getChildNodes(xmlNodeTaskInformation)) {
			childName <- xmlName(child)
			switch(childName,
				"Message" = {
					# Get "type" attribute
					messageType <- xmlGetAttr(child, "type")
	    			# Pull out message content
					messageContent <- list()
					for (grandchild in .getChildNodes(child)) {
						grandchildName <- xmlName(grandchild)
						switch (grandchildName,
							"Toolname" = { messageContent[[grandchildName]] <- xmlValue(grandchild) },
							"Name" = { messageContent[[grandchildName]] <- xmlValue(grandchild) },
							"Content" = { messageContent[[grandchildName]] <- xmlValue(grandchild) },
							"Severity" = { messageContent[[grandchildName]] <- as.integer(xmlValue(grandchild)) },
							warning(paste("Unexpected child node of TaskInformation::Message node encountered:", grandchildName))
						)
					}
	    			# Assign message content to appropriate list 
	    			if (messageType %in% c("ERROR", "WARNING", "TERMINATION", "INFORMATION")) {
						slotName <- paste0(capitalise_first(tolower(messageType)), "Messages")
						# Append the message to the end of the list in the relevant slot in the TaskInformation object
						slot(.Object, slotName) <- append(slot(.Object, slotName), list(messageContent)) # wrap within a list to ensure it is still a list when just 1 message
	    			} else {
						warning("Unexpected message type ", messageType, " encountered on TaskInformation::Message")
	    			}
				},
				"OutputFilePath" = {
					slot(.Object, childName) <- as.character(xmlValue(child))
				},
				"RunTime" = {
					slot(.Object, childName) <- as.numeric(xmlValue(child))
				},
				"NumberChains" = {
					slot(.Object, childName) <- as.integer(xmlValue(child))
				},
				"NumberIterations" = {
					slot(.Object, childName) <- as.integer(xmlValue(child))
				},
				warning(paste("Unexpected child node of", xmlName(xmlNodeTaskInformation), "node encountered:", childName))
			)
		} # end for
	}
	
	.Object
})

# Task Information messages are already written out to the console as part of
# loading in the SO so won't include these here as well
setMethod("getPopulatedSlots", "TaskInformation", function(object) {
	NULL
})

