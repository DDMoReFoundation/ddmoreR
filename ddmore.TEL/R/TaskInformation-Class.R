
#' The TaskInformation Object Class (S4) 
#'
#' An object to house all data associated with Task Information,
#' primarily messages emitted by the target tool.
#' 
#' @slot ErrorMessages list
#' @slot WarningMessages list
#' @slot TerminationMessages list
#' @slot InfoMessages list
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
#' @include xmlParsers.R
#' @include utils.R

setClass(Class = "TaskInformation",
	slots = c("ErrorMessages", "WarningMessages", "TerminationMessages", "InformationMessages",
			  "OutputFilePath", "RunTime", "NumberChains", "NumberIterations"),
	prototype = list(
		ErrorMessages = list(),
		WarningMessages = list(),
		TerminationMessages = list(),
		InformationMessages = list(),
		OutputFilePath = character(0),
		RunTime = numeric(0),
		NumberChains = integer(0),
		NumberIterations = integer(0)
	),
	validity = function(object) {
		# TODO implement checking
		return(TRUE)
	}
)

#' Initialisation function / Constructor for TaskInformation S4 class
#' @param .Object new instance of the class
#' @param xmlNodeTaskInformation XML Node representation of the block
#' @include xmlParsers.R

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
						slot(.Object, slotName) <- c(slot(.Object, slotName), list(messageContent))
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


