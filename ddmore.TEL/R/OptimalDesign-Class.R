
#' The OptimalDesignBlock Object Class (S4) 
#'
#' An object to house all data associated with Optimal Design.
#' 
#' @slot type string
#' @slot blockNumber integer
#' @slot FIM Matrix (i.e. dataframe)
#' @slot CovarianceMatrix Matrix (i.e. dataframe)
#' @slot ParameterPrecision DataSet object
#' @slot Criteria DataSet object
#' @slot Tests DataSet object
#' @slot SimulatedData file path as a string
#' @slot Design file path as a string
#' 
#' @name OptimalDesignBlock-class
#' @rdname OptimalDesignBlock-class
#' @exportClass OptimalDesignBlock
#' @aliases OptimalDesignBlock
#' @examples
#' od <- new(Class = "OptimalDesignBlock")
#' print(od)
#' validObject(od)
#' 
#' @include xmlParsers.R

setClass(Class = "OptimalDesignBlock",
	slots = c( #"type", - on parent OptimalDesign node
		"blockNumber",
		"FIM", "CovarianceMatrix",
		"ParameterPrecision", "Criteria", "Tests",
		"SimulatedData", "Design"),
	prototype = list(
		#type = character(0), - on parent OptimalDesign node
		blockNumber = integer(0),
		FIM = data.frame(), # matrix
		CovarianceMatrix = data.frame(), # matrix
		ParameterPrecision = DataSet(),
		Criteria = DataSet(),
		Tests = DataSet(),
		SimulatedData = character(0),
		Design = character(0)),
	validity = function(object) {
		# TODO implement checking
		return(TRUE)
	}
)

#' Initialisation function / Constructor for OptimalDesignBlock S4 class
#' @param .Object new instance of the class
#' @param xmlNodeOptimalDesignBlock XML Node representation of the block
#' @include xmlParsers.R
setMethod("initialize", "OptimalDesignBlock", function(.Object, xmlNodeOptimalDesignBlock = NULL) {

	if (!is.null(xmlNodeOptimalDesignBlock)) {
		.Object <- .genericParseElements(.Object, xmlNodeOptimalDesignBlock, customParseChildNodeNames = c("SimulatedData", "Design"))

		spAttrs <- xmlAttrs(xmlNodeOptimalDesignBlock)
		# Parse attributes of the OptimalDesignBlock node
		for (spAttrName in names(spAttrs)) {
			if (spAttrName %in% c("blockNumber")) {
				slot(.Object, spAttrName) <- spAttrs[[spAttrName]]
			}
		}
		# Custom parsing of child nodes that aren't simply handled by ParseElement()
		for (child in .getChildNodes(xmlNodeOptimalDesignBlock)) {
			childName <- xmlName(child)
			if (childName %in% c("SimulatedData", "Design")) {
				slot(.Object, childName) <- xmlValue(.getChildNode(.getChildNodes(child), "path"))
			}
		}
	}
	
	.Object
})

