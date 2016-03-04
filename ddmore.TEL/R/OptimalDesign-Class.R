
#' The OptimalDesignBlock Object Class (S4) 
#'
#' An object to house all data associated with Optimal Design
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

OptimalDesignBlock <- function(xmlNodeOptimalDesignBlock = NULL, ...) {
    newObj <- new(Class = "OptimalDesignBlock", ...)
	
	if (!is.null(xmlNodeOptimalDesignBlock)) {
		
		spAttrs <- xmlAttrs(xmlNodeOptimalDesignBlock)
		for (spAttrName in names(spAttrs)) {
			if (spAttrName %in% c("blockNumber")) {
				slot(newObj, spAttrName) <- spAttrs[[spAttrName]]
			}
		}
		for (child in .getChildNodes(xmlNodeOptimalDesignBlock)) {
			childName <- xmlName(child)
			if (childName %in% c("FIM", "CovarianceMatrix")) {
				# Matrix (dataframe)
				slot(newObj, childName) <- ParseElement(child)
			}
			else if (childName %in% c("ParameterPrecision", "Criteria", "Tests")) {
				# Table (DataSet object)
				slot(newObj, childName) <- ParseElement(child)
			}
			else if (childName %in% c("SimulatedData", "Design")) {
				slot(newObj, childName) <- xmlValue(.getChildNode(.getChildNodes(child), "path"))
			}
			else {
				warning(paste("Unexpected child node of OptimalDesignBlock node encountered: ", childName))
			}
		} # end for
	}
	
	newObj
}
