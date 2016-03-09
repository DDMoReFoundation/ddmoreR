
# ========================================================================================== #
# Common class definitions shared between higher-level classes within the SO class structure #
# ========================================================================================== #
#
# Authors: cmusselle, ccampbell, mwise


#' The DataSet Object Class (S4)
#'
#' An object to house all data associated with a data set
#' 
#' @slot description dataframe containing column header information for dataset
#' @slot data matrix containing actual data with one column for each column in description
#' 
#' @name DataSet-class
#' @rdname DataSet-class
#' @exportClass DataSet
#' @aliases DataSet
#' @examples
#' dat <- new(Class = "DataSet", 
#'     description = data.frame(A = c("1", "unknown", "real"), B = c("2", "unknown", "real"), 
#'         row.names = c("columnNum", "columnType", "valueType"), stringsAsFactors = FALSE), 
#'     data = matrix(1:10, ncol = 2))
#' print(dat)
#' validObject(dat)

setClass(Class = "DataSet",
	slots = c(
		description = "data.frame",
		data = "matrix"
	),
    prototype = list(
		# TODO: Shouldn't this just be creating an empty data frame / matrix?
		description = data.frame(Col1 = c("1", "unknown", "real")),
		data = matrix(data = NA_real_, ncol = 1, nrow = 1)
	),
	validity = function(object) { 
    	stopifnot(ncol(object@description) == ncol(object@data))
    	stopifnot(all(rownames(object@description) == c("columnNum", "columnType", "valueType")))
    	return(TRUE)
	}
)

#' Initialisation function / Constructor for \linkS4class{DataSet} S4 class
#' 
#' @param .Object new instance of the class
#' @slot description dataframe containing column header information for dataset
#' @slot data matrix containing actual data with one column for each column in description
#' 
#' @include StandardOutputObjectXmlParsers.R
setMethod("initialize", "DataSet", function(.Object, description = NULL, data = NULL) {
	
	if (!is.null(data)) {
		.Object@description <- description
		.Object@data <- data
	}
	.Object
})


#' Coerce \linkS4class{DataSet} object to data.frame
#' 
#' Make Data useable in R session
#' @param x DataSet object
#' @param dots
#' @return data.frame
#' @examples
#' dat <- new(Class = "DataSet", 
#'     description = data.frame(A = c("1", "unknown", "real"), B = c("2", "unknown", "real"), 
#'         row.names = c("columnNum", "columnType", "valueType"), stringsAsFactors = FALSE), 
#'     data = matrix(1:10, ncol = 2))
#' as.data.frame(x = dat)

setGeneric("as.data.frame")

setMethod(f = "as.data.frame", 
	signature = "DataSet", 
	definition = function(x, ...) {
		validObject(x)
		out <- as.data.frame(x@data, stringsAsFactors = FALSE)
		
		# update order
		index <- as.numeric(unlist(x@description["columnNum", , drop = FALSE]))
		out <- out[, index, drop = FALSE]
		colnames(out) <- colnames(x@description)
		for (i in seq_along(x@description)) {
    		# manually parse valueType
    		asType <- switch(x@description["valueType", i],
        			"real" = as.numeric, 
        			"Real" = as.numeric, 
        			"integer" = as.integer, 
        			"Int" = as.integer, 
        			"Char" = as.character, 
        			"String" = as.character, 
        			"Text" = as.character, {
            			warning("type not recognized in DataSet, treating as Real")
            			as.numeric
        			})
    		out[[i]] <- asType(out[[i]])
		}
		return(out)
	})


#' The DataSetDistribution Object Class (S4)
#'
#' An object to house all data associated with a Sample/Distribution,
#' which for the purposes of representation within the SO class
#' structure, is a \linkS4class{DataSetj} class augmented with a distributionName
#' and optional distributionParameters.
#' 
#' @slot distributionName the name of the distribution
#' @slot distributionParameters named list of the parameters, the list names
#' 								being the parameter names and the values in
#' 								the list being the parameter values
#' @slot description (Optional) dataframe containing column header information for dataset
#' @slot data (Optional) matrix containing actual data with one column for each column in description
#' 
#' @name DataSetDistribution-class
#' @rdname DataSetDistribution-class
#' @exportClass DataSetDistribution
#' @aliases DataSetDistribution
#' @examples
#' dsd <- new(Class = "DataSetDistribution") 
#' print(dsd)
#' validObject(dsd)

setClass(Class = "DataSetDistribution",
	contains = "DataSet", # superclass
	slots = c(
		distributionName = "character",
		distributionParameters = "list"
	),
	validity = function(object) {
		# TODO implement this checking
    	return(TRUE)
	}
)

#' Initialisation function / Constructor for \linkS4class{DataSetDistribution} S4 class,
#' subclass of \linkS4class{DataSet} S4 class.
#' 
#' @param .Object new instance of the class
#' @param distributionName the name of the distribution
#' @param distributionParameters (Optional) named list of the distributions parameters, the
#' 								 list names being the parameter names and the values in the
#' 								 list being the parameter values
#' @slot description (Optional) dataframe containing column header information for dataset
#' @slot data (Optional) matrix containing actual data with one column for each column in description
#' 
#' @include StandardOutputObjectXmlParsers.R
setMethod("initialize", "DataSetDistribution",
		function(.Object, distributionName, distributionParameters = NULL, description = NULL, data = NULL) {
	
	.Object@distributionName <- distributionName
	if (!is.null(distributionParameters)) {
		.Object@distributionParameters <- distributionParameters
	}
	
	callNextMethod(.Object, description, data)
})

