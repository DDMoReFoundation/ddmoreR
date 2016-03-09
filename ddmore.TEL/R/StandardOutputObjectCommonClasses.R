
# ========================================================================================== #
# Common class definitions shared between higher-level classes within the SO class structure #
# ========================================================================================== #
#
# Authors: cmusselle, ccampbell, mwise


#' The DataSet Object Class (S4)
#'
#' An object to house all data associated with a data set
#' 
#' @slot description header information for dataset
#' @slot data matrix information with one column for each column in description
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
    	slots = c("description", "data"), 
    	prototype = list(description = data.frame(Col1 = c("1", "unknown", "real")), 
        		data = matrix(data = NA_real_, ncol = 1, nrow = 1)), 
    	validity = function(object) { 
        	stopifnot(ncol(object@description) == ncol(object@data))
        	stopifnot(all(rownames(object@description) == c("columnNum", "columnType", "valueType")))
        	return(TRUE) })

#' Instantiate a new object of class DataSet.
#' 
#' @param data optional list containing "description" dataframe, "data" dataframe to populate with
#' @param dots arguments to \code{new}
#' @return object of class DataSet
#' @export

DataSet <- function(data = NULL, ...) {
    newObj <- new(Class = "DataSet", ...)
	
	if (!is.null(data)) {
		newObj@description <- data$description
		newObj@data <- data$data
	}
	
	newObj
}


#' Coerce DataSet object to data.frame
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
#' @slot description (Optional) header information for dataset
#' @slot data (Optional) matrix information with one column for each column in description
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
	slots = c("distributionName", "distributionParameters"),
	prototype = list(
		distributionName = character(0),
		distributionParameters = list()
		# Leave these slots from the superclass as null since they are optional for Distribution
		#description = 
		#data = 
	),
	validity = function(object) {
		# TODO implement this checking
    	return(TRUE)
	}
)

#' Instantiate a new object of class DataSetDistribution.
#' 
#' @param distributionName the name of the distribution
#' @param distributionParameters named list of the parameters, the list names
#'  							 being the parameter names and the values in
#' 	 							 the list being the parameter values
#' @param data optional list containing "description" dataframe, "data" dataframe to populate with
#' @param dots arguments to \code{new}
#' @return object of class DataSetDistribution
#' @export

DataSetDistribution <- function(distributionName, distributionParameters = NULL, data = NULL, ...) {
    newObj <- new(Class = "DataSetDistribution", ...)
	
	newObj@distributionName <- distributionName
	if (!is.null(distributionParameters)) {
		newObj@distributionParameters <- distributionParameters
	}
	if (!is.null(data)) {
		newObj@description <- data$description
		newObj@data <- data$data
	}
	
	newObj
}

