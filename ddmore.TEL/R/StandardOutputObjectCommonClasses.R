
# ========================================================================================== #
# Common class definitions shared between higher-level classes within the SO class structure #
# ========================================================================================== #
#
# Authors: cmusselle, ccampbell, mwise


#' Get Indication of Populated Slots
#' 
#' Return an indication as to the slots of this S4 object that are populated
#' as against being empty / having their default value. This is applied
#' recursively as explained in the details section.
#' 
#' @param object S4 object to query
#' @return list of slots that are considered to be populated from this object
#' 		   and recursively from any nested objects.
#' 
#' @details
#' This generic method traverses the slots of this object and builds up a list of
#' those slots that are populated.
#' Each slot is processed according to its type / class, as follows:
#' \itemize{
#'  \item{DataSet:}
#' 		{Does the \code{description} slot contain some header rows => slot name added}
#'  \item{DataSetDistribution:}
#' 		{Is the \code{distributionName} slot populated => slot name added}
#'  \item{SimulationDataSet:}
#' 		{Does the \code{description} slot contain some header rows => slot name added}
#'  \item{dataframe:}
#' 		{Does this dataframe contain some rows of data => slot name added}
#'  \item{S4 Object:}
#' 		{Recursively call \code{getPopulatedSlots()} on the nested object.
#' 		 If null then skip. If returned list of populated "sub-"slots is empty
#' 		 then add the slot name. If returned list is non-empty then add each
#' 		 of the "sub-"slot names to the list, prefixed with the slot name and "::".}
#'  \item{List of primitives:}
#' 		{If list is empty then skip. If non-empty then add the slot name.}
#'  \item{List of S4 Objects:}
#' 		{If list is empty then skip. Otherwise recursively call \code{getPopulatedSlots()}
#' 		 on each of the items in the list. For each, if returned list of
#' 		 populated "sub-"slots is empty then add the slot name + "[" +
#' 		 list element number + "]"; if returned list of populated
#' 		 "sub-" slots is non-empty then for each add the slot name + "[" +
#' 		 list element number + "]" + "::" + "sub-"slot name.}
#' }
#' 
#' Specific S4 classes can override this method to provide their own implementation,
#' e.g. to prematurely truncate the recursion of deeply nested object sub-trees.
#' 
#' @examples
#' cat(paste(ddmore:::getPopulatedSlots(so), collapse="\n"))
#' # Returns:
#' #ToolSettings
#' #RawResults
#' #Estimation::PopulationEstimates::MLE
#' #Estimation::PopulationEstimates::Bayesian
#' #Estimation::PopulationEstimates::OtherMethod[1]
#' #Estimation::PrecisionPopulationEstimates::MLE
#' #Estimation::PrecisionPopulationEstimates::Bayesian

setGeneric("getPopulatedSlots", function(object, ...) {
			
	res <- NULL
	
	for (slotName in slotNames(object)) {
		nestedObj <- slot(object, slotName)
		
		if (class(nestedObj) == "list") {
			if (length(nestedObj) > 0) {
				if (isS4(nestedObj[[1]])) {
					res <- append(res, paste0(slotName, getPopulatedSlots(nestedObj)))
				} else { # A list of primitives
					res <- append(res, slotName)
				}
			}
		}
		else if (class(nestedObj) == "data.frame") {
			if (nrow(nestedObj) > 0) {
				res <- append(res, slotName)
			}
		}
		else if (isS4(nestedObj)) {
			populatedSlotsNestedObj <- getPopulatedSlots(nestedObj)
			if (!is.null(populatedSlotsNestedObj)) {
				if (is.empty(populatedSlotsNestedObj)) {
					res <- append(res, slotName)
				} else {
					res <- append(res, paste0(slotName, "::", populatedSlotsNestedObj))
				}
			}
		}
	}
	
	res
})

# See documentation on the generic function.
setMethod("getPopulatedSlots", "list", function(object) {
	res <- NULL
	
	for (i in seq_along(object)) {
		populatedSlotsNestedObj <- getPopulatedSlots(object[[i]])
		if (!is.null(populatedSlotsNestedObj)) {
			if (is.empty(populatedSlotsNestedObj)) {
				res <- append(res, paste0("[", i, "]"))
			} else {
				res <- append(res, paste0("[", i, "]::", populatedSlotsNestedObj))
			}
		}
	}
	res
})



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
		description = data.frame(), # data.frame(Col1 = c("1", "unknown", "real"))
		data = matrix(ncol = 0, nrow = 0) # matrix(data = NA_real_, ncol = 1, nrow = 1)
	),
	validity = function(object) {
		stopifnot(ncol(object@description) == ncol(object@data))
		if (nrow(object@description) > 0) {
			stopifnot(all(rownames(object@description) == c("columnNum", "columnType", "valueType")))
		}
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

# See documentation on the generic function.
setMethod("getPopulatedSlots", "DataSet", function(object) {
	if (nrow(object@description) > 0) list()
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
#' structure, is a \linkS4class{DataSet} class augmented with a distributionName
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

# See documentation on the generic function.
setMethod("getPopulatedSlots", "DataSetDistribution", function(object) {
	if (length(object@distributionName) > 0) list()
})


