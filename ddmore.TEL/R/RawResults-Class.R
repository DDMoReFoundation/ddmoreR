#
# TODO determine what the structure of this data should be
#

##############################################################################
#' The RawResults Object Class (S4) 
#' 
#' An object to house pointers to the raw data files.
#' 
#' @slot DataFiles a list, TODO of what?
#' @slot GraphicsFiles a list, TODO of what?
#' 
#' @name RawResults-class
#' @rdname RawResults-class
#' @exportClass RawResults
#' @aliases RawResults
#' @examples
#' rr <- new(Class = "RawResults")
#' print(rr)
#' validObject(rr)
#' 
#' @include StandardOutputObjectXmlParsers.R

setClass("RawResults", 
	# Define the slots
	slots=c(
		DataFiles = "list", 
		GraphicsFiles = "list"
	),
	# Validity Checking Function 
	validity = function(object) {
		stopifnot(class(object@DataFiles) == "list")
		stopifnot(class(object@GraphicsFiles) == "list")
		return(TRUE)
	}
)

#' Initialisation function / Constructor for RawResults S4 class
#' @param .Object new instance of the class
#' @param xmlNodeRawResults XML Node representation of the block
#' @include StandardOutputObjectXmlParsers.R
setMethod("initialize", "RawResults", function(.Object, xmlNodeRawResults = NULL) {
	
	if (!is.null(xmlNodeRawResults)) {
		childNodes <- .getChildNodes(xmlNodeRawResults)
		for (i in seq(along=childNodes)) {
			
			fileType <- names(childNodes[i]) # Only one name
			childNode <- childNodes[[i]]
			objectId <- xmlAttrs(childNode)[['oid']]
			
			# Extract child tags and values as a list with names = tag names and elements = tag values
			childTags = xmlSApply(childNode, xmlValue)
			
			# Add this as an element to the Final Files List 
			if (fileType == "DataFile") {
				if (.NODENAME_EXTERNALFILE %in% names(.getChildNodes(childNode))) {
					externalFileNode <- .getChildNodes(childNode)[[.NODENAME_EXTERNALFILE]]
					objectId <- xmlAttrs(externalFileNode)[['oid']] # objectId is on nested ExternalFile node instead of on the DataFile node itself as for GraphicsFile
					.Object@DataFiles[objectId] <- list(parseExternalFile(externalFileNode))
				} else {
					.Object@DataFiles[objectId] <- list(as.list(childTags))
				}
			} else if (fileType == "GraphicsFile") {
				.Object@GraphicsFiles[objectId] <- list(as.list(childTags))
			} else {
				warning(paste("Unexpected child node", fileType, "encountered on RawResults node, expected: DataFile, GraphicsFile"))
			}
			
		} # end for
	}
	
	.Object
})

# Simplify the indication of the slots that are populated for RawResults object,
# to essentially a yes/no as to whether it has any DataFiles or GraphicsFiles
setMethod("getPopulatedSlots", "RawResults", function(object) {
	if (length(object@DataFiles) + length(object@GraphicsFiles)) list()
})

