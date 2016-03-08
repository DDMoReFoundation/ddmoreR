# ======================================= #
# Xml Parsers for sections of the PharmML #
# ======================================= #
# 
# This file contains all code for the low level xml parsing 
# functions associated with loading the StardardOutputObject 
# from the PharmML. 
#
# Author: cmusselle, ccampbell, mwise

.NODENAMES_NAMESPACE_PREFIX = "ds:"
.NODENAME_COMMENT <- "comment"
.NODENAME_DEFINITION <- "Definition"
.NODENAME_EXTERNALFILE <- "ExternalFile"
.NODENAME_TABLE <- "Table"
.NODENAME_MATRIX <- "Matrix"
.NODENAME_ROWNAMES <- "RowNames"
.NODENAME_COLNAMES <- "ColumnNames"
.NODENAME_MATRIXROW <- "MatrixRow"

.DATASET_DESCRIPTION <- "description"
.DATASET_DATA <- "data"


# ================== #
# Data Block Parsers #
# ================== #


# Internal function that calls xmlChildren and then strips out any comment nodes. 
.getChildNodes <- function(parentNode) {
	childNodesList <- xmlChildren(parentNode)
	childNodesList <- childNodesList[names(childNodesList) != .NODENAME_COMMENT]
	childNodesList
}

# Internal function to retrieve a named node from a list of XML child nodes,
# taking into account the "ds:" namespace prefix that may or may not be present.
# Returns NULL if no matching node is present.
.getChildNode <- function(nodeList, nodeName) {
	node <- nodeList[[paste0(.NODENAMES_NAMESPACE_PREFIX, nodeName)]] # Try with namespace prefix
	if (is.null(node)) {
      node <- nodeList[[nodeName]] # Try without namespace prefix
	}
	node
}

# Each custom S4 class's initialize function is intended to delegate to this function
# that will parse the child nodes of the provided XML node into the corresponding
# slots of the provided new instance of the S4 class.
# Note that child nodes that can't be simply parsed by the ParseElement() function,
# need to be explicitly parsed in the initialize function instead.
.genericParseElements <- function(newObj, xmlNode = NULL, customParseChildNodeNames = c()) {
	
	if (!is.null(xmlNode)) {
		for (child in .getChildNodes(xmlNode)) {
			childName <- xmlName(child)
			if (childName %in% customParseChildNodeNames) {
				# Skip - custom parsing to be done by the class's initialize function instead
			} else if (childName %in% slotNames(newObj)) {
				slot(newObj, childName) <- ParseElement(child)
			} else {
				warning(paste("Unexpected child node of", xmlName(xmlNode), "node encountered:", childName))
			}
		} # end for
	}
	
	newObj
}

#.genericParseElements <- function(xmlNode, newS4Obj) {
#	slotNames <- slotNames(newS4Obj)
#	for (child in .getChildNodes(xmlNode)) {
#		childName <- gsub(.NODENAMES_NAMESPACE_PREFIX, '', xmlName(child))
#		if (childName %in% slotNames) {
#			slotType <- class(slot(newS4Obj, childName))
#			switch (slotType,
#				"Matrix"
#			)
#			
#			L <- ParseElement(child)
#			# Table expected - TODO call specific function
#			# TODO: Do this on DataSet() constructor instead
#			slot(newObj, childName)@description <- L$description
#			slot(newObj, childName)@data <- L$data
#		} else {
#			warning(paste("Unexpected child node of ", xmlName(xmlNode),  " node encountered: ", childName))
#		}
#	}
#}

#' @title Parse Element
#'
#' @description Investigates the name of the elements Children and runs the appropriate parser  
#' @param node - XML object
#' @return list
#' @examples
#' dataPath <- system.file("tests", "data", "PharmMLSO", "HandCoded", 
#'     "warfarin_PK_ODE_SO_FULL.xml",  
#'     package = "ddmore")
#' # read file
#' root <- ddmore:::validateAndLoadXMLSOFile(file = dataPath)
#' # SO Block
#' soBlocks <- root[names(root) == "SOBlock"]
#' SOChildren <- xmlChildren(x = soBlocks[[1]])
#' # MLE
#' children <- xmlChildren(x = SOChildren[["Estimation"]][["PopulationEstimates"]])
#' ddmore:::ParseElement(node = children[["MLE"]])

ParseElement <- function(node) {
  
  childNodes <- .getChildNodes(node)
  childNames <- names(childNodes)

  parsed <- NULL
  
  if (length(childNames) == 1) {
	if (childNames == .NODENAME_MATRIX) {
		# Parse Node as a matrix, is returned as a dataframe
		parsed <- ParseMatrix(.getChildNode(childNodes, .NODENAME_MATRIX))
	} else if (childNames == .NODENAME_EXTERNALFILE) {
		# Load data from external file
		parsed <- DataSet( # new object out of the data
			ParseExternalFile(.getChildNode(childNodes, .NODENAME_EXTERNALFILE))
		)
	}
	else {
		warning("Expected Matrix or ExternalFile block in ParseElement, but found ",
			paste(childNames, collapse = " "))
	}
  }
  else if (length(childNames) == 2) {
	combinedChildNames <- paste(sort(childNames), collapse = "|")
	if (combinedChildNames == paste(c(.NODENAME_DEFINITION, .NODENAME_EXTERNALFILE), collapse="|")) {
		# Load data from external file
		parsed <- DataSet( # new object out of the data
			ParseDataSetExternalFile(.getChildNode(childNodes, .NODENAME_DEFINITION), .getChildNode(childNodes, .NODENAME_EXTERNALFILE))
		)
	}
	else if (combinedChildNames == paste(c(.NODENAME_DEFINITION, .NODENAME_TABLE), collapse="|")) {
		# Load data from inline XML
		parsed <- DataSet( # create new object out of the data
			ParseDataSetInline(.getChildNode(childNodes, .NODENAME_DEFINITION), .getChildNode(childNodes, .NODENAME_TABLE))
		)
	} else {
	    warning("Expected ExternalFile or Table blocks with Definition in ParseElement, but found ",
			paste(childNames, collapse = " "))
	}
  }
  else {
	warning("Expected 1 or 2 blocks in ParseElement, but found: ", paste(childNames, collapse=", "))
  }
  
  if (is.null(parsed)) {
    stop(paste("Names of child elements not recognised as a parsable object in the SO XML. Element child names are:\n   ", 
      paste(childNames, collapse="\n    ")))
  }
  return(parsed)
}


#' ParseDataSetInline
#'
#' Utility function to parse a DataSet xml structure as it appears in PharmML. 
#'
#' @param definitionNode "Definition" xmlNode object
#' @param tableNode "Table" xmlNode object
#'
#' @return Returns a list with two named elements: \code{description}, which holds all 
#' the meta data about the columns in a data frame; \code{data}, which holds the
#' actual values in a dataframe.
#'
ParseDataSetInline <- function(definitionNode, tableNode) {

  rowTagName <- "Row"
  columnTagName <- "Column"

  
  # Extract all column Information and store in a data frame
  columnInfo <- as.data.frame(xmlSApply(definitionNode, FUN = function(x) list(
      xmlGetAttr(x, name="columnNum"), 
      xmlGetAttr(x, name="columnType"),
      xmlGetAttr(x, name="valueType"),
      xmlGetAttr(x, name="columnId")
      )
  ))
      
  # Filter out non Columns tags  
  columnInfo <- columnInfo[, names(columnInfo) == columnTagName]
  
  # Rename column headers to column ID
  names(columnInfo) <- unlist(columnInfo[4,])
  columnInfo <- columnInfo[-4, ]
  # Rename rows to lable column info
  rownames(columnInfo) <- c("columnNum", "columnType", "valueType")
  
  # Get all Table Row elements
  rowList <- .getChildNodes(tableNode)
    
  # List of values
  rowData <- lapply(rowList, FUN = function(r) {
	  res <- xmlSApply(X = r, FUN = xmlValue)
	  res[names(res)!=.NODENAME_COMMENT]
  })
  
  # Convert to data frame 
  temp <- Reduce(f = rbind, x = rowData)
  if (length(rowData) == 1) {
    # reduced list is a character vector
    datf <- t(data.frame(temp))
    rownames(datf) <- NULL
    colnames(datf) <- names(columnInfo)
  } else {
    dimnames(temp) <- NULL
    # reduced list is a character matrix
    datf <- data.frame(temp)
	if (length(colnames(datf)) != length(columnInfo)) {
		stop(paste0("Mismatch in number of columns in column header information versus actual row data encountered while parsing inline data set (i.e. pair of <Definition>, <Table> blocks).\n",
			"  Columns defined in header: ", paste(colnames(columnInfo), collapse=", "), "\n",
			"  Data: ", length(rownames(datf)), "x", length(colnames(datf)), " dataframe"))
	}
    colnames(datf) <- names(columnInfo)
  }
  return(list(description = columnInfo, data = datf))
}


#' ParseDataSetExternalFile
#'
#' Utility function to parse a DataSet xml structure by loading in data from the referenced external file.
#'
#' @param definitionNode "Definition" xmlNode object
#' @param externalFileNode "ExternalFile" xmlNode object
#'
#' @return Returns a list with two named elements: \code{description}, which holds all 
#' the meta data about the columns in a data frame; \code{data}, which holds the
#' actual values in a dataframe.
#'
ParseDataSetExternalFile <- function(definitionNode, externalFileNode) {

  # Extract all column Information and store in a data frame
  columnInfo = as.data.frame(xmlSApply(definitionNode, FUN = function(x) list(
  		xmlGetAttr(x, name="columnNum"), 
    	xmlGetAttr(x, name="columnType"),
    	xmlGetAttr(x, name="valueType"),
    	xmlGetAttr(x, name="columnId")
    	)
  ))
      
  # Filter out non Columns tags  
  # columnInfo = columnInfo[, names(columnInfo) == columnRef]
  
  # Rename column headers to column ID
  names(columnInfo) <- unlist(columnInfo[4,])
  columnInfo = columnInfo[-4, ]
  # Rename rows to lable column info
  rownames(columnInfo) <- c("columnNum", "columnType", "valueType")
  
  # Get all ExternalFile elements
  datf <- ParseExternalFile(externalFileNode)

  if (!is.empty(colnames(datf))) {
  	colnames(datf) <- names(columnInfo)
  }

  return(list(description=columnInfo, data=datf))
}


#' ParseMatrix
#' 
#' Utility function to parse a Matrix xml structure as it appears in PharmML. 
#' 
#' @param matrixNode The Matrix xmlNode object that contains the decendant tags:
#'   ct:RowNames, ct:ColumnNames,  and multiple entries of ct:MatrixRow
#'
#' @return Returns a dataframe with row and column names taken from the appropriate 
#' tags in the Matrix structure. 
#'
ParseMatrix <- function(matrixNode) {
  
  if ( (length(matrixNode[[.NODENAME_ROWNAMES]]) == 0) || (length(matrixNode[[.NODENAME_COLNAMES]]) == 0) ) {
	  warning("No RowNames or ColumnNames found for Matrix element. Skipping...")
	  return(NULL)
  }
	
  # Get rownames of matrix 
  matrixRowNames <- xmlSApply(matrixNode[[.NODENAME_ROWNAMES]], xmlValue)
  
  # Get colnames of matrix 
  matrixColumnNames <- xmlSApply(matrixNode[[.NODENAME_COLNAMES]], xmlValue)
  
  # Get all Matrix Rows that contain data
  matrixDataRows <- matrixNode[names(matrixNode) == .NODENAME_MATRIXROW]

  if (length(matrixDataRows) == 0) {
	  warning("No MatrixRows found for Matrix element. Skipping...")
	  return(NULL)
  }
  
  # Extract the value element of each element
  output.matrix.transposed <- sapply(matrixDataRows, FUN=function(x) xmlApply(x, xmlValue))
  output.matrix <- t(output.matrix.transposed)
  
  # Convert to Data Frame
  datf = as.data.frame(output.matrix)
  
  # Update row and column names
  if (nrow(datf) != length(matrixRowNames)) {
    warning("Number of row names given does not match matrix dimensions. Row names ignored.")
    rownames(datf) <- NULL
  } else {
    rownames(datf) <- matrixRowNames
  }
  if (ncol(datf) != length(matrixColumnNames)) {
    warning("Number of column names given does not match matrix dimensions. Column names ignored.")
    colnames(datf) <- NULL
  } else {
    colnames(datf) <- matrixColumnNames
  }
  
  return(datf)
}


#' ParseExternalFile
#'
#' Utility function to parse and load in an external file to the PharmML, defined in the 
#' ExternalFile node passed in. 
#'
#' @param externalFileNode The parent xmlNode object that contains three decendant tags,
#'   path, format and delimiter, where path is mandatory but format and delimiter are optional
#'
#' @return Returns a list with two named elements: \code{description}, which holds all 
#' the meta data about the columns in a data frame; \code{data}, which holds the
#' actual values in a dataframe.
#'
ParseExternalFile <- function(externalFileNode) {
	
  externalFileAttrs <- xmlApply(externalFileNode, xmlValue)
  
  path <- externalFileAttrs[["path"]]
  format <- externalFileAttrs[["format"]]
  delimiter <- externalFileAttrs[["delimiter"]]
  
  if (is.null(path)) {
	  stop("ExternalFile node must have the following mandatory attributes: path; optional attributes are: format, delimiter")
  }

  if (!is.null(delimiter) && delimiter != "COMMA") {
    stop("Comma is the only delimiter currently supported by ExternalFile parsers.")
  }

  if (file.exists(path)) { # TODO: Fail if data file is not present?
	datf <- read.csv(path, sep=",", na.strings=".", header=T)
  } else {
	warning(paste("External data file path resolved to", normalizePath(path), "but this file does not exist."))
	return(data.frame())
  }
  
  return(datf)
}


#' @title ParseDistribution
#'
#' @description Parse a distribution element in the PharmML SO strucutre
#' 
#' @param Node XML object
#' @return Return a list of two elements: name - the name of the distribution, parameters - 
#' a list of the parameter values. 
#' 
ParseDistribution <- function(Node) {
	# TODO Rewrite this for SO v0.3

  subChildren <- .getChildNodes(Node)
  
  for (subChild in subChildren){
    
    if (grepl("distribution", tolower(xmlName(subChild)))) {
      
      # Parse the distribution Tag
      distributionName = xmlName(subChild)
      parameterList = xmlApply(subChild, xmlValue)
    
    }
  }

  distList <- list(name = distributionName, parameters = parameterList)
  return(distList)
}

