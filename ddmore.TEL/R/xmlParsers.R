# ======================================= #
# Xml Parsers for sections of the PharmML #
# ======================================= #
# 
# This file contains all code for the low level xml parsing 
# functions associated with loading the StardardOutputObject 
# from the PharmML. 
#
# Author: cmusselle

.NODENAMES_NAMESPACE_PREFIX = "ds:"
.NODENAME_COMMENT <- "comment"
.NODENAME_DEFINITION <- "Definition"
.NODENAME_EXTERNALFILE <- "ExternalFile"
.NODENAME_TABLE <- "Table"
.NODENAME_MATRIX <- "Matrix"
.NODENAME_ROWNAMES <- "RowNames"
.NODENAME_COLNAMES <- "ColumnNames"
.NODENAME_MATRIXROW <- "MatrixRow"
.NODENAME_DATAFILE <- "DataFile"
.NODENAME_GRAPHICSFILE <- "GraphicsFile"
.NODENAME_MLE = "MLE"
.NODENAME_BAYESIAN = "Bayesian"
.NODENAME_BOOTSTRAP = "Bootstrap"
.NODENAME_LLP <- "LLP"
.NODENAME_SIR <- "SIR"
.NODENAME_MULTIDIMLLP <- "MultiDimLLP"
.NODENAME_ESTIMATES <- "Estimates"
.NODENAME_RANDOMEFFECTS <- "RandomEffects"
.NODENAME_ETASHRINKAGE <- "EtaShrinkage"
.NODENAME_RESIDUALTABLE <- "ResidualTable"
.NODENAME_EPSSHRINKAGE <- "EpsShrinkage"
.NODENAME_OTHERMETHOD <- "OtherMethod"


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
#' ddmore:::ParseElement(Node = children[["MLE"]])

ParseElement <- function(node) {
  
  childNodes <- .getChildNodes(node)
  childNames <- names(childNodes)

  parsed <- NULL
  
  if (length(childNames) == 1) {
	if (childNames == .NODENAME_MATRIX) {
		# Parse Node as a matrix 
		parsed <- ParseMatrix(.getChildNode(childNodes, .NODENAME_MATRIX))		  
	} else if (childNames == .NODENAME_EXTERNALFILE) {
		# Load data from external file
		parsed <- ParseExternalFile(.getChildNode(childNodes, .NODENAME_EXTERNALFILE))		  
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
		parsed <- ParseDataSetExternalFile(.getChildNode(childNodes, .NODENAME_DEFINITION), .getChildNode(childNodes, .NODENAME_EXTERNALFILE))
	}
	else if (combinedChildNames == paste(c(.NODENAME_DEFINITION, .NODENAME_TABLE), collapse="|")) {
		# Load data from inline XML
		parsed <- ParseDataSetInline(.getChildNode(childNodes, .NODENAME_DEFINITION), .getChildNode(childNodes, .NODENAME_TABLE))
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
  rowData <- lapply(X = rowList, FUN = function(x) { xmlSApply(X = x, FUN = xmlValue) })
  
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
    colnames(datf) <- names(columnInfo)
  }
  return(list(description = columnInfo, data = datf))
}

#' ParseDataSetExternalFile
#'
#' Utility function to parse a DataSet xml structure as it appears in PharmML. 

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



# =============== #
# Section Parsers #
# =============== #
#
# Low level parsers specific to certain sections of the xml
# 
# Functions named after the section of pharmML they parse.
#

ParseToolSettings <- function(SOObject, ToolSettingsNode) {  
  # Extract child tags and values as 
  # a list with names = tag names and elements = tag values
  tempList <- xmlApply(ToolSettingsNode, 
    FUN = function(x) xmlName(x) = xmlValue(x)) 
  
  # Strip namespace parts in child element
  # DEPRICATED: Used to adress namespace issue with Xpath. 
  # Xpath expressions no londer used. Can be removed following testing. 
  #newNames = strsplit(names(tempList), ":")
  #newNames = sapply(newNames, FUN = function(x)  x[[2]] )
  #names(tempList) <- newNames
  
  SOObject@ToolSettings <- tempList
  return(SOObject)
}

ParseRawResults <- function(SOObject, rawResultsNode) {

	DataFileTempList <- list()
	GraphicsFileTempList <- list()
	
	childNodes <- .getChildNodes(rawResultsNode)
	for (i in seq(along=childNodes)) {
		
		fileType <- names(childNodes[i]) # Only one name
		childNode <- childNodes[[i]]
		objectId <- xmlAttrs(childNode)[['oid']]
		
		# Extract child tags and values as a list with names = tag names and elements = tag values
		childTags = xmlSApply(childNode, xmlValue)
		
		# Add this as an element to the Final Files List 
		if (fileType == .NODENAME_DATAFILE) {
			if (.NODENAME_EXTERNALFILE %in% names(.getChildNodes(childNode))) {
				externalFileNode <- .getChildNodes(childNode)[[.NODENAME_EXTERNALFILE]]
				objectId <- xmlAttrs(externalFileNode)[['oid']] # objectId is on nested ExternalFile node instead of on the DataFile node itself as for GraphicsFile
				DataFileTempList[objectId] <- list(ParseExternalFile(externalFileNode))
			} else {
				DataFileTempList[objectId] <- list(as.list(childTags))
			}
		} else if (fileType == .NODENAME_GRAPHICSFILE) {
			GraphicsFileTempList[objectId] <- list(as.list(childTags))
		} else {
			warning(paste("Unexpected child node", fileType, "encountered on RawResults node, expected: DataFile, GraphicsFile"))
		}
		
	}
	
	# Assign Result to Files slot of RawResults
	SOObject@RawResults@DataFiles <- DataFileTempList
	SOObject@RawResults@GraphicsFiles <- GraphicsFileTempList
	
	return(SOObject)	
}


# =========================== #
# Parsers for Estimation Slot #
# =========================== #

ParsePopulationEstimates <- function(SOObject, PopulationEstimatesNode) {
  
    STUB_DATASET <- list(description = NULL, data = NULL)
	
	# Iterate over Child nodes, updating SO if appropriate element is present
	for (child in .getChildNodes(PopulationEstimatesNode)) {
		childNodeName <- xmlName(child)
		
		if (childNodeName == .NODENAME_MLE) {
            # Parse XML DataSet Structure	  
            L <- ParseElement(child)
            # Update SO Object Slot
            SOObject@Estimation@PopulationEstimates[[.NODENAME_MLE]] <- L[c(.DATASET_DESCRIPTION, .DATASET_DATA)]
		}
		
		else if (childNodeName == .NODENAME_BAYESIAN) {
            # Fetch Children of Node
            BayesianChildren <- .getChildNodes(child)
            # Parse XML DataSet Structure and update SO
            for (BChild in c("PosteriorMean", "PosteriorMedian", "PosteriorMode")) {
                if (BChild %in% names(BayesianChildren)) {
                    L <- ParseElement(BayesianChildren[[BChild]])
                } else {
                    L <- STUB_DATASET
                }
                SOObject@Estimation@PopulationEstimates[[.NODENAME_BAYESIAN]][[BChild]] <- 
                    L[c(.DATASET_DESCRIPTION, .DATASET_DATA)]
            }
		}
		
		else if (childNodeName == .NODENAME_OTHERMETHOD) {
            if (!"method" %in% names(xmlAttrs(child))) {
                warning("Attribute \"method\" expected on PopulationEstimates::OtherMethod sub-block (since v0.3)")
            } else {
				method <- xmlAttrs(child)[["method"]]
				otherMethodChildNodes <- .getChildNodes(child)
                # Parse XML DataSet Structure and update SO 
				if (method == .NODENAME_BOOTSTRAP) {
                    for (BChild in c("Mean", "Median")) {
                        if (BChild %in% names(otherMethodChildNodes)) {
                            L <- ParseElement(otherMethodChildNodes[[BChild]])
                        } else {
                            L <- STUB_DATASET
                        }
                        SOObject@Estimation@PopulationEstimates[[.NODENAME_OTHERMETHOD]][[.NODENAME_BOOTSTRAP]][[BChild]] <- 
                            L[c(.DATASET_DESCRIPTION, .DATASET_DATA)]
                    }
                }
    			else if (method == .NODENAME_LLP) {
                    warning("LLP not implemented for PopulationEstimates") 
                    SOObject@Estimation@PopulationEstimates[[.NODENAME_OTHERMETHOD]][[.NODENAME_LLP]] <- STUB_DATASET
                }
				else if (method == .NODENAME_SIR) {
                    warning("SIR not implemented for PopulationEstimates")
                    SOObject@Estimation@PopulationEstimates[[.NODENAME_OTHERMETHOD]][[.NODENAME_SIR]] <- STUB_DATASET
                }
				else if (method == .NODENAME_MULTIDIMLLP) {
                    warning("MultiDimLLP not implemented for PopulationEstimates")
                    SOObject@Estimation@PopulationEstimates[[.NODENAME_OTHERMETHOD]][[.NODENAME_MULTIDIMLLP]] <- STUB_DATASET
                }
				else {
					warning(paste("OtherMethod node encountered with unsupported method=", method, "on PopulationEstimates node, expected: Bootstrap, LLP, SIR, MultiDimLLP"))
				}
            }
        }
		
		else {
			warning(paste("Unexpected child node", childNodeName, "encountered on PopulationEstimates, expected: MLE, Bayesian, OtherMethod"))
		}
	}
	
    return(SOObject)
}

ParsePrecisionPopulationEstimates <- function(SOObject, PrecisionPopulationEstimatesNode) {
  
  # Get list of Child Nodes
  children = xmlChildren(PrecisionPopulationEstimatesNode)
  # Iterate over Child nodes, updating SO if appropriate element is present 
  for (child in children){
    
    switch(xmlName(child),
        "MLE" = {
          MLEChildren = xmlChildren(child)
          for (MLEChild in MLEChildren){
            
            if (xmlName(MLEChild) == "FIM"){
              # if FIM is present, parse matrix as data frame to SO {
                datf = ParseElement(MLEChild)
                SOObject@Estimation@PrecisionPopulationEstimates[["MLE"]][["FIM"]] = datf 
            
            } else if(xmlName(MLEChild) == "CovarianceMatrix") {
              # if CovarianceMatrix is present, parse matrix as data frame to SO 
              datf = ParseElement(MLEChild)
              SOObject@Estimation@PrecisionPopulationEstimates[["MLE"]][["CovarianceMatrix"]] = datf
            
            } else if(xmlName(MLEChild) == "CorrelationMatrix") {
              # if CorrelationMatrix is present, parse matrix as data frame to SO 
              datf = ParseElement(MLEChild)
              SOObject@Estimation@PrecisionPopulationEstimates[["MLE"]][["CorrelationMatrix"]] = datf
            
            } else if(xmlName(MLEChild) == "StandardError") {
              # if StandardError is present, parse DataSet as data frame to SO 
              L = ParseElement(MLEChild)
              # Update SO Object Slot
              SOObject@Estimation@PrecisionPopulationEstimates[["MLE"]][["StandardError"]] = list(
                              description=L$description, 
                              data=L$data)
            } else if(xmlName(MLEChild) == "RelativeStandardError") {
              # if RelativeStandardError is present, parse DataSet as data frame to SO 
              L = ParseElement(MLEChild)
              # Update SO Object Slot
              SOObject@Estimation@PrecisionPopulationEstimates[["MLE"]][["RelativeStandardError"]] = list(
                              description=L$description, 
                              data=L$data)

            } else if(xmlName(MLEChild) == "AsymptoticCI") {
              # if AsymptoticCI is present, parse DataSet as data frame to SO 
              L = ParseElement(MLEChild)
              # Update SO Object Slot
              SOObject@Estimation@PrecisionPopulationEstimates[["MLE"]][["AsymptoticCI"]] = list(
                              description=L$description, 
                              data=L$data)
            }
          }
        },
        "Bayesian" = {
          # Fetch Children of Node
          BayesianChildren = xmlChildren(child)
          # Parse XMl DataSet Structure and update SO 
          for (BayesianChild in BayesianChildren) {

            if (xmlName(BayesianChild) == "StandardDeviationPosterior"){
              L = ParseElement(BayesianChild)
              SOObject@Estimation@PrecisionPopulationEstimates[["Bayesian"]][["StandardDeviationPosterior"]] = list(
                                  description=L$description, 
                                  data=L$data)
            } else if (xmlName(BayesianChild) == "PosteriorDistribution"){
              # TODO check how should be implemented for v0.3
              distList = ParseDistribution(BayesianChild)
              SOObject@Estimation@PrecisionIndividualEstimates <- list(
                  # StandardDeviation (needed?)
                  # EstimatesDistribution (renamed PosteriorDistribution)
                  # PercentilesCI (needed?)
                  EstimatesDistribution = distList
                  )
            } else if (xmlName(BayesianChild) == "PercentilesCI"){
              L <- ParseElement(BayesianChild)
              SOObject@Estimation@PrecisionPopulationEstimates[["Bayesian"]][["PercentilesCI"]] <- list(
                                  description=L$description, 
                                  data=L$data)
            }
          }
        }, 
        "OtherMethod" = {
            OtherMethodChildren <- xmlChildren(child)
            
            if (!"method" %in% names(attributes(OtherMethodChildren))) {
                    warning("malformed XML in ParsePrecisionPopulationEstimates\n",
                        "method attribute expected in OtherMethod sub-block (since v0.3)\n", 
                        "block ", xmlName(child), " ignored by ParsePrecisionPopulationEstimates")
            } else {
                switch(attributes(OtherMethodChildren)[["method"]],
                    "Bootstrap" = {
                      # Fetch Children of Node
                      BootstrapChildren <- xmlChildren(child)
                      # Parse XMl DataSet Structure and update SO 
                      for (BootstrapChild in BootstrapChildren) {
                        
                        if (xmlName(BootstrapChild) == "PrecisionEstimates"){
                          L <- ParseElement(BootstrapChild)
                          SOObject@Estimation@PrecisionPopulationEstimates[["Bootstrap"]][["PrecisionEstimates"]] <- list(
                              description=L$description, 
                              data=L$data)
                        } else if (xmlName(BootstrapChild) == "Percentiles"){
                          L <- ParseElement(BootstrapChild)
                          SOObject@Estimation@PrecisionPopulationEstimates[["Bootstrap"]][["Percentiles"]] <- list(
                              description=L$description, 
                              data=L$data)
                        }
                      }
                    }, 
                    "LLP" = {
                      # Fetch Children of Node
                      LLPChildren = xmlChildren(child)
                      # Parse XMl DataSet Structure and update SO 
                      for (LLPChild in LLPChildren) {

                        if (xmlName(LLPChild) == "PrecisionEstimates"){
                          L = ParseElement(LLPChild)
                          SOObject@Estimation@PrecisionPopulationEstimates[["LLP"]][["PrecisionEstimates"]] = list(
                                              description=L$description, 
                                              data=L$data)
                        } else if (xmlName(LLPChild) == "Percentiles"){
                          L = ParseElement(LLPChild)
                          SOObject@Estimation@PrecisionPopulationEstimates[["LLP"]][["Percentiles"]] = list(
                                              description=L$description, 
                                              data=L$data)
                        }
                      }
                    },
                    "SIR" = {
                      # Fetch Children of Node
                      SIRChildren = xmlChildren(child)
                      # Parse XMl DataSet Structure and update SO 
                      for (SIRChild in SIRChildren) {

                        if (xmlName(SIRChild) == "PrecisionEstimates"){
                          L <- ParseElement(SIRChild)
                          SOObject@Estimation@PrecisionPopulationEstimates[["SIR"]][["PrecisionEstimates"]] = list(
                                              description=L$description, 
                                              data=L$data)
                        } else if (xmlName(SIRChild) == "Percentiles"){
                          L = ParseElement(SIRChild)
                          SOObject@Estimation@PrecisionPopulationEstimates[["SIR"]][["Percentiles"]] = list(
                                              description=L$description, 
                                              data=L$data)
                        }
                      }
                    },
                    "MultiDimLLP" = {
                      # Fetch Children of Node
                      MultiDimLLPChildren = xmlChildren(child)
                      # Parse XMl DataSet Structure and update SO 
                      for (MultiDimLLPChild in MultiDimLLPChildren) {

                        if (xmlName(MultiDimLLPChild) == "PrecisionEstimates"){
                          L <- ParseElement(MultiDimLLPChild)
                          SOObject@Estimation@PrecisionPopulationEstimates[["MultiDimLLP"]][["PrecisionEstimates"]] = list(
                                              description=L$description, 
                                              data=L$data)
                        } else if (xmlName(MultiDimLLPChild) == "Percentiles"){
                          L <- ParseElement(MultiDimLLPChild)
                          SOObject@Estimation@PrecisionPopulationEstimates[["MultiDimLLP"]][["Percentiles"]] = list(
                                              description=L$description, 
                                              data=L$data)
                        }
                      }
                    }
                )
            }
        },
        # this should be dead code, since only MLE, Bayesian, OtherMethod permitted by class
        warning("block ", xmlName(child), " ignored by ParsePrecisionPopulationEstimates")
    )
  }
  return(SOObject)
}

ParseIndividualEstimates <- function(SOObject, IndividualEstimatesNode) {
	
	# Iterate over Child nodes, updating SO if appropriate element is present
	for (child in .getChildNodes(IndividualEstimatesNode)) {
		childNodeName <- xmlName(child)
		
		if (childNodeName == .NODENAME_ESTIMATES) {

	      subChildren = .getChildNodes(child)
	      for (subChild in subChildren){
	        
	        if (xmlName(subChild) == "Mean"){
	          L = ParseElement(subChild)
	          # Update SO Object Slot
	          SOObject@Estimation@IndividualEstimates[[.NODENAME_ESTIMATES]][["Mean"]] = list(
	                          description=L$description, 
	                          data=L$data)
	        
	        } else if(xmlName(subChild) == "Median") {
	          L = ParseElement(subChild)
	          # Update SO Object Slot
	          SOObject@Estimation@IndividualEstimates[[.NODENAME_ESTIMATES]][["Median"]] = list(
	                          description=L$description, 
	                          data=L$data)        
	
	        } else if(xmlName(subChild) == "Mode") {
	          L = ParseElement(subChild)
	          # Update SO Object Slot
	          SOObject@Estimation@IndividualEstimates[[.NODENAME_ESTIMATES]][["Mode"]] = list(
	                          description=L$description, 
	                          data=L$data)        
	
	        } else if(xmlName(subChild) == "Samples") {
	          L = ParseElement(subChild)
	          # Update SO Object Slot
	          SOObject@Estimation@IndividualEstimates[[.NODENAME_ESTIMATES]][["Samples"]] = list(
	                          description=L$description, 
	                          data=L$data)
	        }
	        # Option of an else here 
	      }
    
		}
		
		else if (childNodeName == .NODENAME_RANDOMEFFECTS) {
			
	      # Fetch Children of Node
	      subChildren = .getChildNodes(child)
	      # Parse XMl DataSet Structure and update SO 
	      for (subChild in subChildren) {
	
	        if (xmlName(subChild) == "EffectMean"){
	          L = ParseElement(subChild)
	          # Update SO Object Slot
	          SOObject@Estimation@IndividualEstimates[[.NODENAME_RANDOMEFFECTS]][["EffectMean"]] = list(
	                          description=L$description, 
	                          data=L$data)
	
	        } else if (xmlName(subChild) == "EffectMedian"){
	          L = ParseElement(subChild)
	          # Update SO Object Slot
	          SOObject@Estimation@IndividualEstimates[[.NODENAME_RANDOMEFFECTS]][["EffectMedian"]] = list(
	                          description=L$description, 
	                          data=L$data)
	
	        } else if (xmlName(subChild) == "EffectMode"){
	          L = ParseElement(subChild)
	          # Update SO Object Slot
	          SOObject@Estimation@IndividualEstimates[[.NODENAME_RANDOMEFFECTS]][["EffectMode"]] = list(
	                          description=L$description, 
	                          data=L$data)
	
	        } else if (xmlName(subChild) == "Samples"){
	          L = ParseElement(subChild)
	          # Update SO Object Slot
	          SOObject@Estimation@IndividualEstimates[[.NODENAME_RANDOMEFFECTS]][["Samples"]] = list(
	                          description=L$description, 
	                          data=L$data)
	        }
	      }

  		}
		
		else if (childNodeName == .NODENAME_ETASHRINKAGE) {
	
	      L = ParseElement(child)
	      # Update SO Object Slot
	      SOObject@Estimation@IndividualEstimates[[.NODENAME_ETASHRINKAGE]] = list(
	                          description=L$description, 
	                          data=L$data)
		}
		
		else {
			warning(paste("Unexpected child node", childNodeName, "encountered on IndividualEstimates, expected: Estimates, RandomEffects, EtaShrinkage"))
		}
	}
	
	return(SOObject)
}

ParsePrecisionIndividualEstimates <- function(SOObject, PrecisionIndividualEstimatesNode) {

	# Iterate over Child nodes, updating SO if appropriate element is present
	for (child in .getChildNodes(PrecisionIndividualEstimatesNode)) {
		childNodeName <- xmlName(child)
	
		if (childNodeName == "PosteriorDistributionIndividualEstimates") {
		  distList <- ParseDistribution(child)
			
		  # Update SO Object Slot
		  SOObject@Estimation@PrecisionIndividualEstimates <- list(
		      EstimatesDistribution = distList
		      )
		}
		
		else {
			warning(paste("Unexpected child node", childNodeName, "encountered on PrecisionIndividualEstimates, expected: PosteriorDistributionIndividualEstimates"))
		}
	}
  
	return(SOObject)                                 
}

ParseResiduals <- function(SOObject, ResidualsNode) {

  # Since SO 0.1 all residuals are in a single Dataset Table Structure called ResidualsTable

	# Iterate over Child nodes, updating SO if appropriate element is present
	for (child in .getChildNodes(ResidualsNode)) {
		childNodeName <- xmlName(child)
		
		if (childNodeName == .NODENAME_RESIDUALTABLE) {
			
	      L = ParseElement(child)
	
	      # Update SO Object Slot
	      SOObject@Estimation@Residuals[[.NODENAME_RESIDUALTABLE]] = list(
	                          description=L$description, 
	                          data=L$data) 

		}
		
		else if (xmlName(child) == .NODENAME_EPSSHRINKAGE) {
			
	      L = ParseElement(child)
	
	      # Update SO Object Slot
	      SOObject@Estimation@Residuals[[.NODENAME_EPSSHRINKAGE]] = list(
	                          description=L$description, 
	                          data=L$data)

		}
		
		else {
			warning(paste("Unexpected child node", childNodeName, "encountered on Residuals, expected: ResidualTable, EpsShrinkage"))
		}

	}
    
	return(SOObject)
}

ParsePredictions <- function(SOObject, PredictionsNode) {

  L = ParseElement(PredictionsNode)
  # Update SO Object Slot
  SOObject@Estimation@Predictions <- list(
                      description=L$description, 
                      data=L$data)

  return(SOObject)
}

ParseOFMeasures <- function(SOObject, OFMeasuresNode) {
	
	# Iterate over Child nodes, updating SO if appropriate element is present
	for (child in .getChildNodes(OFMeasuresNode)) {
		childNodeName <- xmlName(child)
		
		if (childNodeName == "Deviance") {
	      # Extract Deviance
	      SOObject@Estimation@OFMeasures$Deviance <- as.numeric(xmlValue(child))
		}
	
		else if (childNodeName == "LogLikelihood") {
	      # Extract Likelihood
	      SOObject@Estimation@OFMeasures$LogLikelihood <- as.numeric(xmlValue(child))
	  	}
		
		else if (childNodeName == "IndividualContribToLL") {
		
	      # Extract IndividualContribToLL
	      L = ParseElement(child)
		  
		  # Update SO Object Slot
		  SOObject@Estimation@OFMeasures$IndividualContribToLL <- list(
				  description=L$description, 
				  data=L$data)
		  
		}
		  
		else if (childNodeName == "InformationCriteria") {
	      # Fetch the values of the children for InformationCriteria 
	      SOObject@Estimation@OFMeasures$InformationCriteria <- lapply(.getChildNodes(child), xmlValue)
		}

		else {
			warning(paste("Unexpected child node", childNodeName, "encountered on OFMeasures, expected: Deviance, LogLikelihood, IndividualContribToLL, InformationCriteria"))
		}
	}

	return(SOObject)
}


ParseTaskInformation <- function(SOObject, TaskInformationNode) {
  
  # Initialise counters
  err.msg.count = 1
  warn.msg.count = 1
  term.msg.count = 1
  info.msg.count = 1
	
	# Iterate over Child nodes, updating SO if appropriate element is present
	for (child in .getChildNodes(TaskInformationNode)) {
		childNodeName <- xmlName(child)
		
		if (childNodeName == "Message") {
      
	      # Record all message types in a nested list structure
	
	      # Get "type" Attribute
	      message.type <- xmlGetAttr(child, "type")
	
	      # Pull out message content
	      msg.content <- list( 
	        "Toolname" = xmlValue(child[["Toolname"]][["String"]]), 
	        "Name" = xmlValue(child[["Name"]][["String"]]),
	        "Content" = xmlValue(child[["Content"]][["String"]]),  
	        "Severity" = as.numeric(xmlValue(child[["Severity"]][["Int"]]))
	        )
	
	      # Assign message content to appropriate list 
	      
	      # TODO: Error in current assignment opperation
	      
	      if (message.type == "ERROR"){
	        SOObject@TaskInformation$Messages$Errors[[err.msg.count]] <- msg.content
	        err.msg.count = err.msg.count + 1
	      } else if (message.type == "WARNING") {
	        SOObject@TaskInformation$Messages$Warnings[[warn.msg.count]]  <- msg.content
	        warn.msg.count = warn.msg.count + 1
	      } else if (message.type == "TERMINATION") {
	        SOObject@TaskInformation$Messages$Terminations[[term.msg.count]] <- msg.content
	        term.msg.count = term.msg.count + 1
	      } else if (message.type == "INFORMATION") {
	        SOObject@TaskInformation$Messages$Info[[info.msg.count]] <- msg.content
	        info.msg.count = info.msg.count + 1
	      }
	  
		}
		
		else if (childNodeName == "OutputFilePath") {
      
	      # Extract OutputFilePath
	      SOObject@TaskInformation$OutputFilePath = as.character(xmlValue(child[['path']]))
    
	  	}
		
	  	else if (childNodeName == "RunTime") {
            
	      # Extract RunTime
	      SOObject@TaskInformation$RunTime = as.numeric(xmlValue(child[['Real']]))
		  
	  	}
		
    	else if (childNodeName == "NumberChains") {
      
	      # Extract NumberChains
	      SOObject@TaskInformation$NumberChains <- list(
	        description=as.character(xmlValue(child[['Description']])), 
	        value=as.numeric(xmlValue(child[["Int"]]))
	        )

		}
		
		else if (childNodeName == "NumberIterations") {
      
	      # Extract NumberIterations
	      SOObject@TaskInformation$NumberIterations <- as.numeric(xmlValue(child[["Int"]]))

		}
		
		else {
			warning(paste("Unexpected child node", childNodeName, "encountered on TaskInformation, expected: Message, OutputFilePath, RunTime, NumberChains, NumberIterations"))
		}
	}

	return(SOObject)
}


# ======================= #
# Simulation Slot Parsers #
# ======================= #

ParseSimulation <- function(SOObject, SimulationNode) {

  # Process all Simulation Blocks
  SimulationBlockNodeList <- SimulationNode[names(SimulationNode) == "SimulationBlock"]
  SOObject@Simulation@SimulationBlock <- lapply(X = SimulationBlockNodeList, 
    FUN = ParseSimulationBlocks)

  return(SOObject)
}

ParseSimulationBlocks <- function(SimulationBlockNode) {

	SimulationBlock <- new("SimulationBlock")
	
	# Iterate over Child nodes, updating SO if appropriate element is present
	for (child in .getChildNodes(SimulationBlockNode)) {
        switch(xmlName(child),
            "SimulatedProfiles" = {
                SimulationBlock@SimulatedProfiles <- ParseElement(child)
            },
            "IndivParameters" = {
                SimulationBlock@IndivParameters <- ParseElement(child)
            },
            "RandomEffects" = {
                SimulationBlock@IndivParameters <- ParseElement(child)
            },
            "Covariates" = {
                SimulationBlock@Covariates <- ParseElement(child)
            },
            "PopulationParameters" = {
                SimulationBlock@PopulationParameters <- ParseElement(child)
            },
            "Dosing" = {
                SimulationBlock@Dosing <- ParseElement(child)
            },
            "RawResultsFile" = {
                tempList <- xmlApply(X = child, 
                    FUN = function(x) { xmlName(x) <- xmlValue(x) }) 
                SimulationBlock@RawResultsFile <- tempList
            },
			warning(paste("Unexpected child node", xmlName(child), "encountered on SimulationBlock, expected: SimulatedProfiles, IndivParameters, RandomEffects, Covariates, PopulationParameters, Dosing, RawResultsFile"))
        )
    }
    return(SimulationBlock)
}

# ============================ #
# ModelDiagnostic Slot Parsers #
# ============================ #

# TODO update this for v0.3, and refactor to use new child node parsing pattern
ParseModelDiagnostic <- function(SOObject, ModelDiagnosticNode) {
  
  # Error Checking of unexpected elements in each SimulationBlock block
  expectedTags = c("DiagnosticPlotsStructuralModel", 
    "DiagnosticPlotsIndividualParams")
  unexpected = setdiff(names(ModelDiagnosticNode), expectedTags)

  if (length(unexpected) != 0) {
    warning(paste("The following unexpected elements were detected in the ModelDiagnosticNode block of the PharmML SO.", 
            paste(unexpected, collapse="\n      "), sep="\n      "))
  }

  ModelDiagnosticSlot = new("ModelDiagnostic")

  for (child in .getChildNodes(ModelDiagnosticNode)) {

    if (xmlName(child) == "DiagnosticPlotsStructuralModel" ) {

      ModelDiagnosticSlot@DiagnosticPlotsStructuralModel = ParseDiagnosticPlotsStructuralModel(child)

    } else if (xmlName(child) == "DiagnosticPlotsIndividualParams" ) {

      L = ParseElement(child)
      ModelDiagnosticSlot@DiagnosticPlotsIndividualParams = list(
                          description=L$description, 
                          data=L$data)
    }
  }

  SOObject@ModelDiagnostic = ModelDiagnosticSlot

  return(SOObject)
}

# TODO update this for v0.3, and refactor to use new child node parsing pattern
ParseDiagnosticPlotsStructuralModel <- function(DiagnosticPlotsStructuralNode) {
  
  outputList = list()

  # Get list of Child Nodes
  children = .getChildNodes(DiagnosticPlotsStructuralNode)
  # Iterate over Child nodes, updating SO if appropriate element is present 
  for (child in children){
    
    if (xmlName(child) == "IndivFits") {
      subChildren = .getChildNodes(child)
      for (subChild in subChildren){
        
        if (xmlName(subChild) == "ObservationTable"){
          
          L = ParseElement(subChild)
      
          outputList[["IndivFits"]][["ObservationTable"]] = list(
                          description=L$description, 
                          data=L$data)

        } else if (xmlName(subChild) == "PredictionTable"){
          
          L = ParseElement(subChild)
          
          outputList[["IndivFits"]][["PredictionTable"]] = list(
                          description=L$description, 
                          data=L$data)
        }
      }

    } else if (xmlName(child) == "IndivPredictionVsObserv") {

          L = ParseElement(child)
          
          outputList[["IndivPredictionVsObserv"]] = list(
                          description=L$description, 
                          data=L$data)


    } else if (xmlName(child) == "VPC") {
          
          L = ParseElement(child)
          
          outputList[["VPC"]] = list(
                          description=L$description, 
                          data=L$data)
    }
  }
  return(outputList)
}

