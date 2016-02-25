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
.NODENAME_EXTERNALFILE <- "ExternalFile"


# ================== #
# Data Block Parsers #
# ================== #


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
  
  # Check format of xml element 
  childNames <- names(xmlChildren(node))
  
  # Filter out comments 
  childNames <- childNames[childNames != 'comment']
  
  OUT <- FALSE

  if (length(childNames) == 1) {
    switch(childNames, 
      "Matrix" = {
        # Parse Node as a matrix 
        OUT <- ParseMatrix(.getChildNode(node, "Matrix"))
      }, 
      .NODENAME_EXTERNALFILE = {
        # Load data from external file
        OUT <- ParseExternalFile(getChildNode(node, .NODENAME_EXTERNALFILE))
      },
      warning("block not parsed in ParseElement"))
  } else {
    if (length(childNames) == 2) {
      switch(paste(sort(childNames), collapse = " "), 
        "Definition ExternalFile" = {
          # Load data from external file
          OUT <- ParseDataSetExternalFile(node)
        }, 
        "Definition Table" = {
          # Load data from inline XML
          OUT <- ParseDataSetInline(node)
        },
        warning("expected ExternalFile or Table blocks with Definition in ParseElement, but found ",
          paste(childNames, collapse = " ")))
    } else {
      warning("expected 1 or 2 blocks in ParseElement, but found ", length(childNames))
    }
  }
  
  if (is.logical(OUT)) {
    stop(paste("Names of child elements not recognised as a parsable object in the SO XML. Element child names are:\n   ", 
      paste(childNames, collapse="\n    ")))
  }
  return(OUT)
}

#' ParseDataSetInline
#'
#' Utility function to parse a DataSet xml structure as it appears in PharmML. 
#'
#' @param parentNode The parent xmlNode object that contains two decendant tags:
#'   Definition and Table 
#'
#' @return Returns a list with two named elements: \code{description}, which holds all 
#' the meta data about the columns in a data frame; \code{data}, which holds the
#' actual values in a dataframe.
#'
ParseDataSetInline <- function(parentNode) {
  
  parentNodeChildList <- xmlChildren(parentNode)

  # Error checking
  # Strip comments first
  parentNodeChildNames <- names(parentNodeChildList)[names(parentNodeChildList) != "comment"]
  
  # Namespaces are not dealt with correctly in the R xml library, so two hardcoded 
  # versions of this function are necessary until a workaround is found.
  token <- ""
  if (any(grepl(pattern = "^ds:", x = parentNodeChildNames))) { token <- "ds:" }
  stopifnot(all(paste0(token, c("Definition", "Table")) %in% parentNodeChildNames))
  
  definitionTagName <- paste0(token, "Definition")
  tableTagName <- paste0(token, "Table")
  rowTagName <- paste0(token, "Row")
  columnTagName <- paste0(token, "Column")
  
  definition <- .getChildNode(parentNodeChildList, definitionTagName)
  table <- .getChildNode(parentNodeChildList, tableTagName)
  
  # Extract all column Information and store in a data frame
  columnInfo <- as.data.frame(xmlSApply(definition, FUN = function(x) list(
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
  rowList <- xmlChildren(table)
    
  # List of values
  rowData <- lapply(X = rowList, FUN = function(x) { xmlSApply(X = x, FUN = xmlValue) })
  
  # Filter out any Commnet lines 
  rowData <- rowData[names(rowData) != "comment"]
  
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
#'
#' @param parentNode The parent xmlNode object that contains two decendant tags:
#'   Definition and ExternalFile
#'
#' @return Returns a list with two named elements: \code{description}, which holds all 
#' the meta data about the columns in a data frame; \code{data}, which holds the
#' actual values in a dataframe.
#'
ParseDataSetExternalFile <- function(parentNode) {
  
  childNodes <- xmlChildren(parentNode)

  # Strip comments from the list of child node names
  #parentNodeChildNames <- names(parentNodeChildList)[names(parentNodeChildList) != "comment"]
  
  # Namespaces are not dealt with correctly in the R xml library, so
  # have to check nodes both with and without the namespace prefix on
  # their names until a workaround is found.  
  definition <- .getChildNode(childNodes, "Definition")
  externalFile <- .getChildNode(childNodes, .NODENAME_EXTERNALFILE)
  if (is.null(externalFile)) {
	externalFile <- childNodes[[paste0(.NODENAMES_NAMESPACE_PREFIX, .NODENAME_EXTERNALFILE)]]  
  }
  
  # Error checking
  if (is.null("definition")) {
	  stop(paste("No expected Definition child node found within", xmlName(parentNode), "node when parsing a data set node"))
  }
  if (is.null("externalFile")) {
	  stop(paste("No expected ExternalFile child node found within", xmlName(parentNode), "node when parsing a data set node"))
  }
      
  # Extract all column Information and store in a data frame
  columnInfo = as.data.frame(xmlSApply(definition, FUN = function(x) list(
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
  datf <- ParseExternalFile(externalFile)

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
  
  if ( (length(matrixNode[["RowNames"]]) == 0) || (length(matrixNode[["ColumnNames"]]) == 0) ) {
	  warning("No RowNames or ColumnNames found for Matrix element. Skipping...")
	  return(NULL)
  }
	
  # Get rownames of matrix 
  matrixRowNames <- xmlSApply(matrixNode[["RowNames"]], xmlValue)
  
  # Get colnames of matrix 
  matrixColumnNames <- xmlSApply(matrixNode[["ColumnNames"]], xmlValue)
  
  # Get all Matrix Rows that contain data
  matrixDataRows <- matrixNode[names(matrixNode) == "MatrixRow"]

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
  delimiter <<- externalFileAttrs[["delimiter"]]
  
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

  subChildren <- xmlChildren(Node)
  
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

ParseRawResults <- function(SOObject, RawResultsNode) {
  
  objectIdNames <- xmlSApply(RawResultsNode, xmlAttrs) 
  
  DataFileTempList <- list()
  GraphicsFileTempList <- list()
  
  for (i in seq(along=xmlChildren(RawResultsNode))) {
    
    fileType = names(RawResultsNode[i])

    node = RawResultsNode[[i]]
    
    if (! any(c('XMLCommentNode','XMLInternalCommentNode') %in% class(node)) ) {
		
      # Extract child tags and values as a list with names = tag names and elements = tag values
      childTags = xmlSApply(node, xmlValue)
	  
	  as.list(childTags)
    
      # Add this as an element to the Final Files List 
      if (fileType == 'DataFile') {
        DataFileTempList[objectIdNames[[i]]] = list(as.list(childTags))
      } else if (fileType == 'GraphicsFile') {
        GraphicsFileTempList[objectIdNames[[i]]] = list(as.list(childTags))
      }
    }
  }
  
  # Assign Result to Files slot of RawResults
  SOObject@RawResults@DataFiles = DataFileTempList
  SOObject@RawResults@GraphicsFiles = GraphicsFileTempList
  
  return(SOObject)
}

# =========================== #
# Parsers for Estimation Slot #
# =========================== #

ParsePopulationEstimates <- function(SOObject, PopulationEstimatesNode) {
  
    # Get list and reference to Child Nodes
    children <- xmlChildren(PopulationEstimatesNode)
    L0 <- list(description = NULL, data = NULL)
  
    for (child in children) {
        
        switch(xmlName(child),
            "MLE" = {
                # Parse XMl DataSet Structure	  
                L <- ParseElement(child)
                # Update SO Object Slot
                SOObject@Estimation@PopulationEstimates[["MLE"]] <- L[c("description", "data")]
            },
            "Bayesian" = {
                # Fetch Children of Node
                BayesianChildren <- xmlChildren(child)
                # Parse XMl DataSet Structure	and update SO	
                for (BChild in c("PosteriorMean", "PosteriorMedian", "PosteriorMode")) {
                    if (BChild %in% names(BayesianChildren)) {
                        L <- ParseElement(BayesianChildren[[BChild]])
                    } else {
                        L <- L0
                    }
                    SOObject@Estimation@PopulationEstimates[["Bayesian"]][[BChild]] <- 
                        L[c("description", "data")]
                }
            },
            "OtherMethod" = {
                # Fetch Children of Node
                OtherMethodChildren <- xmlChildren(child)
                if (!"method" %in% names(attributes(OtherMethodChildren))) {
                    warning("malformed XML in ParsePopulationEstimates\n",
                        "method attribute expected in OtherMethod sub-block (since v0.3)")
                } else {
                    switch(attributes(OtherMethodChildren)[["method"]],
                        # Parse XMl DataSet Structure and update SO 
                        "Bootstrap" = {
                            for (BChild in c("Mean", "Median")) {
                                if (BChild %in% names(BootstrapChildren)) {
                                    L <- ParseElement(BootstrapChildren[[BChild]])
                                } else {
                                    L <- L0
                                }
                                SOObject@Estimation@PopulationEstimates[["Bootstrap"]][[BChild]] <- 
                                    L[c("description", "data")]
                            }
                        }, 
                        "LLP" = { 
                            warning("LLP not implemented for PopulationEstimates") 
                            SOObject@Estimation@PopulationEstimates[["LLP"]] <- L0
                        },
                        "SIR" = { 
                            warning("SIR not implemented for PopulationEstimates")
                            SOObject@Estimation@PopulationEstimates[["SIR"]] <- L0
                        },
                        "MultiDimLLP"  = { 
                            warning("MultiDimLLP not implemented for PopulationEstimates")
                            SOObject@Estimation@PopulationEstimates[["MultiDimLLP"]] <- L0
                        },
                        warning("OtherMethod not recognised in ParsePopulationEstimates")
                    )
                }
            },
            warning("block ", xmlName(child), " ignored by ParsePopulationEstimates")
        )
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
  
  # Get list of Child Nodes
  children = xmlChildren(IndividualEstimatesNode)
  # Iterate over Child nodes, updating SO if appropriate element is present 
  for (child in children){
    
    if (xmlName(child) == "Estimates") {
      subChildren = xmlChildren(child)
      for (subChild in subChildren){
        
        if (xmlName(subChild) == "Mean"){
          L = ParseElement(subChild)
          # Update SO Object Slot
          SOObject@Estimation@IndividualEstimates[["Estimates"]][["Mean"]] = list(
                          description=L$description, 
                          data=L$data)
        
        } else if(xmlName(subChild) == "Median") {
          L = ParseElement(subChild)
          # Update SO Object Slot
          SOObject@Estimation@IndividualEstimates[["Estimates"]][["Median"]] = list(
                          description=L$description, 
                          data=L$data)        

        } else if(xmlName(subChild) == "Mode") {
          L = ParseElement(subChild)
          # Update SO Object Slot
          SOObject@Estimation@IndividualEstimates[["Estimates"]][["Mode"]] = list(
                          description=L$description, 
                          data=L$data)        

        } else if(xmlName(subChild) == "Samples") {
          L = ParseElement(subChild)
          # Update SO Object Slot
          SOObject@Estimation@IndividualEstimates[["Estimates"]][["Samples"]] = list(
                          description=L$description, 
                          data=L$data)
        }
        # Option of an else here 
      }
    } else if (xmlName(child) == "RandomEffects") {
      # Fetch Children of Node
      subChildren = xmlChildren(child)
      # Parse XMl DataSet Structure and update SO 
      for (subChild in subChildren) {

        if (xmlName(subChild) == "EffectMean"){
          L = ParseElement(subChild)
          # Update SO Object Slot
          SOObject@Estimation@IndividualEstimates[["RandomEffects"]][["EffectMean"]] = list(
                          description=L$description, 
                          data=L$data)

        } else if (xmlName(subChild) == "EffectMedian"){
          L = ParseElement(subChild)
          # Update SO Object Slot
          SOObject@Estimation@IndividualEstimates[["RandomEffects"]][["EffectMedian"]] = list(
                          description=L$description, 
                          data=L$data)

        } else if (xmlName(subChild) == "EffectMode"){
          L = ParseElement(subChild)
          # Update SO Object Slot
          SOObject@Estimation@IndividualEstimates[["RandomEffects"]][["EffectMode"]] = list(
                          description=L$description, 
                          data=L$data)

        } else if (xmlName(subChild) == "Samples"){
          L = ParseElement(subChild)
          # Update SO Object Slot
          SOObject@Estimation@IndividualEstimates[["RandomEffects"]][["Samples"]] = list(
                          description=L$description, 
                          data=L$data)
        }
      }
    } else if (xmlName(child) == "EtaShrinkage") {
      L = ParseElement(child)
      # Update SO Object Slot
      SOObject@Estimation@IndividualEstimates[["EtaShrinkage"]] = list(
                          description=L$description, 
                          data=L$data)
    }
  }
  return(SOObject)
}


ParsePrecisionIndividualEstimates <- function(SOObject, PrecisionIndividualEstimatesNode) {
  
  # Get list of Child Nodes
  children = xmlChildren(PrecisionIndividualEstimatesNode)

  # Iterate over Child nodes, updating SO if appropriate element is present 
  distList = list()
  for (child in children){
    
    if ("PosteriorDistributionIndividualEstimates" %in% xmlName(child)) {
      distList <- ParseDistribution(child)
    }
  }
  
  # Update SO Object Slot
  SOObject@Estimation@PrecisionIndividualEstimates <- list(
      EstimatesDistribution = distList
      )
  
  return(SOObject)                                 
}

ParseResiduals <- function(SOObject, ResidualsNode) {

  # Since SO 0.1 all residuals are in a single Dataset Table Structure
  # called ResidualsTable

  # Get list of Child Nodes
  children = xmlChildren(ResidualsNode)

  # Iterate over Child nodes, updating SO if appropriate element is present 
  for (child in children){
    
    if (xmlName(child) == "ResidualTable") {
      L = ParseElement(child)

      # Update SO Object Slot
      SOObject@Estimation@Residuals[["ResidualTable"]] = list(
                          description=L$description, 
                          data=L$data) 

    } else if (xmlName(child) == "EpsShrinkage") {
      L = ParseElement(child)

      # Update SO Object Slot
      SOObject@Estimation@Residuals[["EpsShrinkage"]] = list(
                          description=L$description, 
                          data=L$data)      
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
	
  # Get list of Child Nodes
  OFMeasuresChildren <- xmlChildren(OFMeasuresNode)
  # Iterate over Child nodes, updating SO if appropriate element is present 
  for (child in OFMeasuresChildren) {
    
    if (xmlName(child) == "Deviance") {
      
      # Extract Deviance
      SOObject@Estimation@OFMeasures$Deviance <- as.numeric(xmlValue(child))
	  
	  }
	
    if (xmlName(child) == "LogLikelihood") {
      
      # Extract Likelihood
      SOObject@Estimation@OFMeasures$LogLikelihood <- as.numeric(xmlValue(child))
	  
    }
	
	  if (xmlName(child) == "IndividualContribToLL") {
		
      # Extract IndividualContribToLL
      L = ParseElement(child)
	  
	  # Update SO Object Slot
	  SOObject@Estimation@OFMeasures$IndividualContribToLL <- list(
			  description=L$description, 
			  data=L$data)
	  }

    if (xmlName(child) == "InformationCriteria") {
    
      # Fetch the values of the children for InformationCriteria 
      SOObject@Estimation@OFMeasures$InformationCriteria <- lapply(xmlChildren(child), xmlValue)
    }
	
  }

  return(SOObject)
}


ParseTaskInformation <- function(SOObject, TaskInformationNode){

  # Get list of Child Nodes
  children <- xmlChildren(TaskInformationNode)

  # Iterate over Child nodes, updating SO if appropriate element is present 
  
  # Initialise counters
  err.msg.count = 1
  warn.msg.count = 1
  term.msg.count = 1
  info.msg.count = 1

  for (child in children) {
    
    if (xmlName(child) == "Message" ) {
      
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

    } else if (xmlName(child) == "OutputFilePath") {
      
      # Extract OutputFilePath
      SOObject@TaskInformation$OutputFilePath = as.character(xmlValue(child[['path']]))
    
    } else if (xmlName(child) == "RunTime") {
            
      # Extract RunTime
      SOObject@TaskInformation$RunTime = as.numeric(xmlValue(child[['Real']]))

    } else if (xmlName(child) == "NumberChains") {
      
      # Extract NumberChains
      SOObject@TaskInformation$NumberChains <- list(
        description=as.character(xmlValue(child[['Description']])), 
        value=as.numeric(xmlValue(child[["Int"]]))
        )

    } else if (xmlName(child) == "NumberIterations") {
      
      # Extract NumberIterations
      SOObject@TaskInformation$NumberIterations <- as.numeric(xmlValue(child[["Int"]]))

    } 
  }

  return(SOObject)
}

# ======================= #
# Simulation Slot Parsers #
# ======================= #

ParseSimulation <- function(SOObject, SimulationNode) {

  # Get list of Child Nodes
  children <- xmlChildren(SimulationNode)

  # Iterate over Child nodes, updating SO if appropriate element is present 
  for (child in children) {
	  
    if (xmlName(child) == "OriginalDataset" ) {
		
      tempList <- xmlApply(child, 
              FUN = function(x) { xmlName(x) <- xmlValue(x) }) 
      SOObject@Simulation@OriginalDataset <- tempList
	  
    }
	
  }

  # Process all Simulation Blocks
  SimulationBlockNodeList <- SimulationNode[names(SimulationNode) == "SimulationBlock"]
  SOObject@Simulation@SimulationBlock <- lapply(X = SimulationBlockNodeList, 
    FUN = ParseSimulationBlocks)

  return(SOObject)
}

ParseSimulationBlocks <- function(SimulationBlockNode) {
	
	# Error Checking of unexpected elements in each SimulationBlock block
	expectedTags = c("SimulatedProfiles", "RandomEffects", "IndivParameters", 
        "Covariates", "PopulationParameters", "Dosing", 
        "RawResultsFile")
	unexpected = setdiff(names(SimulationBlockNode), expectedTags)
	if (length(unexpected) != 0) {
		warning(paste("The following unexpected elements were detected in a SimulationBlock attribute of the parent Simulation section of the PharmML SO.", 
            paste(unexpected, collapse="\n      "), sep="\n      "))
	}
	
    SimulationBlock <- new("SimulationBlock")
    
    for (child in xmlChildren(SimulationBlockNode)) {
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
            }, warning("block ", xmlName(child), " not recognised in ParseSimulationBlocks")
        )
    }
    return(SimulationBlock)
}

# ============================ #
# ModelDiagnostic Slot Parsers #
# ============================ #

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

  for (child in xmlChildren(ModelDiagnosticNode)) {

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


ParseDiagnosticPlotsStructuralModel <- function(DiagnosticPlotsStructuralNode) {
  
  outputList = list()

  # Get list of Child Nodes
  children = xmlChildren(DiagnosticPlotsStructuralNode)
  # Iterate over Child nodes, updating SO if appropriate element is present 
  for (child in children){
    
    if (xmlName(child) == "IndivFits") {
      subChildren = xmlChildren(child)
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

