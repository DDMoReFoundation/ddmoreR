# ======================================= #
# Xml Parsers for secitons of the PharmML #
# ======================================= #
# 
# This file contains all code for the low level xml parsing 
# functions associated with loading the StardardOutputObject 
# from the PharmML. 
#
# Author: cmusselle

# ================== #
# Data Block Parsers #
# ================== #

#' ParseElement
#'
#' Investigates the name of the elements Children and runs the appropriate parser  
#'
ParseElement <- function(Node) {

  # Check format of xml element 
  ChildNames = names(xmlChildren(Node))

  OUT = FALSE

  if (length(ChildNames) == 1){
      if ("ct:Matrix" %in% ChildNames) {
        # Parse Node as a matrix 
        OUT = ParseMatrix(Node[["ct:Matrix"]])

      } else if ("ds:ImportData" %in% ChildNames) {
        # Load data from external file
        OUT = ParseImportData(Node[["ds:ImportData"]])

      }
  } else if (length(ChildNames) == 2) {
      if ("Definition" %in% ChildNames & "ds:ImportData" %in% ChildNames) {
        # Load data from external file
        OUT = ParseDataSetExternal(Node)

      } else if ("ds:Definition" %in% ChildNames & "ds:Table" %in% ChildNames) {
        # Load data from external file
        OUT = ParseDataSetInline(Node)

      } else if ("Definition" %in% ChildNames & "Table" %in% ChildNames) {
        # Load data from external file
        OUT = ParseDataSetInline(Node)
      }
  }
   
  if (class(OUT) == "logical") {
    stop("Element Children names not recognised as passable objects in SO")
  }
  OUT
}

#' ParseDataSetInline
#'
#' Utility function to parse a DataSet xml structure as it appears in PharmML. 
#'
#' @param parentNode The parent xmlNode object that contains two decendant tags:
#'   Definition and Table 
#'
#' @value Returns a list with two named elements: \code{description}, which holds all 
#' the meta data about the columns in a data frame; \code{data}, which holds the
#' actual values in a dataframe.
#'
ParseDataSetInline <- function(parentNode) {
  
  parentNodeChildList = xmlChildren(parentNode)

  # Error checking
  # Strip comments first
  parentNodeChildNames = names(parentNodeChildList)[names(parentNodeChildList) != "comment"]
  stopifnot(("Definition" %in% parentNodeChildNames & "Table" %in% parentNodeChildNames) | 
   ("ds:Definition" %in% parentNodeChildNames & "ds:Table" %in% parentNodeChildNames))

  # Namespaces are not delt with correctly in the R xml library, so two hardcoded 
  # versions of this function are necessary unitl a workaround is found.  
  if (xmlName(parentNode[[1]]) == "Definition" & xmlName(parentNode[[2]]) == "Table") {
    descriptionRef = "Definition"
    tableRef = "Table"
    rowRef = "Row"
    columnRef = "Column"
  } else if (xmlName(parentNode[[1]]) == "ds:Definition" & xmlName(parentNode[[2]]) == "ds:Table") {
    descriptionRef = "ds:Definition"
    tableRef = "ds:Table"
    rowRef = "ds:Row"
    columnRef = "ds:Column"
  }

  definition = parentNodeChildList[[descriptionRef]]
  table = parentNodeChildList[[tableRef]]
      
  # Extract all column Information and store in a data frame
  columnInfo = as.data.frame(xmlSApply(definition, FUN = function(x) list(
      xmlGetAttr(x, name="columnNum"), 
      xmlGetAttr(x, name="columnType"),
      xmlGetAttr(x, name="valueType"),
      xmlGetAttr(x, name="columnId")
      )
  ))
      
  # Filter out non Columns tags  
  columnInfo = columnInfo[, names(columnInfo) == columnRef]
  
  # Rename column headers to column ID
  names(columnInfo) <- unlist(columnInfo[4,])
  columnInfo = columnInfo[-4, ]
  # Rename rows to lable column info
  rownames(columnInfo) <- c("columnNum", "columnType", "valueType")
  
  # Get all Table Row elements
  # Supress namespace undefined messages 
  sink("NUL")
  rowList = xpathApply(table, paste0("//*[local-name() = '", rowRef, "']"))
  sink()

  # List of values
  rowData = lapply(rowList, FUN = function(x) xmlSApply(x, xmlValue))
  
  # Filter out and Commnet lines 
  rowData = lapply(rowData, function(x) {x[names(x) != "comment"]})
  
  # Convert to data frame 
  temp = Reduce(rbind, rowData)
  if (length(rowData) == 1) {
    # reduced list is a character vector
    df = t(data.frame(temp))
    rownames(df) <- NULL
    colnames(df) <- names(columnInfo)
  } else {
    dimnames(temp) <- NULL
    # reduced list is a character matrix
    df = data.frame(temp)
    colnames(df) <- names(columnInfo)
  }
  return(list(description=columnInfo, data=df))
}


#' ParseDataSetExternal
#'
#' Utility function to parse a DataSet xml structure as it appears in PharmML. 
#'
#' @param parentNode The parent xmlNode object that contains two decendant tags:
#'   Definition and Table 
#'
#' @value Returns a list with two named elements: \code{description}, which holds all 
#' the meta data about the columns in a data frame; \code{data}, which holds the
#' actual values in a dataframe.
#'
ParseDataSetExternal <- function(parentNode) {
  
  parentNodeChildList = xmlChildren(parentNode)

  # Error checking
  # Strip comments first
  parentNodeChildNames = names(parentNodeChildList)[names(parentNodeChildList) != "comment"]
  stopifnot(("Definition" %in% parentNodeChildNames & "ImportData" %in% parentNodeChildNames) | 
   ("Definition" %in% parentNodeChildNames & "ds:ImportData" %in% parentNodeChildNames))

  # Namespaces are not delt with correctly in the R xml library, so two hardcoded 
  # versions of this function are necessary unitl a workaround is found.  
  if (xmlName(parentNode[[1]]) == "Definition" & xmlName(parentNode[[2]]) == "ImportData") {
    descriptionRef = "Definition"
    importDataRef = "ImportData"
  } else if (xmlName(parentNode[[1]]) == "Definition" & xmlName(parentNode[[2]]) == "ds:ImportData") {
    descriptionRef = "Definition"
    importDataRef = "ds:ImportData"
  }

  definition = parentNodeChildList[[descriptionRef]]
  importData = parentNodeChildList[[importDataRef]]
      
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
  
  # Get all importData elements
  df = ParseImportData(importData)

  colnames(df) <- names(columnInfo)

  return(list(description=columnInfo, data=df))
}

#' ParseMatrix
#' 
#' Utility function to parse a Matrix xml structure as it appears in PharmML. 
#' 
#' @param matrixNode The Matrix xmlNode object that contains the decendant tags:
#'   ct:RowNames, ct:ColumnNames,  and multiple entries of ct:MatrixRow
#'
#' @value Returns a dataframe with row and column names taken from the appropriate 
#' tags in the Matrix structure. 
#'
ParseMatrix <- function(matrixNode) {
  
  # Get rownames of matrix 
  matrixRowNames = xmlSApply(matrixNode[["ct:RowNames"]], xmlValue)
  
  # Get colnames of matrix 
  matrixColumnNames = xmlSApply(matrixNode[["ct:ColumnNames"]], xmlValue)
  
  # Get all Matrix Rows that contain data
  # Supress namespace undefined messages
  sink("NUL")
  matrixDataRows = xpathApply(matrixNode, "//*[local-name() = 'ct:MatrixRow']")
  sink()

  # Extract the value element of each element
  output.matrix.transposed = sapply(matrixDataRows, FUN=function(x) xmlApply(x, xmlValue))
  output.matrix = t(output.matrix.transposed)
  
  # Convert to Data Frame
  df = as.data.frame(output.matrix)
  
  # Update row and column names
  if (nrow(df) != length(matrixRowNames)) {
    warning("Number of row names given does not match matrix dimensions. Row names ignored")
  } else {
    rownames(df) <- matrixRowNames
  }
  if (ncol(df) != length(matrixColumnNames)) {
    warning("Number of column names given does not match matrix dimensions. Column names ignored")
  } else {
    colnames(df) <- matrixColumnNames
  }
  
  return(df)
}


#' ParseImportData
#'
#' Utility function to parse and load in an external file to the PharmML, defined in the 
#' ImportData node passed in. 
#'
#' @param ImportDataNode The parent xmlNode object that contains three decendant tags:
#'   path, format and delimiter.  
#'
#' @value Returns a list with two named elements: \code{description}, which holds all 
#' the meta data about the columns in a data frame; \code{data}, which holds the
#' actual values in a dataframe.
#'
ParseImportData <- function(ImportDataNode) {

  # Get rownames of matrix 
  ImportDataChildern = xmlSApply(ImportDataNode, xmlValue)
  
  metaData = names(ImportDataChildern)

  stopifnot(("ds:path" %in% metaData) & ("ds:format" %in% metaData) 
    & ("ds:delimiter" %in% metaData) )

  path = ImportDataChildern[["ds:path"]]
  format = ImportDataChildern[["ds:format"]]
  delimiter = ImportDataChildern[["ds:delimiter"]]

  if (delimiter != "COMMA") {
    stop("Comma is the only delimiter currently supported by importData parsers.")
  }

  df  = read.csv(path, sep=",", na.strings=".", header=T) 
  
  return(df)
}



# =============== #
# Section Parsers #
# =============== #
#
# Low level parsers specific to certain sections of the xml 
#

ParseToolSettings <- function(SOObject, ToolSettingsNode) {  
  # Extract child tags and values as 
  # a list with names = tag names and elements = tag values
  tempList = xmlApply(ToolSettingsNode, 
  					  FUN = function(x) xmlName(x) = xmlValue(x)) 
  
  # Strip namespace parts in child element
  newNames = strsplit(names(tempList), ":")
  newNames = sapply(newNames, FUN = function(x)  x[[2]] )
  names(tempList) <- newNames
  
  SOObject@ToolSettings = tempList
  return(SOObject)
}

ParseRawResults <- function(SOObject, RawResultsNode) {
  
  # Get List of Raw Results Files
  # Supress namespace undefined messages
  sink("NUL")
  RawFileList = xpathApply(RawResultsNode, "//*[local-name() = 'RawFile']")
  sink()

  objectIdNames = sapply(RawFileList, xmlAttrs) 
  
  outerTempList = list()
  
  for (i in seq(along=objectIdNames)) {
    
    # Extract child tags and values as 
    # a list with names = tag names and elements = tag values
    innerTempList = xmlApply(RawFileList[[i]], 
    						 FUN = function(x) xmlName(x) = xmlValue(x)) 
    
    # Strip namespace parts in child element
    newNames = strsplit(names(innerTempList), ":")
    newNames = sapply(newNames, FUN = function(x)  x[[2]] )
    names(innerTempList) <- newNames
    
    # Add this as an element to the Final Files List 
    outerTempList[objectIdNames[[i]]] = list(innerTempList)
  
  }
  
  # Assign Result to Files slot of RawResults
  SOObject@RawResults@Files = outerTempList
  return(SOObject)
}

# =========================== #
# Parsers for Estimation Slot #
# =========================== #

ParsePopulationEstimates <- function(SOObject, PopulationEstimatesNode) {
  
  # Get list and reference to Child Nodes
  PopulationEstimatesChildren = xmlChildren(PopulationEstimatesNode)

  for (PEChild in PopulationEstimatesChildren){
    
    if (xmlName(PEChild) == "MLE") {
      
      # Parse XMl DataSet Structure	  
      L = ParseElement(PEChild)
      # Update SO Object Slot
      SOObject@Estimation@PopulationEstimates[["MLE"]] = list(
      										description=L$description, 
      										data=L$data)
      
    } else if (xmlName(PEChild) == "Bayesian") {
  	  # Fetch Children of Node
      BayesianChildren = xmlChildren(PEChild)
      # Parse XMl DataSet Structure	and update SO	
  	  for (BChild in c("PosteriorMean", "PosteriorMedian", "PosteriorMode")) {
  	 	  L = ParseElement(BayesianChildren[[BChild]])
  	  	SOObject@Estimation@PopulationEstimates[["Bayesian"]][[BChild]] = list(
    									    	description=L$description, 
        										data=L$data)
      }
    }
   }
  return(SOObject)
}


ParsePrecisionPopulationEstimates <- function(SOObject, PrecisionPopulationEstimatesNode) {
  
  # Get list of Child Nodes
  PrecisionPopulationEstimatesChildren = xmlChildren(PrecisionPopulationEstimatesNode)
  # Iterate over Child nodes, updating SO if appropriate element is present 
  for (PPEChild in PrecisionPopulationEstimatesChildren){
    
    if (xmlName(PPEChild) == "MLE") {
      MLEChildren = xmlChildren(PPEChild)
      for (MLEChild in MLEChildren){
        
        if (xmlName(MLEChild) == "FIM"){
          # if FIM is present, parse matrix as data frame to SO {
            DF = ParseElement(MLEChild)
            SOObject@Estimation@PrecisionPopulationEstimates[["MLE"]][["FIM"]] = DF 
        
        } else if(xmlName(MLEChild) == "CovarianceMatrix") {
          # if CovarianceMatrix is present, parse matrix as data frame to SO 
          DF = ParseElement(MLEChild)
          SOObject@Estimation@PrecisionPopulationEstimates[["MLE"]][["CovarianceMatrix"]] = DF
        
        } else if(xmlName(MLEChild) == "CorrelationMatrix") {
          # if CorrelationMatrix is present, parse matrix as data frame to SO 
          DF = ParseElement(MLEChild)
          SOObject@Estimation@PrecisionPopulationEstimates[["MLE"]][["CorrelationMatrix"]] = DF
        
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
    } else if (xmlName(PPEChild) == "Bayesian") {

      # Fetch Children of Node
      BayesianChildren = xmlChildren(PPEChild)
      # Parse XMl DataSet Structure and update SO 
      for (BayesianChild in BayesianChildren) {

        if (xmlName(BayesianChild) == "StandardDeviationPosterior"){
          L = ParseElement(BayesianChild)
          SOObject@Estimation@PrecisionPopulationEstimates[["Bayesian"]][["StandardDeviationPosterior"]] = list(
                              description=L$description, 
                              data=L$data)
        } else if (xmlName(BayesianChild) == "PosteriorDistribution"){
          # TODO ========================== Deal with Probability distributions
        } else if (xmlName(BayesianChild) == "PercentilesCI"){
          L = ParseElement(BayesianChild)
          SOObject@Estimation@PrecisionPopulationEstimates[["Bayesian"]][["PercentilesCI"]] = list(
                              description=L$description, 
                              data=L$data)
        }
      }
    }
  }
  return(SOObject)
}

ParseIndividualEstimates <- function(SOObject, IndividualEstimatesNode) {
  
  # Get list of Child Nodes
  IndividualEstimatesChildren = xmlChildren(IndividualEstimatesNode)
  # Iterate over Child nodes, updating SO if appropriate element is present 
  for (child in IndividualEstimatesChildren){
    
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

ParsePrecisionIndividualEstimates <- function() {
  # TODO ======================
}


ParseResiduals <- function(SOObject, ResidualsNode) {

  # Get list of Child Nodes
  ResidualsChildren = xmlChildren(ResidualsNode)
  # Iterate over Child nodes, updating SO if appropriate element is present 
  for (child in ResidualsChildren){
    for (target in c("RES", "IRES", "WRES", "CWRES", "IWRES", "PD", "NPDE")){
      if (xmlName(child) == target) {
        # Parse as a 
        L = ParseElement(child)
        # Update SO Object Slot
        SOObject@Estimation@Residuals[[target]] = list(
                      description=L$description, 
                      data=L$data)
      }
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

ParseLikelihood <- function(SOObject, LikelihoodNode) {
	
  # Get list of Child Nodes
  LikelihoodChildren = xmlChildren(LikelihoodNode)
  # Iterate over Child nodes, updating SO if appropriate element is present 
  for (child in LikelihoodChildren) {
    
    if (xmlName(child) == "Deviance") {
      
      # Extract Deviance
      SOObject@Estimation@Likelihood$Deviance <- as.numeric(xmlValue(child))
	  
	}
	
    if (xmlName(child) == "LogLikelihood") {
      
      # Extract Likelihood
      SOObject@Estimation@Likelihood$LogLikelihood = as.numeric(xmlValue(child))
	  
    }
	
	if (xmlName(child) == "IndividualContribToLL") {
		
      # Extract IndividualContribToLL
      L = ParseElement(child)
	  
	  # Update SO Object Slot
	  SOObject@Estimation@Likelihood$IndividualContribToLL = list(
			  description=L$description, 
			  data=L$data)
	  
	}
	
  }

  return(SOObject)
}
