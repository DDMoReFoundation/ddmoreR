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

#' ParseDataSet
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
ParseDataSet <- function(parentNode) {
  
  parentNodeChildList = xmlChildren(parentNode)

  # Error checking
  # Strip comments
  parentNodeChildNames = names(parentNodeChildList)[names(parentNodeChildList) != "comment"]
  stopifnot("Definition" %in% parentNodeChildNames & "Table" %in% parentNodeChildNames)

  definition = parentNodeChildList[["Definition"]]
  table = parentNodeChildList[["Table"]]
  
  # Extract all column Information and store in a data frame
  columnInfo = as.data.frame(xmlSApply(definition, FUN = function(x) list(
  		xmlGetAttr(x, name="columnNum"), 
    	xmlGetAttr(x, name="columnType"),
    	xmlGetAttr(x, name="valueType"),
    	xmlGetAttr(x, name="columnId")
    	)
  ))
  
  # Filter out non Columns tags  
  columnInfo = columnInfo[, names(columnInfo) == "Column"]
  
  # Rename column headers to column ID
  names(columnInfo) <- unlist(columnInfo[4,])
  columnInfo = columnInfo[-4, ]
  # Rename rows to lable column info
  rownames(columnInfo) <- c("columnNum", "columnType", "valueType")
  
  # Get all Table Row elements
  # Supress namespace undefined messages 
  sink("NUL")
  rowList = xpathApply(table, "//*[local-name() = 'Row']")
  sink()

  # List of values
  rowData = lapply(rowList, FUN = function(x) xmlSApply(x, xmlValue))
  
  # Filter out and Commnet lines 
  rowData = lapply(rowData, function(x) {x[names(x) != "comment"]})
  
  # Convert to data frame 
  temp = Reduce(rbind, rowData)
  if (length(rowData) == 1) {
    # reduced list is a character vector      
    df = unname(t(data.frame(temp)))
    rownames(df) <- NULL
    colnames(df) <- names(columnInfo)
  } else {
    # reduced list is a character matrix 
    df = unname(data.frame(temp))
    rownames(df) <- NULL
    colnames(df) <- names(columnInfo)
  }
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
      L = ParseDataSet(PEChild)
      # Update SO Object Slot
      SOObject@Estimation@PopulationEstimates[["MLE"]] = list(
      										description=L$description, 
      										data=L$data)
      
    } else if (xmlName(PEChild) == "Bayesian") {
  	  # Fetch Children of Node
      BayesianChildren = xmlChildren(PEChild)
      # Parse XMl DataSet Structure	and update SO	
  	  for (BChild in c("PosteriorMean", "PosteriorMedian", "PosteriorMode")) {
  	 	  L = ParseDataSet(BayesianChildren[[BChild]])
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
          # if FIM is present, parse matrix as data frame to SO 
          DF = ParseMatrix(MLEChild[["ct:Matrix"]])
          SOObject@Estimation@PrecisionPopulationEstimates[["MLE"]][["FIM"]] = DF
        
        } else if(xmlName(MLEChild) == "CovarianceMatrix") {
          # if CovarianceMatrix is present, parse matrix as data frame to SO 
          DF = ParseMatrix(MLEChild[["ct:Matrix"]])
          SOObject@Estimation@PrecisionPopulationEstimates[["MLE"]][["CovarianceMatrix"]] = DF
        
        } else if(xmlName(MLEChild) == "CorrelationMatrix") {
          # if CorrelationMatrix is present, parse matrix as data frame to SO 
          DF = ParseMatrix(MLEChild[["ct:Matrix"]])
          SOObject@Estimation@PrecisionPopulationEstimates[["MLE"]][["CorrelationMatrix"]] = DF
        
        } else if(xmlName(MLEChild) == "StandardError") {
          # if StandardError is present, parse DataSet as data frame to SO 
          L = ParseDataSet(MLEChild)
          # Update SO Object Slot
          SOObject@Estimation@PrecisionPopulationEstimates[["MLE"]][["StandardError"]] = list(
                          description=L$description, 
                          data=L$data)
        } else if(xmlName(MLEChild) == "RelativeStandardError") {
          # if RelativeStandardError is present, parse DataSet as data frame to SO 
          L = ParseDataSet(MLEChild)
          # Update SO Object Slot
          SOObject@Estimation@PrecisionPopulationEstimates[["MLE"]][["RelativeStandardError"]] = list(
                          description=L$description, 
                          data=L$data)

        } else if(xmlName(MLEChild) == "AsymptoticCI") {
          # if AsymptoticCI is present, parse DataSet as data frame to SO 
          L = ParseDataSet(MLEChild)
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
          L = ParseDataSet(BayesianChild)
          SOObject@Estimation@PrecisionPopulationEstimates[["Bayesian"]][["StandardDeviationPosterior"]] = list(
                              description=L$description, 
                              data=L$data)
        } else if (xmlName(BayesianChild) == "PosteriorDistribution"){
          # TODO ========================== Deal with Probability distributions
        } else if (xmlName(BayesianChild) == "PercentilesCI"){
          L = ParseDataSet(BayesianChild)
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
          L = ParseDataSet(subChild)
          # Update SO Object Slot
          SOObject@Estimation@IndividualEstimates[["Estimates"]][["Mean"]] = list(
                          description=L$description, 
                          data=L$data)
        
        } else if(xmlName(subChild) == "Median") {
          L = ParseDataSet(subChild)
          # Update SO Object Slot
          SOObject@Estimation@IndividualEstimates[["Estimates"]][["Median"]] = list(
                          description=L$description, 
                          data=L$data)        

        } else if(xmlName(subChild) == "Mode") {
          L = ParseDataSet(subChild)
          # Update SO Object Slot
          SOObject@Estimation@IndividualEstimates[["Estimates"]][["Mode"]] = list(
                          description=L$description, 
                          data=L$data)        

        } else if(xmlName(subChild) == "Samples") {
          L = ParseDataSet(subChild)
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
          L = ParseDataSet(subChild)
          # Update SO Object Slot
          SOObject@Estimation@IndividualEstimates[["RandomEffects"]][["EffectMean"]] = list(
                          description=L$description, 
                          data=L$data)

        } else if (xmlName(subChild) == "EffectMedian"){
          L = ParseDataSet(subChild)
          # Update SO Object Slot
          SOObject@Estimation@IndividualEstimates[["RandomEffects"]][["EffectMedian"]] = list(
                          description=L$description, 
                          data=L$data)

        } else if (xmlName(subChild) == "EffectMode"){
          L = ParseDataSet(subChild)
          # Update SO Object Slot
          SOObject@Estimation@IndividualEstimates[["RandomEffects"]][["EffectMode"]] = list(
                          description=L$description, 
                          data=L$data)

        } else if (xmlName(subChild) == "Samples"){
          L = ParseDataSet(subChild)
          # Update SO Object Slot
          SOObject@Estimation@IndividualEstimates[["RandomEffects"]][["Samples"]] = list(
                          description=L$description, 
                          data=L$data)
        }
      }
    } else if (xmlName(child) == "EtaShrinkage") {
      L = ParseDataSet(child)
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
        L = ParseDataSet(child)
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

  L = ParseDataSet(PredictionsNode)
  # Update SO Object Slot
  SOObject@Estimation@Predictions <- list(
                      description=L$description, 
                      data=L$data)

  return(SOObject)
}
