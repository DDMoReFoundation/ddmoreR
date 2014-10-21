## Xml Parsers for specific secitons of the XML


# ================== #
# Data Block Parsers #
# ================== #

ParseDataSet <- function(parentNode) {

  # Return a data frame of the descritpiton tag element and the data values in 
  # the table tag element of the parentNode
  
  tempChildList = xpathApply(parentNode, paste("/", xmlName(parentNode), "/*"), namespaces=ns)
  definition = tempChildList[[1]]
  table = tempChildList[[2]]
  
  # Extract all column Information and store in a data frame
  columnInfo = as.data.frame(xmlSApply(definition, FUN = function(x) list(
  		xmlGetAttr(x, name="columnNum"), 
    	xmlGetAttr(x, name="columnType"),
    	xmlGetAttr(x, name="valueType"),
    	xmlGetAttr(x, name="columnId")
    	)
  ))
  # Rename column headers to column ID
  names(columnInfo) <- unlist(columnInfo[4,])
  columnInfo = columnInfo[-4, ]
  # Rename rows to lable column info
  rownames(columnInfo) <- c("columnNum", "columnType", "valueType")
  
  # Extract Table values 
  rowList = xpathApply(table, "/ds:Table/ds:Row", namespaces=ns)
  # List of values
  rowData = lapply(rowList, FUN = function(x) xmlSApply(x, xmlValue))
  # Convert to data frame 
  temp = Reduce(rbind, rowData)
  if (length(rowData) == 1) {
    # reduced list is a character vector      
    df = unname(t(data.frame(temp)))
    colnames(df) <- names(columnInfo)
  } else {
    # reduced list is a character matrix 
    df = unname(data.frame(temp))
    colnames(df) <- names(columnInfo)
  }
  return(list(description=columnInfo, data=df))
}


# =============== #
# Section Parsers #
# =============== #

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
  RawFileList = xpathApply(RawResultsNode, "/RawResults//RawFile", 
  						   namespaces=ns)
  
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

ParsePopulationEstimates <- function(SOObject, PopulationEstimatesNode) {
  
  # Get list and reference to Child Nodes
  PopulationEstimatesChildren = xmlChildren(PopulationEstimatesNode)
  childNames = names(PopulationEstimatesChildren)	
 
  for (i in seq(along=childNames)){
    
    if (childNames[[i]] == "MLE") {
      
      # Parse XMl DataSet Structure	  
      L = ParseDataSet(PopulationEstimatesChildren[[childNames[[i]]]])
      # Update SO Object Slot
      SOObject@Estimation@PopulationEstimate[["MLE"]] = list(
      										description=L$description, 
      										data=L$data)
      
    } else if (childNames[[i]] == "Bayesian") {
  	  # Fetch Children of Node
      BayesianChildren = xmlChildren(PopulationEstimatesChildren[[childNames[[i]]]])
      # Parse XMl DataSet Structure	and update SO	
  	  for (child in c("PosteriorMean", "PosteriorMedian", "PosteriorMode")) {
  	 	  L = ParseDataSet(BayesianChildren[[child]])
  	  	SOObject@Estimation@PopulationEstimate[["Bayesian"]][[child]] = list(
    									    	description=L$description, 
        										data=L$data)
      }
    }
   }
  return(SOObject)
}


