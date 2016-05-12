

# ========================== #
# Reader for RawData files   #
# ========================== #
# readRawData 
#  
setGeneric(name="readRawData",
    def=function(SOObject, fileIndex) {
        standardGeneric("readRawData")
    }
)
setMethod(f = "readRawData",
    signature = "StandardOutputObject",
    definition = function(SOObject, fileIndex)
    {
        # Mappers from id to file description
        id2FileType <- lapply(SOObject@RawResults@DataFiles, function(x) x[["Description"]])
        names(id2FileType)<- tolower(names(id2FileType))
        
        fileType2Id <- as.list(names(id2FileType))
        names(fileType2Id) <- tolower(id2FileType)
        
        fileIndex <- tolower(fileIndex)
        
        # fileIndex can be file id or file description
        if (fileIndex %in% names(id2FileType)) {
            fileElement <- SOObject@RawResults@DataFiles[[fileIndex]]
        } else { 
            if (fileIndex %in% names(fileType2Id)) {
                fileElement <- SOObject@RawResults@DataFiles[[fileType2Id[[fileIndex]]]]
            } else {
                stop(paste0("File Reference: ",
                    fileIndex," not found for either file id or file description"))
            }
        }
        
        # TODO: Possibly need to add custom NA handelling here 
        rawData <- read.csv(file.path(dirname(SOObject@.pathToSourceXML), fileElement$path), na.strings=".")
        return (rawData)
    }
)


#' mergeByPosition
#'
#'  Utility used by as.data to check column types and then merge by possition
#'
#'  @param df1 The first data frame, column types and positions are known.
#'  @param df2 The second data frame to merge in, column types and positions are checked before merge.
#'  @param ID.colName The name used for the ID column in both df1 and df2.
#'  @param TIME.colName The name used for the TIME column in both df1 and df2.
#'
#' @export
mergeByPosition <- function(df1, df2, msg='', ID.colName, TIME.colName) {

  # Set defaults to ID and TIME if they are not given 
  if (missing(ID.colName)) {
    ID.colName = "ID"
  }
  if (missing(TIME.colName)) {
    TIME.colName = "TIME"
  }

  # Find ID column position in df2
  ID.colIdx = toupper(names(df2)) == toupper(ID.colName)
  TIME.colIdx = toupper(names(df2)) == toupper(TIME.colName)

  # Type conversion Checks for ID column
  if (class(df2[, ID.colIdx]) == "factor") {
      df2[, ID.colIdx] = as.character(df2[, ID.colIdx])
  } 
  df2[, ID.colIdx] = as.numeric(df2[, ID.colIdx])

  # Type conversion Checks for TIME column
  if (class(df2[, TIME.colIdx]) == "factor") {
      df2[, TIME.colIdx] = as.character(df2[, TIME.colIdx])
  } 
  df2[, TIME.colIdx] = as.numeric(df2[, TIME.colIdx])

  # Check ID column is the same on df1 and df2
  if (all(df1[, ID.colName] != df2[, ID.colIdx])) {
    stop(paste("ID column order of df1 and df2 does not match when merging", msg))
  }

  # Check TIME column is the same on df1 and df2 
  if (all(df1[, TIME.colName] != df2[, TIME.colIdx])) {
    stop(paste("TIME column order of df1 and df2 does not match when merging", msg))
  }

  # merge all unique columns between df1 and df2
  unique.names.df2 = setdiff(names(df2),c(ID.colName, TIME.colName))
  unique.names.df2 = setdiff(unique.names.df2, names(df1))
  mergedDf = cbind(df1, df2[, unique.names.df2], deparse.level = 0)
  # Update column names
  #names(mergedDf) <- c(names(df1), unique.names.df2)

  return(mergedDf)
}

#' checkDoseRows
#'
#' @param df1 dataframe The current data frame on the left.
#' @param df2 dataframe The data frame of values being merged on the right. 
#' @param label1 character string label for df1, the current data frame. 
#' @param label2 character string label for df2, the data frame that is being merged in. 
#' @param extraInfo character string for any additional information to be reported to the user. 
#'
#' @return Returns a potentially modified df1 data frame with all dose rows removed if required.
#' 
#' There may be extra rows present in the raw data or parts of the SO object that are not 
#' present in other slots of the SO. In order to merge these all together into a single 
#' data frame for Xpose, these extra dose rows need to be identified and dropped if a slot 
#' does not have them. 
#'
#' Dose rows are identified by the following algorithm:
#' \enumerate{
#'   \item If df1 has more rows than df2, then dose rows may be present in df1. 
#'   \item If \code{MDV} column is present, drop all rows in df1 where \code{MDV != 1}
#'   \item Else, if \code{AMT} column is present, drop all rows in df1 where \code{AMT > 0}
#'   \item After attempting to drop dose rows, recompare rows numbers of df1 and df2. If 
#'          they are not the same, through an error. 
#' }  
#'    
checkDoseRows <- function(df1, df2, label1, label2, extraInfo=NULL) {

  rowsDropped = FALSE

  # Test to see if data rows are the same, if not remove dose rows from the 
  # input data (df1) and recompare.
  if (nrow(df1) > nrow(df2)) {
    # Detect Dose rows via MDV column if present
    if ("MDV" %in% names(df1)) {
      df1 <- df1[df1[['MDV']] != 1, ]
      rowsDropped = TRUE

    } else if ("AMT" %in% names(df1))  {
      # Else detect dose rows via AMT column if present
      df1 <- df1[ df1[['AMT']] > 0, ]
      rowsDropped = TRUE
    }
  }

  if (nrow(df1) != nrow(df2)) {
    # Print out information to aid debugging
    message(paste("Source data frame:", label1, nrow(df1)))
    message(paste("Number of Rows in source data frame: ", nrow(df1)))
    
    message(paste("Target data frame:", label2, nrow(df2)))
    message(paste("Number of Rows in source data frame: ", nrow(df2)))

    stop(paste("Number of non-dose rows differs when attempting to merge in", label2))
  }

  # Provide feedback if df1 was modified 
  if (rowsDropped) {
    message(paste("\nRemoved dose rows in", label1, "slot of SO to enable merge with", label2, "data.\n"))
    if (!missing(extraInfo)) {
      message(extraInfo) 
    }     
  }

  return(df1)
}

#' mergeCheckColumnNames 
#' 
#'  @param ID.colName The name used for the ID column in both df1 and df2.
#'  @param TIME.colName The name used for the TIME column in both df1 and df2.
#'
#' Utility to merge two dataframes but print out a warning if duplicate column names are detected
mergeCheckColumnNames <- function(df1, df2, ID.colName, TIME.colName) {

  # Set defaults to ID and TIME if they are not given 
  if (missing(ID.colName)) {
    ID.colName = "ID"
  }
  if (missing(TIME.colName)) {
    TIME.colName = "TIME"
  }

  # Check column names
  duplicateNames = setdiff(intersect(names(df1), names(df2)), c(ID.colName, TIME.colName))
  duplicateNamesIdx = sapply(duplicateNames, FUN=function(x) grep(x, names(df2)))

  if (length(duplicateNames) > 0 ) {
    warning(paste("The following duplicate column names were detected and will be dropped from the output: ", 
            paste(duplicateNames, collapse="\n      "), sep="\n      "))
    df2 <- df2[-duplicateNamesIdx]
  }

  mergedDataFrame <- merge(df1, df2)

}

#' @title Extract ID and IDV Names 
#' 
#' @description Utility to extract the names of the columns marked as id and idv. These are usually ID and TIME, 
#' but may differ. 
#' 
#' @param SOObject The SOObject to extract information from. 
#' @param PredictionsSlotName the name of the slot for the Predictions in the SOObject. (default NULL)
#' @param ResidualsSlotName the name of the slot for the Residuals in the SOObject. (default NULL)
#' @return length 4 list with elements \enumerate{
#'   \item ID.index logical matrix with one row (name columnType) with zero or more columns named for names present
#'   \item ID.colName single character corresponding to name of ID.index where columnType is TRUE
#'   \item TIME.index matrix with one row (name columnType) with zero or more columns named for names present
#'   \item TIME.colName single character corresponding to name of TIME.index where columnType is TRUE
#' }
#' @examples
#' soXmlFilePath <- system.file("tests", "data", "PharmMLSO", "MachineGenerated", 
#'     "UseCase2_TIMEchange_fixed.SO.xml", 
#'     package = "ddmore")
#' SOObject <- suppressMessages(LoadSOObject(dataFile))
#' ddmore:::extractIdandIdvNames(SOObject = SOObject, 
#'     PredictionsSlotName = "Estimation::Predictions")

extractIdandIdvNames <- function(SOObject, 
    PredictionsSlotName = NULL, ResidualsSlotName = NULL) {
  
  mat0 <- matrix(data = NA, nrow = 1, ncol = 0, 
    dimnames = list("columnType", character(0)))
  
  idTimeLs <- list(
      ID.index = mat0, 
      ID.colName = character(0), 
      TIME.index = mat0, 
      TIME.colName = character(0))
  
  # Check slots exist (character vector)
  populatedSlots <- getPopulatedSlots(SOObject)
  
  for (slotName in c(PredictionsSlotName, ResidualsSlotName)){
    if (slotName %in% populatedSlots) {

      x <- gsub("::", "@", slotName)
      ans <- eval(parse(text=paste("SOObject", x, sep="@")))

      idTimeLs$ID.index <- ans@description["columnType", , drop = FALSE] == "id"
      idTimeLs$ID.colName <- names(ans@description)[idTimeLs$ID.index]

      idTimeLs$TIME.index <- ans@description["columnType", , drop = FALSE] == "idv"
      idTimeLs$TIME.colName <- names(ans@description)[idTimeLs$TIME.index]

      # Assuming here that the id and idv columns are the same in all slots 
      # so when we have found one we don't need to continue searching others
      break
    }
  }
  
  # Error Check the result
  # Check that only a single column is assigned with id or idv 
  if (length(idTimeLs$ID.colName) > 1) {
    warning(paste0("Multiple DATA_INPUT_VARIABLES have use defined as 'id' in StandardOutputObject, (", 
      paste(idTimeLs$ID.colName, collapse = ", "), 
      ") cannot determine correct column name for ID from StandardOutputObject. "))
  } else if (length(idTimeLs$ID.colName) == 0) {
    warning(paste0("No DATA_INPUT_VARIABLES have a 'use' parameter defined as 'id' in the StandardOutputObject ", 
      "cannot determine correct column name for ID from StandardOutputObject."))
  }
  if (length(idTimeLs$TIME.colName) > 1) {
    warning(paste0("Multiple DATA_INPUT_VARIABLES have use defined as 'idv' in StandardOutputObject, (", 
      paste(idTimeLs$TIME.colName, collapse = ", "), 
      ") cannot determine correct column name for TIME from StandardOutputObject."))
  } else if (length(idTimeLs$TIME.colName) == 0) {
    warning(paste0("No DATA_INPUT_VARIABLES have a 'use' parameter defined as 'idv' in the StandardOutputObject ", 
      "cannot determine correct column name for TIME from StandardOutputObject."))
  }
  return(idTimeLs)
}


# ============================= #
# Convert to single Data Frame  #
# ============================= #
#' as.data
#'
#'  Method to Fetch all relevant data and return a merged data.frame object.
#'
#' @include readDataObj.R
#' @export
setGeneric(name = "as.data",
           def = function(SOObject, inputDataPath) {
    standardGeneric("as.data")
  }
)

setMethod(f = "as.data",
  signature = signature(SOObject = "StandardOutputObject", inputDataPath = "character"),
  definition = function(SOObject, inputDataPath) {
    
    # note never called due to S4 dispatch
	if (missing(inputDataPath)) {
        stop("Path to input data must be specified")
    }
    
    # Check slots exist 
    populatedSlots <- getPopulatedSlots(SOObject)
    
    PredictionsSlotName <- "Estimation::Predictions"
    ResidualsSlotName <- "Estimation::Residuals::ResidualTable"
    IndivEstEstMeanSlotName <- "Estimation::IndividualEstimates::Estimates"
    IndivEstRandomEffectsMeanSlotName <- "Estimation::IndividualEstimates::RandomEffects"
    
    res <- extractIdandIdvNames(SOObject, PredictionsSlotName, ResidualsSlotName)
    ID.colName <- res[["ID.colName"]]
    if (is.null(ID.colName) || any(is.na(ID.colName)) || length(ID.colName) != 1L) {
        warning("ID.colName was ", paste(deparse(ID.colName), collapse = ", "), 
            ", setting to 'ID' in as.data")
        ID.colName <- "ID"
    }
    TIME.colName <- res[["TIME.colName"]]
    if (is.null(TIME.colName) || any(is.na(TIME.colName)) || length(TIME.colName) != 1L) {
        warning("TIME.colName was ", paste(deparse(TIME.colName), collapse = ", "), 
            ", setting to 'TIME' in as.data")
        TIME.colName <- "TIME"
    }
    # Pass in the rawData file 
    rawData <- read.NONMEMDataSet(inputDataPath)
    # Convert all column headers to upper case 
    names(rawData) <- toupper(names(rawData))
    ID.colName <- toupper(ID.colName)
    TIME.colName <- toupper(TIME.colName)
    ID.col <- names(rawData) == ID.colName
    TIME.col <- names(rawData) == TIME.colName
    
    if (!any(ID.col)) {
        stop(ID.colName, " not present in file at inputDataPath")
    }
    if (!any(TIME.col)) {
        stop(TIME.colName, " not present in file at inputDataPath")
    }
    # Checks for Column format
    rawData[[ID.colName]] <- as.numeric(rawData[[ID.colName]])
    rawData[[TIME.colName]] <- as.numeric(rawData[[TIME.colName]])
    
    # Reorder data frame to have ID and TIME column as first two. 
    remainingNames <- setdiff(x = names(rawData),
        y = c(ID.colName, TIME.colName))
    rawData <- rawData[, c(ID.colName, TIME.colName, remainingNames)]

    # Begin merging columns
    # ---------------------

    mergedDataFrame <- rawData
    rm(rawData)
    
    if (PredictionsSlotName %in% populatedSlots) {
      # Predictions
      df1 <- mergedDataFrame
      df2 <- as.data.frame(SOObject@Estimation@Predictions@data)
      # Test to see if data rows are the same, if not remove dose rows from the 
      # input data (df1) and recompare.
      df1 <- checkDoseRows(df1, df2, label1="rawData", label2="Predictions")
      # Fetch and merge Predictions 
      mergedDataFrame <- mergeByPosition(df1, df2, 
        msg = 'predictions', ID.colName = ID.colName, TIME.colName = TIME.colName)
    } else {
        warning("No Estimation::Predictions found in the SO; ",
            "the resulting data frame will not contain these")
    }
      
    if (ResidualsSlotName %in% populatedSlots) {
        # Fetch and merge Residuals 
        df1 <- mergedDataFrame
        df2 <- as.data.frame(SOObject@Estimation@Residuals@ResidualTable@data)
        # Test to see if data rows are the same, if not remove dose rows from the 
        # input data (df1) and recompare.
        df1 <- checkDoseRows(df1, df2, label1="rawData+Predictions", label2="Residuals", 
            extraInfo = "Residuals data does not currently contain dose rows in output from Nonmem executions.")
        mergedDataFrame <- mergeByPosition(df1, df2, 
            msg = 'residuals', ID.colName = ID.colName, TIME.colName = TIME.colName)
    } else {
        warning("No Estimation::Residuals found in the SO; ", 
            "the resulting data frame will not contain these")
    }

    if (IndivEstEstMeanSlotName %in% populatedSlots) {
        # IndividualEstimates, Estimates
        df1 <- mergedDataFrame
        df2 <- as.data.frame(SOObject@Estimation@IndividualEstimates@Estimates@Mean@data)
        mergedDataFrame <- mergeCheckColumnNames(df1, df2, 
            ID.colName=ID.colName, TIME.colName=TIME.colName)
    } else {
        warning("No Estimation::IndividualEstimates::Estimates::Mean found in the SO; ", 
            "the resulting data frame will not contain these")
    }

    if (IndivEstRandomEffectsMeanSlotName %in% populatedSlots) {
        # IndividualEstimates, RandomEffects
        df1 <- mergedDataFrame
        df2 <- as.data.frame(SOObject@Estimation@IndividualEstimates@RandomEffects@EffectMean@data)
        mergedDataFrame <- mergeCheckColumnNames(df1, df2, 
           ID.colName=ID.colName, TIME.colName=TIME.colName)
    } else {
        warning("No Estimation::IndividualEstimates::RandomEffects::EffectMean found in the SO; ", 
            "the resulting data frame will not contain these")
    }
    return(mergedDataFrame)
  }
)


# ========================= #
# Convert to Xpose Database #
# ========================= #
#' as.xpdb
#'
#'  Method to Fetch all relevant data and convert to Xpose database object.
#' 
#' @export
setGeneric(name="as.xpdb",
    def=function(SOObject, inputDataPath) {
    standardGeneric("as.xpdb")
    }
)
setMethod(f="as.xpdb",
    signature=signature(SOObject="StandardOutputObject", inputDataPath="character"),
    definition=function(SOObject, inputDataPath)
    {
        # create Merged data frame
        xpose4_dataFrame <- as.data(SOObject, inputDataPath)
        
        # Assert that all "ETA_" column names are upper case so that they are compatible with expose. 
        # eta_cols_idx <- grep('ETA_', colnames(xpose4_dataFrame), ignore.case=TRUE)
        # colnames(xpose4_dataFrame)[eta_cols_idx] <- toupper(colnames(xpose4_dataFrame)[eta_cols_idx])
        
        library("xpose4")
        # CREATE new Xpose database
        myXpdb<-new("xpose.data",Runno=0,Doc=NULL)
        
        # TODO: Possibly need to check data types here

        ## Map data.out to xpdb@Data
        Data(myXpdb) <- xpose4_dataFrame
        
        ## Update xpdb@Prefs@Xvardef (variable definitions)
        ###################################################
        
        # Extract name for ID and TIME column 
        PredictionsSlotName <- "Estimation::Predictions"
        ResidualsSlotName <- "Estimation::Residuals::ResidualTable"
        res <- extractIdandIdvNames(SOObject, PredictionsSlotName, ResidualsSlotName)
        ID.colName <- res[["ID.colName"]]
        TIME.colName <- res[["TIME.colName"]]

        myXpdb@Prefs@Xvardef$id <- ID.colName
        myXpdb@Prefs@Xvardef$idv <- TIME.colName

        # Note the below values are hard coded at current as it is not obvious where to look these up from in the SO
        myXpdb@Prefs@Xvardef$occ <-NA
        myXpdb@Prefs@Xvardef$dv <- "DV"
        myXpdb@Prefs@Xvardef$pred <- "PRED"
        myXpdb@Prefs@Xvardef$ipred <- "IPRED"
        myXpdb@Prefs@Xvardef$wres <- "WRES"
        myXpdb@Prefs@Xvardef$iwres <- "IWRES"
                    
        params <- setdiff(names(SOObject@Estimation@IndividualEstimates@Estimates@Mean@description), ID.colName)
        randpar <- setdiff(names(SOObject@Estimation@IndividualEstimates@RandomEffects@EffectMean@description), ID.colName)
        myXpdb@Prefs@Xvardef$parms <- params
        myXpdb@Prefs@Xvardef$ranpar <- randpar

        # Note that covariates are not currently stored in the SO so these are left unfilled for now. Possible to get these
        # from the cotab datafile in the case of Nonmem executions, though there is no reference to this file in RawResults
        # slot at current. 
        #
        # myXpdb@Prefs@Xvardef$covariates <- covariates

        ## Ideally would also update xpdb@Prefs@Labels (variable labels for plots)
        myXpdb@Prefs@Labels$OCC <- NA
        
        #####################################################

        ## update runno
        myXpdb@Runno <- 1

        return(myXpdb)
    }
)

# summary of object

setMethod("head", 
    signature("StandardOutputObject"), 
    function(x, ...) {
    popEst <- slot(object = x, 
            name = "Estimation")@PopulationEstimates$MLE$data[1, , drop = TRUE]
    mode(popEst) <- "numeric"
    list(`class(x)` = "S4 object of class StandardOutputObject",
        `x@Estimation@PopulationEstimates$MLE$data` = popEst,
        `x@Estimation@Residuals$ResidualTable$data` = head(slot(object = x, 
            name = "Estimation")@Residuals$ResidualTable$data, ...))
})

