# ====================== #
# Standard Output Object #
# ====================== #
# 
# This file contains all code for the top level S4 class of StandardOutputObject.
# Classes for individual slots within the StandardOutputObject class structure are
# stored in separate files, named xxx-Class.R.
#
# Author: cmusselle, ccampbell, mwise


###############################################################################
#' The Standard Output Object Class (S4) 
#'
#' Contains slots having specific classes for each SO section.
#'
#' @slot ToolSettings A named list containing the settings relating to the execution tool,
#' 					  specifically, relevant file paths
#' @slot RawResults An object of S4 class \linkS4class{RawResults}
#' @slot TaskInformation An object of S4 class \linkS4class{TaskInformation} containing
#' 						 output messages from the task that was executed
#' @slot Estimation An object of S4 class \linkS4class{Estimation}, housing all data
#' 					associated with population and individual estimates, precision,
#' 					residuals, predictions, likelihoods and output messages
#' 					(and error messages) from individual modelling software 
#' @slot ModelDiagnostic An object of S4 class \linkS4class{ModelDiagnostic}
#' 						 housing all data associated with model diagnostics
#' 						 and model comparisons
#' @slot Simulation A list of objects of S4 class \linkS4class{SimulationBlock}
#' 					housing data produced from a simulation task
#' @slot OptimalDesign A list of objects of S4 class \linkS4class{OptimalDesignBlock}
#'					   housing results produced by Optimal Design
#' @slot .pathToSourceXML (Internal) The absolute path to the XML file from which
#' 						  this SO object was created
#' 
#' @author cmusselle

setClass("StandardOutputObject", 
  # Define the slots
  slots=c(
    ToolSettings = "list", 
    RawResults = "RawResults",
    TaskInformation = "TaskInformation",
    Estimation = "Estimation",
    ModelDiagnostic = "ModelDiagnostic",
    Simulation = "list", # of SimulationBlock objects
    OptimalDesign = "list", # of OptimalDesignBlock objects
	# The absolute path to the XML file that was parsed to create this SO
	.pathToSourceXML = "character"
    ),
  # Set Default Values to blank instances of the nested classes
  prototype = list(
    ToolSettings = list(),
    RawResults = new("RawResults"),
    TaskInformation = new("TaskInformation"),
  	Estimation = new("Estimation"),
    ModelDiagnostic = new("ModelDiagnostic"),
  	Simulation = list(),
	OptimalDesign = list(),
	.pathToSourceXML = character(0)
  	),
  # Validity Checking Function 
  validity = function(object) {
    stopifnot(class(object@ToolSettings) == "list")
    stopifnot(class(object@RawResults) == "RawResults")
    stopifnot(class(object@TaskInformation) == "TaskInformation")
	stopifnot(class(object@Estimation) == "Estimation")
    stopifnot(class(object@ModelDiagnostic) == "ModelDiagnostic")
	stopifnot(class(object@Simulation) == "list")
	stopifnot(class(object@OptimalDesign) == "list")
	return(TRUE)
	}
)


#' is.SOObject
#'
#' Determines if an object is of class "StandardOutputObject".
#'
#' @usage is.SOObject(object)
#'
#' @return TRUE or FALSE
#' @export
# Called from jobExecution.R
is.SOObject <- function(x) {
    is(object = x, class2 = "StandardOutputObject")
}


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
        id2FileType <- lapply(SOObject@RawResults@Files, function(x) x[["Description"]])
        names(id2FileType)<- tolower(names(id2FileType))
        
        fileType2Id <- as.list(names(id2FileType))
        names(fileType2Id) <- tolower(id2FileType)
        
        fileIndex <- tolower(fileIndex)
        
        # fileIndex can be file id or file description
        if (fileIndex %in% names(id2FileType)) {
            fileElement <- SOObject@RawResults@Files[[fileIndex]]
        } else { 
            if (fileIndex %in% names(fileType2Id)) {
                fileElement <- SOObject@RawResults@Files[[fileType2Id[[fileIndex]]]]
            } else {
                stop(paste0("File Reference: ",
                    fileIndex," not found for either file id or file description"))
            }
        }
        
        # TODO: Possibly need to add custom NA handelling here 
        rawData <- read.csv(rawFile$path, na.strings=".")
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

# ============================= #
# Convert to single Data Frame  #
# ============================= #
#' as.data
#'
#'  Method to Fetch all relevant data and return a merged data.frame object.
#'
#' @include readDataObj.R
#' @export
setGeneric(name="as.data",
           def=function(SOObject, inputDataPath)
          {
             standardGeneric("as.data")
          }
          )
setMethod(f="as.data",
          signature=signature(SOObject="StandardOutputObject", inputDataPath="character"),
          definition=function(SOObject, inputDataPath)
		  {
			  if (missing(inputDataPath)){
				  stop("Path to input data must be specified")
			  } else {

          # Fetching Column names for the raw data is now dependent on an existing MDL file
          # file existence check is performed by .getMdlInfoFromSO
          MDLObjs <- .getMdlInfoFromSO(SOObject, what="data")

          # Find the variable names being used as ID and TIME
          input.var.use.definitions = lapply(MDLObjs@DATA_INPUT_VARIABLES, FUN= function(x) {x[['use']]})

          ID.index = input.var.use.definitions == "id"
          TIME.index = input.var.use.definitions == "idv"

          ID.colName = toupper(names(input.var.use.definitions[ID.index]))
          TIME.colName = toupper(names(input.var.use.definitions[TIME.index]))

          if (length(ID.colName) > 1) {
            stop(paste0("Multiple DATA_INPUT_VARIABLES have use defined as 'id' in MDL file, ", 
              "cannot determine correct column name for ID from MDL file. "))
          } else if (length(ID.colName) == 0) {
            stop(paste0("No DATA_INPUT_VARIABLES have a 'use' parameter defined as 'id' in the MDL file ", 
              "cannot determine correct column name for ID from MDL file."))
          }
          if (length(TIME.colName) > 1) {
            stop(paste0("Multiple DATA_INPUT_VARIABLES have use defined as 'idv' in MDL file, ", 
              "cannot determine correct column name for TIME from MDL file."))
          } else if (length(TIME.colName) == 0) {
            stop(paste0("No DATA_INPUT_VARIABLES have a 'use' parameter defined as 'idv' in the MDL file ", 
              "cannot determine correct column name for TIME from MDL file."))
          }

          # Pass in the rawData file 
          colNames <- toupper(names(MDLObjs@DATA_INPUT_VARIABLES))
          rawData <- read.NONMEMDataSet(inputDataPath, colNames=colNames)
          # Convert all column headers to upper case 
          names(rawData) <- toupper(names(rawData))

          # Checks for Column format
				  rawData[[ID.colName]] <- as.numeric(rawData[[ID.colName]]) 
				  rawData[[TIME.colName]] <- as.numeric(rawData[[TIME.colName]]) 

          # Reorder data frame to have ID and TIME column as first two. 
          ID.col = names(rawData) == ID.colName
          TIME.col = names(rawData) == TIME.colName
          remaining.names = setdiff(names(rawData),c(ID.colName,TIME.colName))
          rawData = cbind(rawData[, ID.col], 
                          rawData[, TIME.col], 
                          rawData[, remaining.names],
                          deparse.level = 0)
          # Update names for first two columns 
          names(rawData) <- c(ID.colName, TIME.colName, remaining.names) 
			  }
			  
        mergedDataFrame <- rawData

        if (!is.empty(SOObject@Estimation@Predictions)) {
          
          df1 <- mergedDataFrame
          df2 <- SOObject@Estimation@Predictions$data

          # Test to see if data rows are the same, if not remove dose rows from the 
          # input data (df1) and recompare.
          df1 <- checkDoseRows(df1, df2, label1="rawData", label2="Predictions")

          # Fetch and merge Predictions 
          mergedDataFrame <- mergeByPosition(df1, df2, 'predictions', ID.colName=ID.colName, TIME.colName=TIME.colName)
        } else {
          warning("No Estimation::Predictions found in the SO; the resulting data frame will not contain these")
        }
          
			  if (!is.empty(SOObject@Estimation@Residuals)) {
				  # Fetch and merge Residuals 
				  df1 <- mergedDataFrame
				  df2 <- SOObject@Estimation@Residuals$ResidualTable$data

          # Test to see if data rows are the same, if not remove dose rows from the 
          # input data (df1) and recompare.
          df1 = checkDoseRows(df1, df2, label1="rawData+Predictions", label2="Residuals", 
            extraInfo="Residuals data does not currently contain dose rows in output from Nonmem executions.")

				  mergedDataFrame <- mergeByPosition(df1, df2, 'residuals', ID.colName=ID.colName, TIME.colName=TIME.colName)
			  } else {
				  warning("No Estimation::Residuals found in the SO; the resulting data frame will not contain these")
			  }
			  
			  if (!is.null(SOObject@Estimation@IndividualEstimates$Estimates$Mean)) {
				  # IndividualEstimates, Estimates
				  df1 <- mergedDataFrame
				  df2 <- SOObject@Estimation@IndividualEstimates$Estimates$Mean$data
				  mergedDataFrame <- mergeCheckColumnNames(df1, df2, ID.colName=ID.colName, TIME.colName=TIME.colName)
			  } else {
				  warning("No Estimation::IndividualEstimates::Estimates::Mean found in the SO; the resulting data frame will not contain these")
			  }
			  
			  if (!is.null(SOObject@Estimation@IndividualEstimates$RandomEffects$EffectMean)) {
				  # IndividualEstimates, RandomEffects
				  df1 <- mergedDataFrame
				  df2 <- SOObject@Estimation@IndividualEstimates$RandomEffects$EffectMean$data
				  mergedDataFrame <- mergeCheckColumnNames(df1, df2, ID.colName=ID.colName, TIME.colName=TIME.colName)
			  } else {
				  warning("No Estimation::IndividualEstimates::RandomEffects::EffectMean found in the SO; the resulting data frame will not contain these")
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
        eta_cols_idx <- grep('ETA_', colnames(xpose4_dataFrame), ignore.case=TRUE)
        colnames(xpose4_dataFrame)[eta_cols_idx] <- toupper(colnames(xpose4_dataFrame)[eta_cols_idx])
        
        library("xpose4")
        # CREATE new Xpose database
        myXpdb<-new("xpose.data",Runno=0,Doc=NULL)
        
        # TODO: Possibly need to check data types here

        ## Map data.out to xpdb@Data
        Data(myXpdb)<-xpose4_dataFrame
        
        ## Update xpdb@Prefs@Xvardef (variable definitions)
        ###################################################
        ## TODO: Infer these values from the full PharmML
        
    #            ## Fill in / Confirm information from PharmML
    #            myXpdb@Prefs@Xvardef$id<-"ID"
    #            myXpdb@Prefs@Xvardef$idv<-"TIME"
        myXpdb@Prefs@Xvardef$occ<-NA
    #            myXpdb@Prefs@Xvardef$dv<-"DV"
    #            
    #            ## Fill in / Confirm information from SO
        myXpdb@Prefs@Xvardef$pred<-"PRED"
        myXpdb@Prefs@Xvardef$ipred<-"IPRED"
        myXpdb@Prefs@Xvardef$wres <- "WRES"
        myXpdb@Prefs@Xvardef$iwres <- "IWRES"

            # Below are the hard-coded column definitions for the Warfarin-latest-ODE model.
            # These are slightly different to the default ones that are assigned; so this
            # may give rise to errors such as "ETAs are not properly set in the database"
            # for certain plots.
    #            myXpdb@Prefs@Xvardef$parms <- c("V","CL","KA","TLAG")
    #            myXpdb@Prefs@Xvardef$covariates <- "logtWT"
    #            myXpdb@Prefs@Xvardef$ranpar <- c("ETA_V","ETA_CL","ETA_KA","ETA_TLAG")
                    
        # TODO: Temporary workaround is to find model parameters from MDL file to populate 
        # The Xvardef slot of the xpdb object
        obj <- .getMdlInfoFromSO(SOObject, what='model')
        params = sapply(obj@INDIVIDUAL_VARIABLES, FUN=function(x) x$name ) 
        covariates = sapply(obj@COVARIATES, FUN=function(x) x$name )
        randpar = sapply(obj@RANDOM_VARIABLE_DEFINITION, FUN=function(x) x$name ) 

        myXpdb@Prefs@Xvardef$parms <- toupper(params)
        myXpdb@Prefs@Xvardef$covariates <- toupper(covariates)
        myXpdb@Prefs@Xvardef$ranpar <- toupper(randpar)

        ## Ideally would also update xpdb@Prefs@Labels (variable labels for plots)
        #myXpdb@Prefs@Labels
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

