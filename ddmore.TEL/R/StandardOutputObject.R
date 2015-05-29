# ====================== #
# Standard Output Object #
# ====================== #
# 
# This file contains all code for the top level S4 class of StandardOutputObject 
# and the associated getting/setting and loading PharmML methods. 
#
# The subclasses that make up the individual slots of StandardOutputObject are stored 
# in a separate file called StandardOutputSubClasses.R
#
# Author: cmusselle

###############################################################################
#' The Standard Output Object Class (S4) 
#'
#' The initial design here is to have a differnt subclasss for each SO section: 
#' 
#' @include StandardOutputSubClasses.R 
#'
#' @slot ToolSettings A list object to capture settings from executed tool.
#' @slot RawResults An object of S4 class "RawResults"
#' @slot Estimation An object of S4 class "Estimation" to house all data associated 
#' with population and individual estimates, precision, rediduals, predictions, 
#' liklihoods and output messages (and error messages) from individual modeling 
#' software. 
#' @slot TaskInformation list (nested) containing all standard output messages from 
#' the task executed.
#' @slot ModelDiagnosticEvaluation An object of S4 class "ModelDiagnosticEvaluation" 
#' to house all data associated with model diagnostics and model comparisons.
#' @slot SimulationExploration An object of S4 class "SimulationExploration" 
#' to house all data associated with ... 
#' @slot OptimalDesign An object of S4 class "OptimalDesign" 
#' to house all data associated with ...  
#' 
#' @author cmusselle
#' @include StandardOutputSubClasses.R 
setClass("StandardOutputObject", 
  # Define the slots
  slots=c(
    ToolSettings = "list", 
    RawResults = "RawResults",
    TaskInformation = "list",
    Estimation = "Estimation",
    ModelDiagnostic = "ModelDiagnostic",
    Simulation = "Simulation",
    OptimalDesign = "OptimalDesign"
    ), 
  # Set Default Values to blank instances of the subclasses
  prototype = list(
    ToolSettings = list(),
    RawResults = new("RawResults"),
    TaskInformation = list( Messages = list("Errors"=NULL, 
      "Warnings"=NULL, "Termination"=NULL, "Info"=NULL) ),
  	Estimation = new("Estimation"),
    ModelDiagnostic = new("ModelDiagnostic"),
  	Simulation = new("Simulation"),
	OptimalDesign = new("OptimalDesign")
  	),
  # Validity Checking Function 
  validity = function(object) {
    stopifnot(class(object@ToolSettings)=="list")
    stopifnot(class(object@RawResults)=="RawResults")
    stopifnot(class(object@TaskInformation)=="list")
	  stopifnot(class(object@Estimation)=="Estimation")
	  stopifnot(class(object@ModelDiagnostic)=="ModelDiagnostic")
	  stopifnot(class(object@Simulation)=="Simulation")
	  stopifnot(class(object@OptimalDesign)=="OptimalDesign")
	  return(TRUE)
	}
)

#' createSOObject
#'
#' Create a new empty StandardOutput object 
#'
#' @return An S4 Object of class "StandardOutput".
#'
#' @export
#' @docType methods
#' @rdname createSOObject
createSOObject <- function(...) {

  SO <- new("StandardOutputObject")
  # if (missing()) { 
  # } 
  return(SO)
}


# ========================== #
# Reader for RawData files   #
# ========================== #
#' readRawData 
#'
#'  
#'
#'
#'
setGeneric(name="readRawData",
           def=function(SOObject, fileIndex)
          {
             standardGeneric("readRawData")
          }
          )
setMethod(f="readRawData",
          signature="StandardOutputObject",
          definition=function(SOObject, fileIndex)
          {

          # Mappers from id to file description
          id2FileType <- lapply(SOObject@RawResults@Files, function(x) x[["Description"]])
          names(id2FileType)<- tolower(names(id2FileType))

          fileType2Id <- as.list(names(id2FileType))
          names(fileType2Id) <- tolower(id2FileType)

          fileIndex <- tolower(fileIndex)

          # fileIndex can be file id or file description
          if (fileIndex %in% names(id2FileType)){
            fileElement <- SOObject@RawResults@Files[[fileIndex]]
          } else if (fileIndex %in% names(fileType2Id)){           
            fileElement <- SOObject@RawResults@Files[[fileType2Id[[fileIndex]]]]
          } else {
            stop(paste0("File Reference: ",
              fileIndex," not found for either file id or file description"))
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
#'
#' @export
mergeByPosition <- function(df1, df2, msg='') {

  # Find ID column position in df2
  ID.col = toupper(names(df2)) == "ID"
  TIME.col = toupper(names(df2)) == "TIME"

  # Type conversion Checks for ID column
  if (class(df2[, ID.col]) == "factor") {
      df2[, ID.col] = as.character(df2[, ID.col])
  } 
  df2[, ID.col] = as.numeric(df2[, ID.col])

  # Type conversion Checks for TIME column
  if (class(df2[, TIME.col]) == "factor") {
      df2[, TIME.col] = as.character(df2[, TIME.col])
  } 
  df2[, TIME.col] = as.numeric(df2[, TIME.col])

  # Check ID column is the same on df1 and df2
  if (all(df1[, "ID"] != df2[, ID.col])) {
    stop(paste("ID column order of df1 and df2 does not match when merging", msg))
  }

  # Check TIME column is the same on df1 and df2 
  if (all(df1[, "TIME"] != df2[, TIME.col])) {
    stop(paste("TIME column order of df1 and df2 does not match when merging", msg))
  }

  # merge all unique columns between df1 and df2
  unique.names.df2 = setdiff(names(df2),c("ID","TIME"))
  mergedDf = cbind(df1, df2[, unique.names.df2], deparse.level = 0)
  # Update column names
  #names(mergedDf) <- c(names(df1), unique.names.df2)

  return(mergedDf)
}


# ============================= #
# Convert to single Data Frame  #
# ============================= #
#' as.data
#'
#'  Method to Fetch all relevant data and return a merged data.frame onject.
#'
#' @include read.R
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
				  rawData <- read.NONMEMDataSet(inputDataPath)
          # Checks for Column format
				  rawData[["ID"]] <- as.numeric(rawData[["ID"]]) 
				  rawData[["TIME"]] <- as.numeric(rawData[["TIME"]]) 
          names(rawData) <- toupper(names(rawData))

          # Reorder data frame to have ID and TIME column as first two. 
          ID.col = names(rawData) == "ID"
          TIME.col = names(rawData) == "TIME"
          remaining.names = setdiff(names(rawData),c("ID","TIME"))
          rawData = cbind(rawData[, ID.col], 
                          rawData[, TIME.col], 
                          rawData[, remaining.names],
                          deparse.level = 0)
          # Update names for first two columns 
          names(rawData) <- c(c("ID", "TIME"), remaining.names) 
			  }
			  
			  # Test to see if data rows are the same, if not remove dose rows from the 
			  # input data and recompare.
			  if (nrow(rawData) > nrow(SOObject@Estimation@Predictions$data)) {
				  rawData <- rawData[!is.na(rawData[['DV']]), ]
			  }
			  if (nrow(rawData) != nrow(SOObject@Estimation@Predictions$data)) {
				  print(paste("Number of Rows in Raw Data: ", nrow(rawData)))
				  print(paste("Number of Rows in Predictions: ", nrow(SOObject@Estimation@Predictions$data)))
				  stop("Number of non-dose rows in raw data is different to those in SO Predictions")
			  }
			  
			  mergedDataFrame <- rawData
			  
			  if (!is.null(SOObject@Estimation@Predictions)) {
				  # Fetch and merge Predictions 
				  df1 <- mergedDataFrame
				  df2 <- SOObject@Estimation@Predictions$data
				  mergedDataFrame <- mergeByPosition(df1, df2, 'predictions')
			  } else {
				  warning("No Estimation::Predictions found in the SO; the resulting data frame will not contain these")
			  }
			  
			  if (!is.null(SOObject@Estimation@Residuals)) {
				  # Fetch and merge Residuals 
				  df1 <- mergedDataFrame
				  df2 <- SOObject@Estimation@Residuals$ResidualTable$data
				  mergedDataFrame <- mergeByPosition(df1, df2, 'residuals')
			  } else {
				  warning("No Estimation::Residuals found in the SO; the resulting data frame will not contain these")
			  }
			  
			  if (!is.null(SOObject@Estimation@IndividualEstimates$Estimates$Mean)) {
				  # IndividualEstimates, Estimates
				  df1 <- mergedDataFrame
				  df2 <- SOObject@Estimation@IndividualEstimates$Estimates$Mean$data
				  mergedDataFrame <- merge(df1, df2)
			  } else {
				  warning("No Estimation::IndividualEstimates::Estimates::Mean found in the SO; the resulting data frame will not contain these")
			  }
			  
			  if (!is.null(SOObject@Estimation@IndividualEstimates$RandomEffects$EffectMean)) {
				  # IndividualEstimates, RandomEffects
				  df1 <- mergedDataFrame
				  df2 <- SOObject@Estimation@IndividualEstimates$RandomEffects$EffectMean$data
				  mergedDataFrame <- merge(df1, df2)
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
           def=function(SOObject, inputDataPath)
          {
             standardGeneric("as.xpdb")
          }
          )
setMethod(f="as.xpdb",
          signature=signature(SOObject="StandardOutputObject", inputDataPath="character"),
          definition=function(SOObject, inputDataPath)
          {

            # create Merged data frame
            xpose4_dataFrame <- as.data(SOObject, inputDataPath)

            library('xpose4')
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
			# TODO: Need to determine if we do need to override the column names here (in
			# some generic manner), or whether we leave it up to the user to set them in
			# the Xpose object as and when required.
#            myXpdb@Prefs@Xvardef$parms <- c("V","CL","KA","TLAG")
#            myXpdb@Prefs@Xvardef$covariates <- "logtWT"
#            myXpdb@Prefs@Xvardef$ranpar <- c("ETA_V","ETA_CL","ETA_KA","ETA_TLAG")
            
            ## Ideally would also update xpdb@Prefs@Labels (variable labels for plots)
            #myXpdb@Prefs@Labels
			myXpdb@Prefs@Labels$OCC <- NA
            
            #####################################################

            ## update runno
            myXpdb@Runno <- 1

            return(myXpdb)
          }
          )


