# ====================== #
# Standard Output Object #
# ====================== #
# 
# This file contains all code for the top level S4 class of StandardOutputObject 
# and the associated gettiing/setting and loading PharmML methods. 
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
 #   ModelDiagnosticEvaluation = "ModelDiagnosticEvaluation",
    SimulationExploration = "SimulationExploration",
    OptimalDesign = "OptimalDesign"
    ), 
  # Set Default Values to blank instances of the subclases
  prototype = list(
    ToolSettings = list(),
    RawResults = new("RawResults"),
    TaskInformation = list( Messages = list("Errors"=NULL, 
      "Warnings"=NULL, "Termination"=NULL, "Info"=NULL) ),
  	Estimation = new("Estimation"),
  #	ModelDiagnosticEvaluation = new("ModelDiagnosticEvaluation"),
  	SimulationExploration = new("SimulationExploration"),
	OptimalDesign = new("OptimalDesign")
  	),
  # Validity Checking Function 
  validity = function(object) {
    stopifnot(class(object@ToolSettings)=="list")
    stopifnot(class(object@RawResults)=="RawResults")
    stopifnot(class(object@TaskInformation)=="list")
	  stopifnot(class(object@Estimation)=="Estimation")
	 # stopifnot(class(object@ModelDiagnosticEvaluation)=="ModelDiagnosticEvaluation")
	  stopifnot(class(object@SimulationExploration)=="SimulationExploration")
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
#' @rdname createMogObj
createSOObject <- function(...) {

  SO <- new("StandardOutputObject")
  # if (missing()) { 
  # } 
  return(SO)
}

#' LoadSOObject
#'
#'   Parse in the SO Object of a PharmML .xml file and return it as a new StandardOutputObject.
#'
#' @param file The relative path to the .xml file. 
#' 
#' @value Returns a newly created StandardOutputObject class populated with all entries in the SO section of the PharmML file.  
#' @export
#' @include xmlParsers.R
LoadSOObject <- function(file) {

  # Error Checking
  stopifnot(class(file) == "character" & file.exists(file))

  # Set working directory to that specified in file 
  old.wd <- getwd()
  dir <- dirname(file)
  f.name <- basename(file)
  setwd(dir)

  # Generate Blank SO object 
  SOObject <- createSOObject()

  # Get a reference to the root node in the xml doc
  root <- xmlRoot(xmlTreeParse(f.name))

  # Update the namespace to use 'd' as the defualt. 
  # Must be done to use Xpath expressions with namespaces
  ns <- xmlNamespaceDefinitions(root, simplify = TRUE)
  defaultNS <- 'd'
  names(ns)[1] <- defaultNS
  
  # Fetch List of SOBlock elements
  # Supress namespace undefined messages
  sink("NUL")
  SOlist <- xpathApply(root, "//d:SOBlock", namespaces=ns)
  sink()

  # Assumes only one SOBlock for now!
  stopifnot(length(SOlist) == 1)

  ### Future for loop to start here i multiple SO blocks

  # Fetch all Components of the SO object that are defined
  SOChildren <- xmlChildren(SOlist[[1]])

  # Error Checking of unexpected elements
  expectedTags = c("ToolSettings", "RawResults", "TaskInformation", "Estimation", 
                  "Simulation")
  unexpected = setdiff(names(SOChildren), expectedTags)
  if (length(unexpected) != 0) {
    warning(paste("The following unexpected elements were detected in the PharmML SO.", 
              paste(unexpected, collapse="\n      "), sep="\n      "))
  }

  # Error checking of expected XML structure + Parser Execution
  if ("ToolSettings" %in% names(SOChildren)){
    SOObject <- ParseToolSettings(SOObject, SOChildren[["ToolSettings"]])
  } else {
    warning("ToolSettings element not detected in PharmML. Skipping...")
  }

  if ("RawResults" %in% names(SOChildren)){
    SOObject <- ParseRawResults(SOObject, SOChildren[["RawResults"]])
  } else {
    warning("RawResults element not detected in PharmML. Skipping...")
  }

  if ("TaskInformation" %in% names(SOChildren)){
    SOObject <- ParseTaskInformation(SOObject, SOChildren[["TaskInformation"]])
  } else {
    warning("TaskInformation element not detected in PharmML. Skipping...")
  }

  if ("Estimation" %in% names(SOChildren)){

      # Error Checking of unexpected elements in Estmation Block
      expectedTags = c("PopulationEstimates", "PrecisionPopulationEstimates", 
        "IndividualEstimates", "Residuals", "Predictions", "Likelihood")
      unexpected = setdiff(names(SOChildren[["Estimation"]]), expectedTags)
      if (length(unexpected) != 0) {
        warning(paste("The following unexpected elements were detected in the Estimation block of the PharmML SO.", 
              paste(unexpected, collapse="\n      "), sep="\n      "))
      }

      if ("PopulationEstimates" %in% names(SOChildren[["Estimation"]])){
        SOObject <- ParsePopulationEstimates(SOObject, SOChildren[["Estimation"]][["PopulationEstimates"]])
      } else {
        warning("PopulationEstimates element not detected in PharmML. Skipping...")
      }

      if ("PrecisionPopulationEstimates" %in% names(SOChildren[["Estimation"]])){
        SOObject <- ParsePrecisionPopulationEstimates(SOObject, SOChildren[["Estimation"]][["PrecisionPopulationEstimates"]])
      } else {
        warning("PrecisionPopulationEstimates element not detected in PharmML. Skipping...")
      }

      if ("IndividualEstimates" %in% names(SOChildren[["Estimation"]])){
        SOObject <- ParseIndividualEstimates(SOObject, SOChildren[["Estimation"]][["IndividualEstimates"]])
      } else {
        warning("IndividualEstimates element not detected in PharmML. Skipping...")
      }

      if ("Residuals" %in% names(SOChildren[["Estimation"]])){
        SOObject <- ParseResiduals(SOObject, SOChildren[["Estimation"]][["Residuals"]])
      } else {
        warning("Residuals element not detected in PharmML. Skipping...")
      }

      if ("Predictions" %in% names(SOChildren[["Estimation"]])){
        SOObject <- ParsePredictions(SOObject, SOChildren[["Estimation"]][["Predictions"]])
      } else {
        warning("Predictions element not detected in PharmML. Skipping...")
      }

      if ("Likelihood" %in% names(SOChildren[["Estimation"]])){
        SOObject <- ParseLikelihood(SOObject, SOChildren[["Estimation"]][["Likelihood"]])
      } else {
        warning("Likelihood element not detected in PharmML. Skipping...")
      }

  } else {
    warning("Estimation element not detected in PharmML. Skipping...")
  }

  # Run validation functions on S4 Class and subclasses
  validObject(SOObject)
  validObject(SOObject@RawResults)
  validObject(SOObject@Estimation)
  validObject(SOObject@SimulationExploration)
  validObject(SOObject@SimulationExploration)
  validObject(SOObject@OptimalDesign)

  # Reset Working directory 
  setwd(old.wd)

  return(SOObject)
}

# ================ #
# Getter Methods   #
# ================ #

#' Create a method to fetch the value of ToolSetting Slot
setGeneric(name="getToolSettings",
           def=function(SOObject)
           {
                   standardGeneric("getToolSettings")
           }
)
setMethod(f="getToolSettings",
          signature="StandardOutputObject",
          definition=function(SOObject)
          {                              
            ToolSettings <- SOObject@ToolSettings
            pprintList(ToolSettings, "Tool Settings")
          }
)

#' Create a method to fetch the value of RawResults Slot
setGeneric(name="getRawResults",
           def=function(SOObject)
           {
                   standardGeneric("getRawResults")
           }
)
setMethod(f="getRawResults",
          signature="StandardOutputObject",
          definition=function(SOObject)
          {                              
            RawResults <- SOObject@RawResults@Files
            pprintList(RawResults, "Raw Results")  
          }
)

#' Create a method to fetch the value of PopulationEstimates Slot
setGeneric(name="getPopulationEstimates",
           def=function(SOObject)
           {
              standardGeneric("getPopulationEstimates")
           }
)
setMethod(f="getPopulationEstimates",
          signature="StandardOutputObject",
          definition=function(SOObject)
          {     
            PopulationEstimates <- SOObject@Estimation@PopulationEstimates
            pprintList(PopulationEstimates, "Population Estimates")
          }
)

#' Create a method to fetch the value of PrecisionPopulationEstimates Slot
setGeneric(name="getPrecisionPopulationEstimates",
           def=function(SOObject)
           {
              standardGeneric("getPrecisionPopulationEstimates")
           }
)
setMethod(f="getPrecisionPopulationEstimates",
          signature="StandardOutputObject",
          definition=function(SOObject)
          {                              
          PrecisionPopulationEstimates <- SOObject@Estimation@PrecisionPopulationEstimates
          pprintList(PrecisionPopulationEstimates, "Precision Population Estimates")
          }
)

#' Create a method to fetch the value of IndividualEstimates Slot
setGeneric(name="getIndividualEstimates",
           def=function(SOObject)
           {
              standardGeneric("getIndividualEstimates")
           }
)
setMethod(f="getIndividualEstimates",
          signature="StandardOutputObject",
          definition=function(SOObject)
      {  
        IndividualEstimates <- SOObject@Estimation@IndividualEstimates
        pprintList(IndividualEstimates, "Individual Estimates")                           
      }                              
)

#' Create a method to fetch the value of PrecisionIndividualEstimates Slot
setGeneric(name="getPrecisionIndividualEstimates",
           def=function(SOObject)
           {
              standardGeneric("getPrecisionIndividualEstimates")
           }
)
setMethod(f="getPrecisionIndividualEstimates",
                      signature="StandardOutputObject",
                      definition=function(SOObject)
   {                              
      PrecisionIndividualEstimates <- SOObject@Estimation@PrecisionIndividualEstimates
      pprintList(PrecisionIndividualEstimates, "Precision Individual Estimates")
   }                                                     
)

#' Create a method to fetch the value of Residuals Slot
setGeneric(name="getResiduals",
           def=function(SOObject)
           {
              standardGeneric("getResiduals")
           }
)
setMethod(f="getResiduals",
                      signature="StandardOutputObject",
                      definition=function(SOObject)
    {
      Residuals <- SOObject@Estimation@Residuals
      pprintList(Residuals, "Residuals")
    }
)

#' Create a method to fetch the value of Predictions Slot
setGeneric(name="getPredictions",
           def=function(SOObject)
           {
              standardGeneric("getPredictions")
           }
)
setMethod(f="getPredictions",
          signature="StandardOutputObject",
          definition=function(SOObject)
         {
           Predictions <- SOObject@Estimation@Predictions
           pprintList(Predictions, "Predictions")
         }                              
)

#' Create a method to fetch the value of Likelihood Slot
setGeneric(name="getLikelihood",
           def=function(SOObject)
           {
              standardGeneric("getLikelihood")
           }
)
setMethod(f="getLikelihood",
          signature="StandardOutputObject",
          definition=function(SOObject)
          { 
           Likelihood <- SOObject@Estimation@Likelihood
           pprintList(Likelihood, "Likelihood")
          }
)

#' Create a method to fetch the value of SoftwareMessages Slot
setGeneric(name="getSoftwareMessages",
                       def=function(SOObject)
                       {
                          standardGeneric("getSoftwareMessages")
                       }
                       )
setMethod(f="getSoftwareMessages",
          signature="StandardOutputObject",
          definition=function(SOObject)
          {                              
           SoftwareMessages <- SOObject@Estimation@SoftwareMessages
           pprintList(SoftwareMessages, "Software Messages")
          }
)

# ============================= #
# Higher Level Getter Functions #
# ============================= #
#' getEstimationInfo
#'
#' This function acts on an object of class StandardOutputObject 
#' and is a wrapper to getLikelihood and getSoftwareMessages presenting information
#' to the user about the estimation process including any warnings, errors from 
#' the log and, depending on the target software and estimation method, values 
#' such as Objective Function Value (OFV), -2*log-likelihood, Information criteria
#' such as AIC, DIC, BIC. 
#'
#' @param object an object of class StandardOutputObject, the output from an 
#' estimation task.
#'
#' @return A named list.
#'
#' @examples getEstimationInfo(object)
#'
#' @seealso getLikelihood, getSoftwareMessages
#'
#' @export 
getEstimationInfo <- function(SOObject){
  
  likelihood <- getLikelihood(SOObject)
  messages <- getSoftwareMessages(SOObject)

  invisible(list(likelihood=likelihood, messages=messages))
}

#' getParameterEstimates
#' 
#' This function acts on an object of class StandardOutputObject and is a wrapper 
#' to getPopulationEstimates and getPrecisionPopulationEstimates presenting 
#' estimates of the STRUCTURAL and RANDOM_VARIABILITY parameters along with any 
#' associated measures of uncertainty (Std Dev) and interval estimates if these 
#' are available.
#' Usage
#' 
#' @param SOObject an object of class StandardOutputObject, the output from an 
#'    estimation task.
#' @param type character string determining which parameters to return. 
#'    Options are “structural”, “variability”, “all” (default).
#' @param what character vector determining what values to return:
#'  \itemize{
#'    \item “estimates” – returns point estimates for the parameters.
#'    \item “precision” – returns variability / uncertainty estimates for the 
#'        parameters.
#'    \item “intervalEstimates” - returns interval estimates for the parameters. 
#'    \item “all” – returns all of the above in a table.}
#' 
#' The values available for return from the StandardOutputObject depend on the 
#' estimation method used and how these are populated – either directly from the
#' estimation task output or subsequently via other methods e.g. bootstrapping. 
#' The function will provide suitable names for the columns depending on the 
#' methods used. So for example, although the argument for “what” accepts 
#' “precision” this may mean SE assuming Normality, Bootstrap SE, SD from MCMC 
#' or SAEM estimation methods.
#' 
#' @return If only returning “estimates” or “precision” then a named vector of 
#' real values. If returning “intervalEstimates” or “All” then a data frame 
#' containing one row for each parameter. Columns are “Estimate”, “Precision”, 
#' “Lower”, “Upper”, “Shrinkage”
#' 
#' @examples getParameterEstimates(object, type=”all”, what=”all”)
#'
#' @seealso getPopulationEstimates, getPrecisionPopulationEstimates
#'
#' @export
getParameterEstimates <- function(SOObject, type="all", what="all"){
  
  output <- list()

  if (type== "estimates" | type == "all") {

    PopulationEstimates <- getPopulationEstimates(SOObject)
    output <- c(output, list(PopulationEstimates=PopulationEstimates))

  } 
  if (type== "precision" | type == "all") {
    PrecisionPopulationEstimates <- getPrecisionPopulationEstimates(SOObject)
    output <- c(output, list(PrecisionPopulationEstimates=PrecisionPopulationEstimates))
  
  }
  if (type== "intervalEstimates" | type == "all") {

  }

  invisible(output)
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
#'  Utility used bt as.data to check column types and then merge by possition
#'
#' @export
mergeByPosition <- function(df1, df2, msg='') {

  # Type conversion Checks fro column 1
  if (class(df2[, 1]) == "factor") {
      df2[, 1] = as.numeric(as.character(df2[, 1]))
  } else if (class(df2[, 1]) == "integer") {
      df2[, 1] = as.numeric(df2[, 1])
  } else if (class(df2[, 1]) == "character") {
    df2[, 1] = as.numeric(df2[, 1])
  } else if (class(df2[, 1]) == "numeric") {
    df2[, 1] = df2[, 1]
  }

  # Type conversion Checks fro column 2
  if (class(df2[, 2]) == "factor") {
      df2[, 2] = as.numeric(as.character(df2[, 2]))
  } else if (class(df2[, 2]) == "integer") {
      df2[, 2] = as.numeric(df2[, 2])
  } else if (class(df2[, 2]) == "character") {
    df2[, 2] = as.numeric(df2[, 2])
  } else if (class(df2[, 2]) == "numeric") {
    df2[, 2] = df2[, 2]
  }

  # Check ID column is the same on df1 and two
  if (all(df1[, 1] != df2[, 1])) {
    stop(paste("ID column order of df1 and df2 does not match when merging", msg))
  }

  # Check TIME column is the same on df1 and df2 
  if (all(df1[, 2] != df2[, 2])) {
    stop(paste("TIME column order of df1 and df2 does not match when merging", msg))
  }

  names.df1 = names(df1)
  names.df2 = names(df2)

  new.col.names = c(names.df1, names.df2[3:ncol(df2)])

  mergedDf = cbind(df1, df2[, 3:ncol(df2)], deparse.level = 0)

  names(mergedDf) <- new.col.names

  return(mergedDf)
}


# ============================= #
# Convert to single Data Frame  #
# ============================= #
#' as.data.old
#'
#'  Method to Fetch all relevant data and return a merged data.frame onject.
#'
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
              rawData <- read.csv(inputDataPath, na.strings=".")
              rawData[["ID"]] <- as.numeric(rawData[["ID"]]) 
              rawData[["TIME"]] <- as.numeric(rawData[["TIME"]]) 
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

            # Fetch and merge Predictions 
            df1 <- rawData
            df2 <- SOObject@Estimation@Predictions$data
            mergedDataFrame <- mergeByPosition(df1, df2, 'predictions')

            # Fetch and merge Residuals 
            residuals <- SOObject@Estimation@Residuals
            residualTagNames <- c("WRES", "IWRES", "RES", "IRES")
            for (name in residualTagNames){
              if (name %in% names(residuals)) {
                df1 <- mergedDataFrame
                df2 <- residuals[[name]][["data"]]
                mergedDataFrame <- mergeByPosition(df1, df2, paste("Residual", name))
                }
              } 

            # IndividualEstimates, Estimates
            df1 <- mergedDataFrame
            df2 <- SOObject@Estimation@IndividualEstimates$Estimates$Mean$data
            df2[, 1] <- as.numeric(as.character(df2[, 1]))
            mergedDataFrame <- merge(df1, df2)
            
            # IndividualEstimates, RandomEffects
            df1 <- mergedDataFrame
            df2 <- SOObject@Estimation@IndividualEstimates$RandomEffects$EffectMean$data
            df2[, 1] <- as.numeric(as.character(df2[, 1]))
            mergedDataFrame <- merge(df1, df2)

            return(mergedDataFrame)
          }
          )
# ============================= #
# Convert to single Data Frame  #
# ============================= #
#' as.data.merge
#'
#'  An old implimentation of the as.data method which aims to merge 
#' data frames by matching ID and TIME columns. This approach is problematic 
#' when ID and TIME columns are duplicated, which turns out to happen quite often.  
#'
#'  Method to Fetch all relevant data and return a merged data.frame onject.
#'
#' 
#' @export
setGeneric(name="as.data.merge",
           def=function(SOObject, inputDataPath)
          {
             standardGeneric("as.data.merge")
          }
          )
setMethod(f="as.data.merge",
          signature=signature(SOObject="StandardOutputObject", inputDataPath="character"),
          definition=function(SOObject, inputDataPath)
          {
            if (missing(inputDataPath)){
              stop("Path to input data must be specified")
            } else {
              rawData <- read.csv(inputDataPath, na.strings=".")
              rawData <- rawData[with(rawData, order(TIME, ID)), ]
              rawData[["ID"]] <- as.numeric(rawData[["ID"]]) 
              rawData[["TIME"]] <- as.numeric(rawData[["TIME"]]) 
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

            # Fetch and merge Predictions 
            predictions <- SOObject@Estimation@Predictions$data

            predictions <- predictions[with(predictions, order(TIME, ID)), ]
            predictions[["ID"]] = as.numeric(as.character(predictions[["ID"]]))
            predictions[["TIME"]] = as.numeric(as.character(predictions[["TIME"]]))

            df1 <- rawData
            df2 <- predictions

            # Remove any duplicated rows before the merge
            df2 <- df2[!duplicated(df2), ]

            mergedDataFrame <- merge(df1, df2)

            if (nrow(mergedDataFrame) == 0) {

              print(paste("Number of Rows in Raw Data: ", nrow(df1), 
                " - ID col class:", class(df1[, 1]), " - TIME col class:", class(df1[, 2])))
              print(paste("Number of Rows in Predictions: ", nrow(df2), 
                " - ID col class:", class(df2[, 1]), " - TIME col class:", class(df2[, 2])))
              stop("Merging of predictions has failed. Check format of ID and TIME columns")
            }

            # Fetch and merge Residuals method 1
            residuals <- SOObject@Estimation@Residuals
            residualTagNames <- c("WRES", "IWRES", "RES", "IRES")
            for (name in residualTagNames){
              if (name %in% names(residuals)) {
                df1 <- mergedDataFrame
                df2 <- residuals[[name]][["data"]]
                df2[["ID"]] <- as.numeric(as.character(df2[["ID"]]))
                df2[["TIME"]] <- as.numeric(as.character(df2[["TIME"]]))
                df2 <- df2[!duplicated(df2), ]
                mergedDataFrame <- merge(mergedDataFrame, df2)
              
                if (nrow(mergedDataFrame) == 0) {
                  df2 <- SOObject@Estimation@Residuals[[name]][["data"]]
                  print(paste("Number of Rows in df1: ", nrow(df1), 
                    " - ID col class:", class(df1[, 1]), " - TIME col class:", class(df1[, 2])))
                  print(paste("Number of Rows in df2: ", nrow(df2),
                    " - ID col class:", class(df2[, 1]), " - TIME col class:", class(df2[, 2])))
                  stop("Merging of residuals has failed. Check format of ID and TIME columns")
                }
              } 
            }

            # IndividualEstimates, Estimates
            df1 <- mergedDataFrame
            df2 <- SOObject@Estimation@IndividualEstimates$Estimates$Mean$data
            df2[["ID"]] <- as.numeric(as.character(df2[["ID"]]))
            df2[["TIME"]] <- as.numeric(as.character(df2[["TIME"]]))  
            df2 <- df2[!duplicated(df2), ]
            mergedDataFrame <- merge(df1, df2)
            
            if (nrow(mergedDataFrame) == 0) {
              print(paste("Number of Rows in df1: ", nrow(df1), 
                    " - ID col class:", class(df1[, 1]), " - TIME col class:", class(df1[, 2])))
              print(paste("Number of Rows in df2: ", nrow(df2),
                    " - ID col class:", class(df2[, 1]), " - TIME col class:", class(df2[, 2])))
              stop("Merging of IndividualEstimates-Estimates has failed. Check format of ID and TIME columns")
            }
            
            # IndividualEstimates, RandomEffects
            df1 <- mergedDataFrame
            df2 <- SOObject@Estimation@IndividualEstimates$RandomEffects$EffectMean$data
            df2[["ID"]] <- as.numeric(as.character(df2[["ID"]]))
            df2[["TIME"]] <- as.numeric(as.character(df2[["TIME"]]))
            df2 <- df2[!duplicated(df2), ]
            mergedDataFrame <- merge(df1, df2)

            if (nrow(mergedDataFrame) == 0) {
              print(paste("Number of Rows in df1: ", nrow(df1), 
                    " - ID col class:", class(df1[, 1]), " - TIME col class:", class(df1[, 2])))
              print(paste("Number of Rows in df2: ", nrow(df2),
                    " - ID col class:", class(df2[, 1]), " - TIME col class:", class(df2[, 2])))
              stop("Merging of IndividualEstimates-RandomEffects has failed. Check format of ID and TIME columns")
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
            
            # TODO: Remove i in ID columns for now.
            xpose4_dataFrame$ID <- as.numeric(sub("i", "", xpose4_dataFrame$ID ))

            ## Map data.out to xpdb@Data
            Data(myXpdb)<-xpose4_dataFrame
            
            ## Update xpdb@Prefs@Xvardef (variable definitions)
            ###################################################
            ## TODO: Infer these values from the full PharmML
            
            ## Fill in / Confirm information from PharmML
            myXpdb@Prefs@Xvardef$id<-"ID"
            myXpdb@Prefs@Xvardef$idv<-"TIME"
            myXpdb@Prefs@Xvardef$occ<-NA
            myXpdb@Prefs@Xvardef$dv<-"DV"
            
            ## Fill in / Confirm information from SO
            myXpdb@Prefs@Xvardef$pred<-"PRED"
            myXpdb@Prefs@Xvardef$ipred<-"IPRED"
            myXpdb@Prefs@Xvardef$wres <- "WRES"
            myXpdb@Prefs@Xvardef$iwres <- "IWRES"
            myXpdb@Prefs@Xvardef$parms <- c("V","CL","KA","TLAG")
            myXpdb@Prefs@Xvardef$covariates <- "logtWT"
            myXpdb@Prefs@Xvardef$ranpar <- c("ETA_V","ETA_CL","ETA_KA","ETA_TLAG")
            
            ## Ideally would also update xpdb@Prefs@Labels (variable labels for plots)
            myXpdb@Prefs@Labels
            
            #####################################################

            ## update runno
            myXpdb@Runno <- 1

            return(myXpdb)
          }
          )


