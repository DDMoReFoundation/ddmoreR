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
    Estimation = "Estimation",
 #   ModelDiagnosticEvaluation = "ModelDiagnosticEvaluation",
    SimulationExploration = "SimulationExploration",
    OptimalDesign = "OptimalDesign"
    ), 
  # Set Default Values to blank instances of the subclases
  prototype = list(
    ToolSettings = list(),
    RawResults = new("RawResults"),
  	Estimation = new("Estimation"),
  #	ModelDiagnosticEvaluation = new("ModelDiagnosticEvaluation"),
  	SimulationExploration = new("SimulationExploration"),
	OptimalDesign = new("OptimalDesign")
  	),
  # Validity Checking Function 
  validity = function(object) {
    stopifnot(class(object@ToolSettings)=="list")
    stopifnot(class(object@RawResults)=="RawResults")
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

  # Generate Blank SO object 
  SOObject = createSOObject()

  # Get a reference to the root node in the xml doc
  root = xmlRoot(xmlTreeParse(file))

  # Update the namespace to use 'd' as the defualt. 
  # Must be done to use Xpath expressions with namespaces
  ns = xmlNamespaceDefinitions(root, simplify = TRUE)
  defaultNS = 'd'
  names(ns)[1] <- defaultNS
  
  # Fetch List of SOBlock elements
  # Supress namespace undefined messages
  sink("NUL")
  SOlist = xpathApply(root, "//d:SOBlock", namespaces=ns)
  sink()

  # Assumes only one SOBlock for now!
  stopifnot(length(SOlist) == 1)

  ### Future for loop to start here i multiple SO blocks

  # Fetch all Components of the SO object that are defined
  SOChildren = xmlChildren(SOlist[[1]])

  # Error checking of expected XML structure + Parser Execution
  if ("ToolSettings" %in% names(SOChildren)){
    SOObject = ParseToolSettings(SOObject, SOChildren[["ToolSettings"]])
  } else {
    warning("ToolSettings element not detected in PharmML. Skipping...")
  }

  if ("RawResults" %in% names(SOChildren)){
    SOObject = ParseRawResults(SOObject, SOChildren[["RawResults"]])
  } else {
    warning("RawResults element not detected in PharmML. Skipping...")
  }

  if ("Estimation" %in% names(SOChildren)){

      if ("PopulationEstimates" %in% names(SOChildren[["Estimation"]])){
        SOObject = ParsePopulationEstimates(SOObject, SOChildren[["Estimation"]][["PopulationEstimates"]])
      } else {
        warning("PopulationEstimates element not detected in PharmML. Skipping...")
      }

      if ("PrecisionPopulationEstimates" %in% names(SOChildren[["Estimation"]])){
        SOObject = ParsePrecisionPopulationEstimates(SOObject, SOChildren[["Estimation"]][["PrecisionPopulationEstimates"]])
      } else {
        warning("PrecisionPopulationEstimates element not detected in PharmML. Skipping...")
      }

      if ("IndividualEstimates" %in% names(SOChildren[["Estimation"]])){
        SOObject = ParseIndividualEstimates(SOObject, SOChildren[["Estimation"]][["IndividualEstimates"]])
      } else {
        warning("IndividualEstimates element not detected in PharmML. Skipping...")
      }

      if ("Residuals" %in% names(SOChildren[["Estimation"]])){
        SOObject = ParseResiduals(SOObject, SOChildren[["Estimation"]][["Residuals"]])
      } else {
        warning("Residuals element not detected in PharmML. Skipping...")
      }

      if ("Predictions" %in% names(SOChildren[["Estimation"]])){
        SOObject = ParsePredictions(SOObject, SOChildren[["Estimation"]][["Predictions"]])
      } else {
        warning("Predictions element not detected in PharmML. Skipping...")
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
                              return(SOObject@ToolSettings)
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
                              return(SOObject@RawResults@Files)
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
                        return(SOObject@Estimation@PopulationEstimates)
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
                        return(SOObject@Estimation@PrecisionPopulationEstimates)
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
                              return(SOObject@Estimation@IndividualEstimates)
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
                              return(SOObject@Estimation@PrecisionIndividualEstimates)
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
                              return(SOObject@Estimation@Residuals)
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
                              return(SOObject@Estimation@Predictions)
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
                              return(SOObject@Estimation@Likelihood)
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
                              return(SOObject@Estimation@SoftwareMessages)
                      }
                      )



# ========================== #
# Reader for RawData files  #
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
          id2FileType = lapply(SOObject@RawResults@Files, function(x) x[["Description"]])
          names(id2FileType)<- tolower(names(id2FileType))

          fileType2Id = as.list(names(id2FileType))
          names(fileType2Id) <- tolower(id2FileType)

          fileIndex = tolower(fileIndex)

          # fileIndex can be file id or file description
          if (fileIndex %in% names(id2FileType)){
            fileElement = SOObject@RawResults@Files[[fileIndex]]
          } else if (fileIndex %in% names(fileType2Id)){           
            fileElement = SOObject@RawResults@Files[[fileType2Id[[fileIndex]]]]
          } else {
            stop(paste0("File Reference: ",
              fileIndex," not found for either file id or file description"))
          }

          # TODO: Possibly need to add custom NA handelling here 
          rawData = read.csv(rawFile$path, na.strings=".")
          return (rawData)
}
)

# ============================= #
# Convert to single Data Frame  #
# ============================= #
#' as.data
#'
#'  Method to Fetch all relevant data and return a merged data.frame onject.
#'
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
              rawData = read.csv(inputDataPath, na.strings=".")
              names(rawData)<-c("ID","TIME","WT","AMT","DVID","DV","MDV","logtWT") 
            }

            # Fetch and merge Predictions 
            predictions = SOObject@Estimation@Predictions$data
            predictions[["TIME"]] = ToNumeric(predictions[["TIME"]])
            mergedDataFrame <- merge(rawData, predictions)

            # Fetch and merge Residuals 
            # residuals = SOObject@Estimation@Residuals
            # for (name in c("WRES", "IWRES")){
            #   mergedDataFrame <- merge(mergedDataFrame, residuals[[name]][["data"]])
            # }

            # Fetch and merge Residuals 
            residuals = SOObject@Estimation@Residuals
            for (name in c("Population", "Individual")){
              mergedDataFrame <- merge(mergedDataFrame, residuals[[name]])
            }

            # IndividualEstimates, Estimates
            dat = SOObject@Estimation@IndividualEstimates$Estimates$Mean$data
            mergedDataFrame <- merge(mergedDataFrame, dat)

            # IndividualEstimates, RandomEffects
            dat = SOObject@Estimation@IndividualEstimates$RandomEffects$EffectMean$data
            mergedDataFrame <- merge(mergedDataFrame, dat)

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
            xpose4_dataFrame = as.data(SOObject, inputDataPath)

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


