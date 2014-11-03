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
setClass("StandardOutputObject", package="DDMoRe.TEL", 
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

  # Execute Parsers
  SOObject = ParseToolSettings(SOObject, SOChildren[["ToolSettings"]])
  SOObject = ParseRawResults(SOObject, SOChildren[["RawResults"]])
  SOObject = ParsePopulationEstimates(SOObject, SOChildren[["Estimation"]][["PopulationEstimates"]])
  SOObject = ParsePrecisionPopulationEstimates(SOObject, SOChildren[["Estimation"]][["PrecisionPopulationEstimates"]])
  SOObject = ParseIndividualEstimates(SOObject, SOChildren[["Estimation"]][["IndividualEstimates"]])
  SOObject = ParseResiduals(SOObject, SOChildren[["Estimation"]][["Residuals"]])
  SOObject = ParsePredictions(SOObject, SOChildren[["Estimation"]][["Predictions"]])

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
                              return(SOObject@RawResults)
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

# =========================== #
# Higher Level Getter Methods #
# =========================== #

#' getEstimationInfo
#'
#'  Print the liklihood and output messages from a StandardOutputObject.
#'
#'  This function acts on an object of class StandardOutputObject, and is a wrapper to
#'  getLikelihood and getSoftareMessages.    
#'
#' @export
setGeneric(name="getEstimationInfo",
                       def=function(SOObject)
                       {
                          standardGeneric("getEstimationInfo")
                       }
                       )
setMethod(f="getEstimationInfo",
                      signature="StandardOutputObject",
                      definition=function(SOObject)
                      {                      

                        liklihood = getLikelihood(SOObject)
                        messages = getSoftwareMessages(SOObject)

                        # Filter out missing messages 
                        msgsToPrint = messages[!sapply(messages, is.null)]
                        
                        print(liklihood)
                        print(msgsToPrint)
                        
                        return(list(liklihood=liklihood, msgs=msgsToPrint))
                      }
                      )

#' getParameterEstimates
#'
#'  This function acts on an object of class StandardOutputObject and is 
#'  a wrapper to getPopulationEstimates and getPrecisionPopulationEstimates 
#'  presenting estimates of the STRUCTURAL and RANDOM_VARIABILITY parameters along 
#'  with any associated measures of uncertainty (Std Dev) and interval estimates if 
#'  these are available.  Fetch estimation information from parameters.
#'
#' @param type character string determining which parameters to return. Choice of 
#' "structural", "variability" or "all"
#'
#' @param what character string determining what values to return: 
#'    \itemize{
#'        \item “estimates” – returns point estimates for the parameters.
#'        \item “precision” – returns variability / uncertainty estimates for the parameters.
#'        \item “intervalEstimates” - returns interval estimates for the parameters. 
#'        \item “all” – returns all of the above in a table.
#'    } 
#'
#' The values available for return from the StandardOutputObject depend on the 
#' estimation method used and how these are populated – either directly from the 
#' estimation task output or subsequently via other methods e.g. bootstrapping. 
#' The function will provide suitable names for the columns depending on the 
#' methods used. So for example, although the argument for “what” accepts 
#' “precision” this may mean SE assuming Normality, Bootstrap SE, SD from MCMC or
#' SAEM estimation methods.
#'
#' @value If only returning “estimates” or “precision” then a named vector of 
#' real values. If returning “intervalEstimates” or “all” then a data frame 
#' containing one row for each parameter. Columns are “Estimate”, “Precision”, 
#' “Lower”, “Upper”, “Shrinkage".
#'
#' @export
setGeneric(name="getParameterEstimates",
                       def=function(SOObject, type="all", what="all")
                       {
                          standardGeneric("getParameterEstimates")
                       }
                       )
setMethod(f="getParameterEstimates",
                      signature="StandardOutputObject",
                      definition=function(SOObject, type="all", what="all")
                      {
                        
                        # TODO: Finish writiing 

                        switch{type, 
                          "structural" = {

                          },
                          "variability" = {

                          },
                          "all" = {

                          }
                        }


                        switch{what, 
                          "estimates" = {

                          },
                          "precision" = {

                          },
                          "individualEstimates" = {

                          },
                          "all" = {

                          }
                        }

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
           def=function(SOObject, ...)
          {
             standardGeneric("as.data")
          }
          )
setMethod(f="as.data",
          signature="StandardOutputObject",
          definition=function(SOObject, ...)
                   {
            ####################################
            # Read in Raw Data
            # TODO: Replace Hard coded parts in this section. They should be gleamed from full PharmML 

            # Define a file type to id mapping 
            id2FileType = list("r1" = "estimates", "r2" = "predictions", "r3"="rawData")
            fileType2Id = as.list(names(id2FileType))
            names(fileType2Id) <- id2FileType

            setwd("C:/Users/cmusselle/Projects/DDmore/TEL-R/development data/warfarin_PK_ODE/warfarin_PK_ODE_project/")
            rawFile = SOObject@RawResults@Files[[fileType2Id[["rawData"]]]]
            rawData = read.csv(rawFile$path, na.strings=".")
            setwd("C:/Users/cmusselle/Projects/DDmore/TEL-R/development data")

            names(rawData)<-c("ID","TIME","WT","AMT","DVID","DV","MDV","logtWT") 
            ####################################

            # Fetch and merge Predictions 
            predictions = SOObject@Estimation@Predictions$data
            mergedDataFrame <- merge(rawData, predictions)

            # Fetch and merge Residuals 
            residuals = SOObject@Estimation@Residuals
            for (name in c("WRES", "IWRES")){
              mergedDataFrame <- merge(mergedDataFrame, residuals[[name]][["data"]])
            }

            # IndividualEstimates, Estimates
            mergedDataFrame <- merge(mergedDataFrame, 
              SOObject@Estimation@IndividualEstimates$Estimates$Mean$data)
            # IndividualEstimates, RandomEffects
            mergedDataFrame <- merge(mergedDataFrame, 
              SOObject@Estimation@IndividualEstimates$RandomEffects$EffectMean$data)

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
#' 
setGeneric(name="as.xpdb",
           def=function(SOObject)
          {
             standardGeneric("as.xpdb")
          }
          )
setMethod(f="as.xpdb",
          signature="StandardOutputObject",
          definition=function(SOObject)
          {

            # create Merged data frame
            xpose4_dataFrame = as.data(SOObject)

            library('xpose4')
            # CREATE new Xpose database
            myXpdb<-new("xpose.data",Runno=0,Doc=NULL)
            
            # Possibly need to check data types here
            #
            #
            #

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


