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
            pprintList(ToolSettings, title="Tool Settings")
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
            DataFiles <- SOObject@RawResults@DataFiles
            GraphicsFiles <- SOObject@RawResults@GraphicsFiles

            L = c(DataFiles, GraphicsFiles)
            pprintList(L, title="Raw Result Files")  
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
          
          A = PopulationEstimates[["MLE"]]
          B = PopulationEstimates[["Bayesian"]]
          names(B) <- paste0('Bayes:', names(B))
          
          L = c(A, B)

          # Pretty print a list of data table elements 
          pprintDefTable(L, title="Population Estimates")
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
          
          A = PrecisionPopulationEstimates[["MLE"]]
          names(A) <- paste0('MLE:', names(A))
          B = PrecisionPopulationEstimates[["Bayesian"]]
          names(B) <- paste0('Bayes:', names(B))
          C = PrecisionPopulationEstimates[["Bootstrap"]]
          names(C) <- paste0('Bootstrap:', names(C))

          L = c(A, B, C)

          # Pretty print a list of data table elements 
          pprintDefTable(L, title="Precision Population Estimates")
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
        
          A = IndividualEstimates[["EtaShrinkage"]]
          B = IndividualEstimates[["RandomEffects"]]
          names(B) <- paste0('RandomEffects:', names(B))
          C = IndividualEstimates[["Estimates"]]
          names(C) <- paste0('Estimates:', names(C))

          L = c(A, B, C)

          # Pretty print a list of data table elements
          pprintDefTable(L, "Individual Estimates")
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

          A = Residuals[["EpsShrinkage"]]
          B = Residuals[["ResidualTable"]]

          L = c(A, B)

          # Pretty print a list of data table elements
          pprintDefTable(L, "Residuals")
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
           pprintDefTable(Predictions, "Predictions")
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

           # Concatenate all information into one list 
           L = list(LogLikelihood=Likelihood[["LogLikelihood"]], 
                    Deviance=Likelihood[["Deviance"]], 
                    IndividualContribToLL=Likelihood[["IndividualContribToLL"]][["data"]], 
                    AIC=Likelihood[["InformationCriteria"]][["AIC"]], 
                    BIC=Likelihood[["InformationCriteria"]][["BIC"]], 
                    DIC=Likelihood[["InformationCriteria"]][["DIC"]]
                    )

           pprintList(L, "Likelihood")
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
           SoftwareMessages <- SOObject@TaskInformation
           pprintList(SoftwareMessages, "Software Messages")
          }
)


#' Create a method to fetch the value of Simulation : SimulationBlock(s) : SimulatedProfiles slot
setGeneric(name="getSimulatedProfiles",
		def=function(SOObject)
		{
			standardGeneric("getSimulatedProfiles")
		}
)
setMethod(f="getSimulatedProfiles",
		signature="StandardOutputObject",
		definition=function(SOObject)
		{
			simulationBlocks <- SOObject@Simulation@SimulationBlock
			
			SimulatedProfiles <- lapply(simulationBlocks, function(n) { n@SimulatedProfiles })
			names(SimulatedProfiles) <- rep("SimulatedProfiles", length(SimulatedProfiles))  # the names of the elements in the named list are incorrect after the lapply()
			
			pprintList(SimulatedProfiles, "Simulation : Simulation Block(s) : Simulated Profiles")
		}                                                     
)


#' Create a method to fetch the value of Simulation : SimulationBlock(s) : IndivParameters slot
setGeneric(name="getSimulationIndividualParameters",
		def=function(SOObject)
		{
			standardGeneric("getSimulationIndividualParameters")
		}
)
setMethod(f="getSimulationIndividualParameters",
		signature="StandardOutputObject",
		definition=function(SOObject)
		{
			simulationBlocks <- SOObject@Simulation@SimulationBlock
			
			IndivParameters <- lapply(simulationBlocks, function(n) { n@IndivParameters })
			names(IndivParameters) <- rep("IndivParameters", length(IndivParameters))  # the names of the elements in the named list are incorrect after the lapply()
			
			pprintList(IndivParameters, "Simulation : Simulation Block(s) : Individual Parameters")
		}
)


#' Create a method to fetch the value of Simulation : SimulationBlock(s) : PopulationParameters slot
setGeneric(name="getSimulationPopulationParameters",
		def=function(SOObject)
		{
			standardGeneric("getSimulationPopulationParameters")
		}
)
setMethod(f="getSimulationPopulationParameters",
		signature="StandardOutputObject",
		definition=function(SOObject)
		{
			simulationBlocks <- SOObject@Simulation@SimulationBlock
			
			PopulationParameters <- lapply(simulationBlocks, function(n) { n@PopulationParameters })
			names(PopulationParameters) <- rep("PopulationParameters", length(PopulationParameters))  # the names of the elements in the named list are incorrect after the lapply()
			
			pprintList(PopulationParameters, "Simulation : Simulation Block(s) : Population Parameters")
		}
)


#' Create a method to fetch the value of Simulation : SimulationBlock(s) : RawResultsFile slot
setGeneric(name="getSimulationRawResultsFiles",
		def=function(SOObject)
		{
			standardGeneric("getSimulationRawResultsFiles")
		}
)
setMethod(f="getSimulationRawResultsFiles",
		signature="StandardOutputObject",
		definition=function(SOObject)
		{
			simulationBlocks <- SOObject@Simulation@SimulationBlock
			
			RawResultsFiles <- lapply(simulationBlocks, function(n) { n@RawResultsFile })
			names(RawResultsFiles) <- rep('RawResultsFile', length(RawResultsFiles))  # the names of the elements in the named list are incorrect after the lapply()
			
			pprintList(RawResultsFiles, "Simulation : Simulation Block(s) : Raw Results File")
		}
)


#' Create a method to fetch the value of Simulation : OriginalDataSet slot
setGeneric(name="getSimulationOriginalDataset",
		def=function(SOObject)
		{
			standardGeneric("getSimulationOriginalDataset")
		}
)
setMethod(f="getSimulationOriginalDataset",
		signature="StandardOutputObject",
		definition=function(SOObject)
		{
			OriginalDataset <- SOObject@Simulation@OriginalDataset
			
			pprintList(OriginalDataset, "Simulation : Original Data Set")
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

  invisible(list(Likelihood=likelihood, Messages=messages))
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
      df2[, ID.col] = as.numeric(as.character(df2[, ID.col]))
  } else if (class(df2[, ID.col]) == "integer" | 
             class(df2[, ID.col]) == "character") {
      df2[, ID.col] = as.numeric(df2[, ID.col])
  }

  # Type conversion Checks for TIME column
  if (class(df2[, TIME.col]) == "factor") {
      df2[, TIME.col] = as.numeric(as.character(df2[, TIME.col]))
  }else if (class(df2[, TIME.col]) == "integer" | 
             class(df2[, TIME.col]) == "character") {
      df2[, TIME.col] = as.numeric(df2[, TIME.col])
  }

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

          # Reorder data frame to have ID and TIME column as first two. 
          ID.col = toupper(names(rawData)) == "ID"
          TIME.col = toupper(names(rawData)) == "TIME"
          remaining.names = setdiff(toupper(names(rawData)),c("ID","TIME"))
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


