#' getEstimationInfo
#'
#' This function acts on an object of class StandardOutputObject 
#' and presents information to the user about the estimation process.  
#' 
#' Depending on the target software and estimation method, values 
#' such as Objective Function Value (OFV), -2*log-likelihood and/or Information criteria
#' such as AIC, DIC, BIC may be returned. In addition, any warnings, errors and info messages 
#' from the log will also be stored in the returned output. If there are any errors of warnings, 
#' these will also be printed to the console.   
#'
#' @param object an object of class StandardOutputObject, the output from an estimation task
#'
#' @return A nested list with two elements:.
#'         \describe{
#'  \item{"OFMeasures"}{All information from the OFMeasures slot of the SOObject}
#'  \item{"Messages"}{A nested list for each message grouped by message type ("Info", "Error", and/or "Warning" if present)}
#' }
#'
#' @examples getEstimationInfo(object)
#'
#' @export 
getEstimationInfo <- function(SOObject){
  
	if (class(SOObject) != "StandardOutputObject") {
		stop(paste0("getEstimationInfo expected a StandardOutputObject as input, got a ", class(SOObject), '.'))
  	}

  # Fetch OFMeasures information

  likelihood <- list()
  
  # Format as numeric 
  if ("Likelihood" %in% getPopulatedSlots(SOObject@Estimation@OFMeasures)) {
    likelihood[["Likelihood"]] <- lapply(SOObject@Estimation@OFMeasures@Likelihood, as.numeric)
  }
  # Format as numeric 
  if ("LogLikelihood" %in% getPopulatedSlots(SOObject@Estimation@OFMeasures)) {
    likelihood[["LogLikelihood"]] <- lapply(SOObject@Estimation@OFMeasures@LogLikelihood, as.numeric)
  }
  # Format as numeric 
  if ("Deviance" %in% getPopulatedSlots(SOObject@Estimation@OFMeasures)) {
    likelihood[["Deviance"]] <- lapply(SOObject@Estimation@OFMeasures@Deviance, as.numeric)
  }
  # Format as numeric (TODO get metadata "type", string)
  if ("ToolObjFunction" %in% getPopulatedSlots(SOObject@Estimation@OFMeasures)) {
    likelihood[["ToolObjFunction"]] <- lapply(SOObject@Estimation@OFMeasures@ToolObjFunction, as.numeric)
  }
  # data set
  if ("IndividualContribToLL" %in% getPopulatedSlots(SOObject@Estimation@OFMeasures)) {
    likelihood[["IndividualContribToLL"]] <- as.data.frame(SOObject@Estimation@OFMeasures@IndividualContribToLL)
  }
  # Format as numeric TODO (TODO get metadata "type", string)
  if ("InformationCriteria" %in% getPopulatedSlots(SOObject@Estimation@OFMeasures)) {
    likelihood[["InformationCriteria"]] <- lapply(SOObject@Estimation@OFMeasures@InformationCriteria, as.numeric)
  }

  # Fetch Messages 
  info.msgs <- list()
  for (elem in (SOObject@TaskInformation@InformationMessages)) { 
    info.msgs[[elem$Name]] <- elem$Content
  }
  err.msgs <- list()
  for (elem in (SOObject@TaskInformation@ErrorMessages)) { 
    err.msgs[[elem$Name]] <- elem$Content
  }
  warn.msgs <- list()
  for (elem in (SOObject@TaskInformation@WarningMessages)) { 
    warn.msgs[[elem$Name]] <- elem$Content
  }
  # Drop any list that does not contain messages
  temp.list <- list(Info=info.msgs, Errors=err.msgs, Warnings=warn.msgs)
  messages <- list()
  for (msg.list.name in names(temp.list)) {
    if (length(temp.list[[msg.list.name]]) > 0 ) {
      messages[[msg.list.name]] <- temp.list[[msg.list.name]]
    }
  }

  # Print out any errors in the SO Object to the R console to make it obvious if execution failed
  if (length(SOObject@TaskInformation@ErrorMessages) > 0) {
    message("\nThe following ERRORs were raised during the job execution:", file=stderr())
    for (e in (SOObject@TaskInformation@ErrorMessages)) { 
      message(paste0(" ", e$Name, ": ", str_trim(e$Content)), file=stderr()) 
    }
  }
  if (length(SOObject@TaskInformation@WarningMessages) > 0) {
    message("\nThe following WARNINGs were raised during the job execution:", file=stderr())
    for (e in (SOObject@TaskInformation@WarningMessages)) { 
      message(paste0(" ", e$Name, ": ", str_trim(e$Content)), file=stderr()) 
    }
  }

  list(OFMeasures=likelihood, Messages=messages)
}



# Lower Level Getter functions: Not Currently used  #
# ------------------------------------------------- #

# #' Create a method to fetch the value of OFMeasures Slot
# setGeneric(name="getOFMeasures",
#            def=function(SOObject)
#            {
#               standardGeneric("getOFMeasures")
#            }
# )
# setMethod(f="getOFMeasures",
#           signature="StandardOutputObject",
#           definition=function(SOObject)
#           { 
#            OFMeasures <- SOObject@Estimation@OFMeasures

#           L = list()
#           if ("LogLikelihood" %in% names(OFMeasures)) {
#             A = list(LogLikelihood=OFMeasures[["LogLikelihood"]])
#             L <- c(L, A)
#           }
#           if ("Deviance" %in% names(OFMeasures)) {
#             B <- list(Deviance=OFMeasures[["Deviance"]]) 
#             L <- c(L, B)
#           }
#           if ("IndividualContribToLL" %in% names(OFMeasures)) {
#             C <- list(IndividualContribToLL=OFMeasures[["IndividualContribToLL"]][["data"]]) 
#             L <- c(L, C)
#           }          
#           if ("AIC" %in% names(OFMeasures[["InformationCriteria"]])) {
#             D <- list(AIC=OFMeasures[["InformationCriteria"]][["AIC"]])
#             L <- c(L, D)
#           }   
#           if ("BIC" %in% names(OFMeasures[["InformationCriteria"]])) {
#             E <- list(BIC=OFMeasures[["InformationCriteria"]][["BIC"]])
#             L <- c(L, E)
#           } 
#           if ("DIC" %in% names(OFMeasures[["InformationCriteria"]])) {
#             F <- list(DIC=OFMeasures[["InformationCriteria"]][["DIC"]])
#             L <- c(L, F)
#           }       
#            pprintList(L, "OFMeasures")
#           }
# )



# #' Create a method to fetch the value of SoftwareMessages Slot
# setGeneric(name="getSoftwareMessages",
#                        def=function(SOObject)
#                        {
#                           standardGeneric("getSoftwareMessages")
#                        }
#                        )
# setMethod(f="getSoftwareMessages",
#           signature="StandardOutputObject",
#           definition=function(SOObject)
#           {                              
#            SoftwareMessages <- SOObject@TaskInformation
#            pprintList(SoftwareMessages, "Software Messages")
#           }
# )

# ================ #
# Getter Methods   #
# ================ #

# #' Create a method to fetch the value of ToolSetting Slot
# setGeneric(name="getToolSettings",
#            def=function(SOObject)
#            {
#                    standardGeneric("getToolSettings")
#            }
# )
# setMethod(f="getToolSettings",
#           signature="StandardOutputObject",
#           definition=function(SOObject)
#           {                              
#             ToolSettings <- SOObject@ToolSettings
#             pprintList(ToolSettings, title="Tool Settings")
#           }
# )


# #' Create a method to fetch the value of RawResults Slot
# setGeneric(name="getRawResults",
#            def=function(SOObject)
#            {
#                    standardGeneric("getRawResults")
#            }
# )
# setMethod(f="getRawResults",
#           signature="StandardOutputObject",
#           definition=function(SOObject)
#           {                              
#             DataFiles <- SOObject@RawResults@DataFiles
#             GraphicsFiles <- SOObject@RawResults@GraphicsFiles

#             L = c(DataFiles, GraphicsFiles)
#             pprintList(L, title="Raw Result Files")  
#           }
# )


# #' Create a method to fetch the value of PopulationEstimates Slot
# setGeneric(name="getPopulationEstimates",
#            def=function(SOObject)
#            {
#               standardGeneric("getPopulationEstimates")
#            }
# )
# setMethod(f="getPopulationEstimates",
#           signature="StandardOutputObject",
#           definition=function(SOObject)
#           {     
#           PopulationEstimates <- SOObject@Estimation@PopulationEstimates
          
#           L = list()
#           if ("MLE" %in% names(PopulationEstimates)) {
#             L[["MLE"]] <- PopulationEstimates[["MLE"]]
#           } 
#           if ("Bayesian" %in% names(PopulationEstimates)) {
#             B <- PopulationEstimates[["Bayesian"]]
#             names(B) <- paste0('Bayes:', names(B))
#             L <- c(L, B)
#           }

#           # Pretty print a list of data table elements 
#           pprintDefTable(L, title="Population Estimates")
#           }
# )


# #' Create a method to fetch the value of PrecisionPopulationEstimates Slot
# setGeneric(name="getPrecisionPopulationEstimates",
#            def=function(SOObject)
#            {
#               standardGeneric("getPrecisionPopulationEstimates")
#            }
# )
# setMethod(f="getPrecisionPopulationEstimates",
#           signature="StandardOutputObject",
#           definition=function(SOObject)
#           {                              
#           PrecisionPopulationEstimates <- SOObject@Estimation@PrecisionPopulationEstimates
          
#           L = list()
#           if ("MLE" %in% names(PrecisionPopulationEstimates)) {
#             A = PrecisionPopulationEstimates[["MLE"]]
#             names(A) <- paste0('MLE:', names(A))
#             L <- c(L, A)
#           } 
#           if ("Bayesian" %in% names(PrecisionPopulationEstimates)) {
#             B <- PrecisionPopulationEstimates[["Bayesian"]]
#             names(B) <- paste0('Bayes:', names(B))
#             L <- c(L, B)
#           }
#           if ("Bootstrap" %in% names(PrecisionPopulationEstimates)) {
#             C <- PrecisionPopulationEstimates[["Bayesian"]]
#             names(C) <- paste0('Bootstrap:', names(C))
#             L <- c(L, C)
#           }

#           # Pretty print a list of data table elements 
#           pprintDefTable(L, title="Precision Population Estimates")
#           }
# )


# #' Create a method to fetch the value of IndividualEstimates Slot
# setGeneric(name="getIndividualEstimates",
#            def=function(SOObject)
#            {
#               standardGeneric("getIndividualEstimates")
#            }
# )
# setMethod(f="getIndividualEstimates",
#           signature="StandardOutputObject",
#           definition=function(SOObject)
#       {  
#           IndividualEstimates <- SOObject@Estimation@IndividualEstimates
        
#           L = list()
#           if ("EtaShrinkage" %in% names(IndividualEstimates)) {
#             A = IndividualEstimates[["EtaShrinkage"]]
#             L <- c(L, A)
#           } 
#           if ("RandomEffects" %in% names(IndividualEstimates)) {
#             B <- IndividualEstimates[["RandomEffects"]]
#             names(B) <- paste0('RandomEffects:', names(B))
#             L <- c(L, B)
#           }
#           if ("Estimates" %in% names(IndividualEstimates)) {
#             C <- IndividualEstimates[["Estimates"]]
#             names(C) <- paste0('Estimates:', names(C))
#             L <- c(L, C)
#           }

#           # Pretty print a list of data table elements
#           pprintDefTable(L, "Individual Estimates")
#       }                              
# )


# #' Create a method to fetch the value of PrecisionIndividualEstimates Slot
# setGeneric(name="getPrecisionIndividualEstimates",
#            def=function(SOObject)
#            {
#               standardGeneric("getPrecisionIndividualEstimates")
#            }
# )
# setMethod(f="getPrecisionIndividualEstimates",
#                       signature="StandardOutputObject",
#                       definition=function(SOObject)
#    {                          
#       PrecisionIndividualEstimates <- SOObject@Estimation@PrecisionIndividualEstimates
#       pprintList(PrecisionIndividualEstimates, "Precision Individual Estimates")
#    }                                                     
# )


# #' Create a method to fetch the value of Residuals Slot
# setGeneric(name="getResiduals",
#            def=function(SOObject)
#            {
#               standardGeneric("getResiduals")
#            }
# )
# setMethod(f="getResiduals",
#                       signature="StandardOutputObject",
#                       definition=function(SOObject)
#     {
#           Residuals <- SOObject@Estimation@Residuals

#           L = list()
#           if ("EpsShrinkage" %in% names(Residuals)) {
#             A = Residuals[["EpsShrinkage"]]
#             L <- c(L, A)
#           } 
#           if ("ResidualTable" %in% names(Residuals)) {
#             B <- Residuals[["ResidualTable"]]
#             L <- c(L, B)
#           }

#           # Pretty print a list of data table elements
#           pprintDefTable(L, "Residuals")
#     }
# )


# #' Create a method to fetch the value of Predictions Slot
# setGeneric(name="getPredictions",
#            def=function(SOObject)
#            {
#               standardGeneric("getPredictions")
#            }
# )
# setMethod(f="getPredictions",
#           signature="StandardOutputObject",
#           definition=function(SOObject)
#          {
#            Predictions <- SOObject@Estimation@Predictions
#            pprintDefTable(Predictions, "Predictions")
#          }                              
# )








# #' Create a method to fetch the value of Simulation : SimulationBlock(s) : SimulatedProfiles slot
# setGeneric(name="getSimulatedProfiles",
# 		def=function(SOObject)
# 		{
# 			standardGeneric("getSimulatedProfiles")
# 		}
# )
# setMethod(f="getSimulatedProfiles",
# 		signature="StandardOutputObject",
# 		definition=function(SOObject)
# 		{
# 			simulationBlocks <- SOObject@Simulation@SimulationBlock
			
# 			SimulatedProfiles <- lapply(simulationBlocks, function(n) { n@SimulatedProfiles })
# 			names(SimulatedProfiles) <- rep("SimulatedProfiles", length(SimulatedProfiles))  # the names of the elements in the named list are incorrect after the lapply()
			
# 			pprintList(SimulatedProfiles, "Simulation : Simulation Block(s) : Simulated Profiles")
# 		}                                                     
# )


# #' Create a method to fetch the value of Simulation : SimulationBlock(s) : IndivParameters slot
# setGeneric(name="getSimulationIndividualParameters",
# 		def=function(SOObject)
# 		{
# 			standardGeneric("getSimulationIndividualParameters")
# 		}
# )
# setMethod(f="getSimulationIndividualParameters",
# 		signature="StandardOutputObject",
# 		definition=function(SOObject)
# 		{
# 			simulationBlocks <- SOObject@Simulation@SimulationBlock
			
# 			IndivParameters <- lapply(simulationBlocks, function(n) { n@IndivParameters })
# 			names(IndivParameters) <- rep("IndivParameters", length(IndivParameters))  # the names of the elements in the named list are incorrect after the lapply()
			
# 			pprintList(IndivParameters, "Simulation : Simulation Block(s) : Individual Parameters")
# 		}
# )


# #' Create a method to fetch the value of Simulation : SimulationBlock(s) : PopulationParameters slot
# setGeneric(name="getSimulationPopulationParameters",
# 		def=function(SOObject)
# 		{
# 			standardGeneric("getSimulationPopulationParameters")
# 		}
# )
# setMethod(f="getSimulationPopulationParameters",
# 		signature="StandardOutputObject",
# 		definition=function(SOObject)
# 		{
# 			simulationBlocks <- SOObject@Simulation@SimulationBlock
			
# 			PopulationParameters <- lapply(simulationBlocks, function(n) { n@PopulationParameters })
# 			names(PopulationParameters) <- rep("PopulationParameters", length(PopulationParameters))  # the names of the elements in the named list are incorrect after the lapply()
			
# 			pprintList(PopulationParameters, "Simulation : Simulation Block(s) : Population Parameters")
# 		}
# )


# #' Create a method to fetch the value of Simulation : SimulationBlock(s) : RawResultsFile slot
# setGeneric(name="getSimulationRawResultsFiles",
# 		def=function(SOObject)
# 		{
# 			standardGeneric("getSimulationRawResultsFiles")
# 		}
# )
# setMethod(f="getSimulationRawResultsFiles",
# 		signature="StandardOutputObject",
# 		definition=function(SOObject)
# 		{
# 			simulationBlocks <- SOObject@Simulation@SimulationBlock
			
# 			RawResultsFiles <- lapply(simulationBlocks, function(n) { n@RawResultsFile })
# 			names(RawResultsFiles) <- rep('RawResultsFile', length(RawResultsFiles))  # the names of the elements in the named list are incorrect after the lapply()
			
# 			pprintList(RawResultsFiles, "Simulation : Simulation Block(s) : Raw Results File")
# 		}
# )


# #' Create a method to fetch the value of Simulation : OriginalDataSet slot
# setGeneric(name="getSimulationOriginalDataset",
# 		def=function(SOObject)
# 		{
# 			standardGeneric("getSimulationOriginalDataset")
# 		}
# )
# setMethod(f="getSimulationOriginalDataset",
# 		signature="StandardOutputObject",
# 		definition=function(SOObject)
# 		{
# 			OriginalDataset <- SOObject@Simulation@OriginalDataset
			
# 			pprintList(OriginalDataset, "Simulation : Original Data Set")
# 		}
# )
