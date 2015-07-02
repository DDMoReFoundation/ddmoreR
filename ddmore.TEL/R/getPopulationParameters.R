# ============================= #
# Higher Level Getter Functions #
# ============================= #
#' getPopulationParameters
#' 
#' This function acts on an object of class StandardOutputObject and is a wrapper 
#' to getMLEPopulationParameters, getBayesianPopulationParameters and 
#' getBootstrapPopulationParameters presenting 
#' estimates of the STRUCTURAL and RANDOM_VARIABILITY parameters along with any 
#' associated measures of uncertainty (Std Dev) and interval estimates if these 
#' are available.
#'
#' The values available for return from the StandardOutputObject depend on the 
#' estimation method used and how these are populated - either directly from the
#' estimation task output or subsequently via other methods e.g. bootstrapping. 
#' The function will provide suitable names for the columns depending on the 
#' methods used. So for example, although the argument for "what" accepts 
#' "precision" this may mean SE assuming Normality, Bootstrap SE, SD from MCMC 
#' or SAEM estimation methods.
#'
#' @param SOObject an object of class StandardOutputObject, the output from an 
#'    estimation task.
#' @param type character string determining which parameters to return. 
#'    Options are "structural", "variability", "all" (default).
#' @param what character vector determining what values to return:
#'  \itemize{
#'    \item "estimates" - returns point estimates for the parameters.
#'    \item "precision" - returns variability / uncertainty estimates for the 
#'        parameters.
#'    \item "intervals" - returns interval estimates for the parameters. 
#'    \item "all" - returns all of the above in a table.}
#' @param keep.only character string determining which central tendency statistic to use 
#'      for the estimate when multiple are present. Only applicable to Bayesian which
#'      has Mean, Median and  Mode; and Bootstrap which has Mean and Median as options.   
#' 
#' @return If only returning "estimates" or "precision" then a named vector of 
#' real values. If returning "intervalEstimates" or "All" then a data frame 
#' containing one row for each parameter. Columns are "Estimate", "Precision", 
#' "Lower", "Upper", "Shrinkage"
#' 
#' @examples getPopulationParameters(object, type="all", what="all")
#'
#' @seealso getPopulationEstimates, getPrecisionPopulationEstimates
#'
#' @export
getPopulationParameters <- function(SOObject, type="all", what="all", keep.only=NULL) {
  
  if (!(type %in% c("structural", "variability", "all"))) {
      stop('Parameter "type" must be one of: "structural", "variability" or "all".')
  } 
  if (!(what %in% c("estimates", "precision", "intervals", "all"))) {
      stop('Parameter "what" must be one of: "estimates", "precision", "intervals" or "all".')
  }

  stopifnot(isS4(SOObject) & class(SOObject) == "StandardOutputObject")

  # Examine what objects exist in Population Estimates
  if (is.null(SOObject@Estimation@PopulationEstimates) || length(SOObject@Estimation@PopulationEstimates) == 0 ) {
    stop("Section Estimation:PopulationEstimates not found in SO Object")
  }

  if (is.null(SOObject@Estimation@PrecisionPopulationEstimates) || length(SOObject@Estimation@PrecisionPopulationEstimates) == 0 ) {
    message("Section Estimation:PrecisionPopulationEstimates not found in SO Object. \nOutput will only contain values for the point estimates. ")
    what='estimates'
  }
  
  # Cycle through estimates present, parsing as necessary
  estimateTypes <- names(SOObject@Estimation@PopulationEstimates)
  estimates.output.list <- list()
  
  for (estimateType in estimateTypes) {
  
    if (estimateType == "MLE") {
      
      out <- getMLEPopulationParameters(SOObject, what=what)
      estimates.output.list[["MLE"]] <- out
      
    }
    if (estimateType == "Bayesian") {
      
      out <- getBayesianPopulationParameters(SOObject, what=what, keep.only=keep.only)
      estimates.output.list[["Bayesian"]] <- out
      
    }
    if (estimateType == "Bootstrap") {
      
      out <- getBootstrapPopulationParameters(SOObject, what=what, keep.only=keep.only)
      estimates.output.list[["Bootstrap"]] <- out  
      
    }
  
  }
  
  if (type == "structural") {

	structParams <- .deriveStructuralParametersFromAssociatedMDL(SOObject)
    
    # Only return the structural parameters for each type of estimate
    for (df.name in names(estimates.output.list)) {
        
        if (is.vector(estimates.output.list[[df.name]])) {

          keep.idx <- sapply(names(estimates.output.list[[df.name]]), 
              FUN = function(x) {
                  x %in% structParams
              })
          
          estimates.output.list[[df.name]] <- estimates.output.list[[df.name]][keep.idx]

        } else {

          keep.idx <- sapply(estimates.output.list[[df.name]]["Parameter"], 
              FUN = function(x) {
				  x %in% structParams
              })
        
          estimates.output.list[[df.name]] <- estimates.output.list[[df.name]][keep.idx, ]
        
        }
    }
  }
  
  if (type == "variability") {
    
	variabilityParams <- .deriveVariabilityParametersFromAssociatedMDL(SOObject)
	
    # Only return the variability parameters for each type of estimate
    for (df.name in names(estimates.output.list)) {
		
		if (is.vector(estimates.output.list[[df.name]])) {
			
			keep.idx <- sapply(names(estimates.output.list[[df.name]]), 
				FUN = function(x) {
					x %in% variabilityParams
				})
			
			estimates.output.list[[df.name]] <- estimates.output.list[[df.name]][keep.idx]
			
		} else {
    
			keep.idx <- sapply(estimates.output.list[[df.name]]["Parameter"],
				FUN = function(x) {
					x %in% variabilityParams
				})
		
	        estimates.output.list[[df.name]] <- estimates.output.list[[df.name]][keep.idx, ]
		
		}
    }
  }
  
  # Convert all values to a numeric type
  for (estimate.name in names(estimates.output.list)) {

    x <- estimates.output.list[[estimate.name]] 
    
    if (!is.null(colnames(x))) {
        x[, setdiff(colnames(x), "Parameter")] <- sapply(x[, setdiff(colnames(x), "Parameter")], 
                              FUN = function(x) as.numeric(as.character(x)))
    } else {      
      vec.names <- names(x)
      x <- as.numeric(as.character(x))
      names(x) <- vec.names
    }
    
    estimates.output.list[[estimate.name]] <- x
  
  }
  
  return(estimates.output.list)
  
}


getMLEPopulationParameters <- function(SOObject, what="all"){
  
  # Extract parameter values for MLE
  df <- SOObject@Estimation@PopulationEstimates$MLE$data
  # Input checking
  if (is.null(df)) {
    stop("Section Estimation:PopulationEstimates not found in SO Object")
  } 
  
  if (what == "estimates") {
    df.as.list <- unlist(as.list(df))
    names(df.as.list) <- colnames(df)
    return(df.as.list)
  }
  
  # Manipulate to dataframe 
  MLE.output <- as.data.frame(t(df))
  colnames(MLE.output) <- "MLE"
  MLE.output['Parameter'] <- rownames(MLE.output)
  MLE.output <- MLE.output[c("Parameter", "MLE")]
  rownames(MLE.output) <- NULL
  
  if (what %in% c("all", "precision")) {
    se <- SOObject@Estimation@PrecisionPopulationEstimates$MLE$StandardError$data  
    rse <- SOObject@Estimation@PrecisionPopulationEstimates$MLE$RelativeStandardError$data
    if (is.null(se)) {
      stop("Section Estimation:PrecisionPopulationEstimates$MLE$StandardError not found in SO Object")
    }
    if (is.null(rse)) {
      stop("Section Estimation:PrecisionPopulationEstimates$MLE$RelativeStandardError not found in SO Object")
    }
    
    # Append precision information for parameters
    MLE.output <- cbind(MLE.output, se["SE"])
    MLE.output <- cbind(MLE.output, rse["RSE"])
  } 
  
  if (what %in% c("all", "intervals")) {
    CIs <- SOObject@Estimation@PrecisionPopulationEstimates$MLE$AsymptoticCI$data  
    if (is.null(CIs)) {
      stop("Section Estimation:PrecisionPopulationEstimates$MLE$AsymptoticCI not found in SO Object")
    }
    
    # Append Confidence Interval information for parameters
    MLE.output <- cbind(MLE.output, CIs[c("CI", "LowerBound", "UpperBound")])
  } 
  
  return(MLE.output)

}


getBayesianPopulationParameters <- function(SOObject, what="all", keep.only=NULL){
  
  # Extract parameter values for MLE
  estimate_mean <- SOObject@Estimation@PopulationEstimates$Bayesian$PosteriorMean$data
  if (is.null(estimate_mean)) {
    stop(paste0("Section Estimation:PopulationEstimates$Bayesian$PosteriorMean not found in SO Object"))
  }
  estimate_median <- SOObject@Estimation@PopulationEstimates$Bayesian$PosteriorMedian$data
  if (is.null(estimate_median)) {
    stop(paste0("Section Estimation:PopulationEstimates$Bayesian$PosteriorMedian not found in SO Object"))
  }
  estimate_mode <- SOObject@Estimation@PopulationEstimates$Bayesian$PosteriorMode$data
  if (is.null(estimate_mode)) {
    stop(paste0("Section Estimation:PopulationEstimates$Bayesian$PosteriorMode not found in SO Object"))
  }
  
  if (what == "estimates" & !is.null(keep.only)) {
      # Return only named vector of mena/median/mode if thats all thats asked for
      out <- eval(parse(text=paste0("estimate_", tolower(keep.only))))
      out.as.list <- unlist(as.list(out))
      names(out.as.list) <- colnames(out)
      return(out.as.list)
  } 
  
  # Manipulate into data frame
  Bayesian.mean.output <- as.data.frame(t(estimate_mean))
  colnames(Bayesian.mean.output) <- "Mean"
  Bayesian.mean.output['Parameter'] <- rownames(Bayesian.mean.output)
  Bayesian.mean.output <- Bayesian.mean.output[c("Parameter", "Mean")]
  rownames(Bayesian.mean.output) <- NULL
  
  Bayesian.median.output <- as.data.frame(t(estimate_median))
  colnames(Bayesian.median.output) <- "Median"
  Bayesian.median.output['Parameter'] <- rownames(Bayesian.median.output)
  Bayesian.median.output <- Bayesian.median.output[c("Parameter", "Median")]
  rownames(Bayesian.median.output) <- NULL
  
  Bayesian.mode.output <- as.data.frame(t(estimate_mode))
  colnames(Bayesian.mode.output) <- "Mode"
  Bayesian.mode.output['Parameter'] <- rownames(Bayesian.mode.output)
  Bayesian.mode.output <- Bayesian.mode.output[c("Parameter", "Mode")]
  rownames(Bayesian.mode.output) <- NULL
  
  Bayesian.output <- cbind(Bayesian.mean.output, 
                          Bayesian.median.output["Median"], 
                          Bayesian.mode.output["Mode"])

  # Return this data frame if thats all the user requires
  if (what == "estimates") {
    return(Bayesian.output)
  }
  
  # Append precision information for parameters
  if (what %in% c("all", "precision")) {
    sdp <- SOObject@Estimation@PrecisionPopulationEstimates$Bayesian$StandardDeviationPosterior$data  
    if (is.null(sdp)) {
      stop("Section Estimation:PrecisionPopulationEstimates$Bayesian$StandardDeviationPosterior not found in SO Object")
    }
    Bayesian.output <- cbind(Bayesian.output, sdp["SDP"])
  } 
  
  # Append Confidence Interval information for parameters
  if (what %in% c("all", "intervals")) {
    CIs <- SOObject@Estimation@PrecisionPopulationEstimates$Bayesian$PercentilesCI$data 
    if (is.null(CIs)) {
      stop("Section Estimation:PrecisionPopulationEstimates$MLE$AsymptoticCI not found in SO Object")
    }
    
    # Manipulate into data frame
    CI.output <- as.data.frame(t(CIs)[-1, ])
    colnames(CI.output) <- paste("Perc_", CIs[["Percentile"]], sep="")
    CI.output["Parameter"] <- setdiff(rownames(CI.output), "Percentile")
    CI.output <- CI.output[c("Parameter", paste("Perc_", CIs[["Percentile"]], sep=""))]
    rownames(CI.output) <- NULL
    
    Bayesian.output <- cbind(Bayesian.output, CI.output[setdiff(colnames(CI.output), "Parameter")])
  } 

  if (!is.null(keep.only)) {

    stopifnot(tolower(keep.only) %in% c("mean", "median", "mode"))

    # Drop the other central tendency statistics and only keep the one mentioned. 
    drop.indicies <- grep(tolower(keep.only), tolower(names(Bayesian.output))[2:4], invert = TRUE)
    Bayesian.output <- Bayesian.output[-(drop.indicies+1)]
  }
  
  return(Bayesian.output)
  
}


getBootstrapPopulationParameters <- function(SOObject, what="all", keep.only=NULL){

  # Extract parameter values for MLE
  estimate_mean <- SOObject@Estimation@PopulationEstimates$Bootstrap$Mean$data
  if (is.null(estimate_mean)) {
    stop(paste0("Section Estimation:PopulationEstimates$Bootstrap$Mean not found in SO Object"))
  }
  estimate_median <- SOObject@Estimation@PopulationEstimates$Bootstrap$Median$data
  if (is.null(estimate_median)) {
    stop(paste0("Section Estimation:PopulationEstimates$Bootstrap$Median not found in SO Object"))
  }
  
  if (what == "estimates" & !is.null(keep.only)) {
    # Return only named vector of mena/median/mode if thats all thats asked for
    out <- eval(parse(text=paste0("estimate_", tolower(keep.only))))
    out.as.list <- unlist(as.list(out))
    names(out.as.list) <- colnames(out)
    return(out.as.list)
  } 
  
  # Manipulate into data frame
  Bootstrap.mean.output <- as.data.frame(t(estimate_mean))
  colnames(Bootstrap.mean.output) <- "Mean"
  Bootstrap.mean.output['Parameter'] <- rownames(Bootstrap.mean.output)
  Bootstrap.mean.output <- Bootstrap.mean.output[c("Parameter", "Mean")]
  rownames(Bootstrap.mean.output) <- NULL
  
  Bootstrap.median.output <- as.data.frame(t(estimate_median))
  colnames(Bootstrap.median.output) <- "Median"
  Bootstrap.median.output['Parameter'] <- rownames(Bootstrap.median.output)
  Bootstrap.median.output <- Bootstrap.median.output[c("Parameter", "Median")]
  rownames(Bootstrap.median.output) <- NULL
  
  Bootstrap.output <- cbind(Bootstrap.mean.output, 
                          Bootstrap.median.output["Median"])
  
  # Return this data frame if thats all the user requires
  if (what == "estimates") {
    return(Bootstrap.output)
  }
  
  # Append precision information for parameters
  if (what %in% c("all", "precision")) {
    precision.stats <- SOObject@Estimation@PrecisionPopulationEstimates$Bootstrap$PrecisionEstimates$data  
    if (is.null(precision.stats)) {
      stop("Section Estimation:PrecisionPopulationEstimates$Bootstrap$PrecisionEstimates not found in SO Object")
    }
    Bootstrap.output <- cbind(Bootstrap.output, precision.stats[setdiff(colnames(precision.stats), "Parameter")])
  } 
  
  
  # Append Confidence Interval information for parameters
  if (what %in% c("all", "intervals")) {
    perc <- SOObject@Estimation@PrecisionPopulationEstimates$Bootstrap$Percentiles$data 
    if (is.null(perc)) {
      stop("Section Estimation:PrecisionPopulationEstimates$MLE$AsymptoticCI not found in SO Object")
    }
    
    # Manipulate into data frame
    percentiles.output <- as.data.frame(t(perc)[-1, ])
    colnames(percentiles.output) <- paste("Perc_", perc[["Percentile"]], sep="")
    percentiles.output["Parameter"] <- setdiff(rownames(percentiles.output), "Percentile")
    percentiles.output <- percentiles.output[c("Parameter", paste("Perc_", perc[["Percentile"]], sep=""))]
    rownames(percentiles.output) <- NULL

    Bootstrap.output <- cbind(Bootstrap.output, percentiles.output[setdiff(colnames(percentiles.output), "Parameter")])
  } 

  if (!is.null(keep.only)) {

    stopifnot(tolower(keep.only) %in% c("mean", "median"))

    # Drop the other central tendency statistics and only keep the one mentioned. 
    drop.indicies <- grep(tolower(keep.only), tolower(names(Bootstrap.output))[2:3], invert = TRUE)
    Bootstrap.output <- Bootstrap.output[-(drop.indicies+1)]
  }
  
  return(Bootstrap.output)
  
}


.readParameterObjectFromAssociatedMDL <- function(SOObject) {
	
	mdlFile <- sub(x=SOObject@.pathToSourceXML, pattern="\\.SO\\.xml$", replacement=".mdl")
	if (!file.exists(mdlFile)) {
		stop("getPopulationParameters() for this SO object expects MDL file at ", mdlFile, ", perhaps it has been moved or deleted")
	}
	
	parObjs <- getParameterObjects(mdlFile)
	if (length(parObjs) > 1) {
		stop("More than one parameter object found in MDL file ", mdlFile)
	}
	
	parObjs[[1]]
}

.deriveStructuralParametersFromAssociatedMDL <- function(SOObject) {
	names(.readParameterObjectFromAssociatedMDL(SOObject)@STRUCTURAL)
}

.deriveVariabilityParametersFromAssociatedMDL <- function(SOObject) {
	names(.readParameterObjectFromAssociatedMDL(SOObject)@VARIABILITY)
}



# ---------------------------------------------------------------------------------------

# TODO: Need to know whether these lower level functions are still necessary

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


# #' Create a method to fetch the value of Likelihood Slot
# setGeneric(name="getLikelihood",
#            def=function(SOObject)
#            {
#               standardGeneric("getLikelihood")
#            }
# )
# setMethod(f="getLikelihood",
#           signature="StandardOutputObject",
#           definition=function(SOObject)
#           { 
#            Likelihood <- SOObject@Estimation@Likelihood

#           L = list()
#           if ("LogLikelihood" %in% names(Likelihood)) {
#             A = list(LogLikelihood=Likelihood[["LogLikelihood"]])
#             L <- c(L, A)
#           }
#           if ("Deviance" %in% names(Likelihood)) {
#             B <- list(Deviance=Likelihood[["Deviance"]]) 
#             L <- c(L, B)
#           }
#           if ("IndividualContribToLL" %in% names(Likelihood)) {
#             C <- list(IndividualContribToLL=Likelihood[["IndividualContribToLL"]][["data"]]) 
#             L <- c(L, C)
#           }          
#           if ("AIC" %in% names(Likelihood[["InformationCriteria"]])) {
#             D <- list(AIC=Likelihood[["InformationCriteria"]][["AIC"]])
#             L <- c(L, D)
#           }   
#           if ("BIC" %in% names(Likelihood[["InformationCriteria"]])) {
#             E <- list(BIC=Likelihood[["InformationCriteria"]][["BIC"]])
#             L <- c(L, E)
#           } 
#           if ("DIC" %in% names(Likelihood[["InformationCriteria"]])) {
#             F <- list(DIC=Likelihood[["InformationCriteria"]][["DIC"]])
#             L <- c(L, F)
#           }       

#            pprintList(L, "Likelihood")
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
