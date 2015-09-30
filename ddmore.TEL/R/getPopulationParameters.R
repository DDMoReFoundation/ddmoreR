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
#' @param block character string determining which parameters to return. 
#'    Options are "STRUCTURAL", "VARIABILITY" and "all" (default).
#' @param what character vector determining what values to return:
#'  \itemize{
#'    \item "estimates" - returns point estimates for the parameters.
#'    \item "precision" - returns variability / uncertainty estimates for the 
#'        parameters.
#'    \item "intervals" - returns interval estimates for the parameters. 
#'    \item "all" - returns all of the above in a table if they are present in the SOObject (default).}
#' @param keep.only character string determining which central tendency statistic to use 
#'      for the estimate when multiple are present. Only applicable to Bayesian which
#'      has Mean, Median and  Mode; and Bootstrap which has Mean and Median as options.   
#' 
#' @return If only returning estimates by setting \code{what="estimates"} then a named vector of 
#' real values is returned to fascilitate the outputs use with the \code{update} method. 
#' If what is set to "intervals", "precision" or "all" then a data frame 
#' containing one row for each parameter is returned. The columns of which will be the parameter 
#' names, estimate values, then followed by the statistics specified e.g. precision values, 
#' interval values or both
#' 
#' @examples getPopulationParameters(object, block="ALL", what="all")  
#'
#' @seealso getPopulationEstimates, getPrecisionPopulationEstimates
#'
#' @export
getPopulationParameters <- function(SOObject, block="all", what="all", keep.only=NULL, ...) {
  
  # Parameter deprecation warning
  extraParams = list(...)
  if ('type' %in% names(extraParams)) {
    block = extraParams[['type']]
    message('The "type" parameter has been renamed to "block" for clarity. At current using both is possible though using "type" may become deprecated in the future.')
  }

  # Error checking  #
  # --------------- #
  if (!(tolower(block) %in% c("structural", "variability", "all"))) {
      stop('Parameter "block" must be one of: "STRUCTURAL", "VARIABILITY" or "ALL".')
  } 
  block = tolower(block)

  if (!(tolower(what) %in% c("estimates", "precision", "intervals", "all"))) {
      stop('Parameter "what" must be one of: "estimates", "precision", "intervals" or "all".')
  }
  what = tolower(what)

  stopifnot(isS4(SOObject) & class(SOObject) == "StandardOutputObject")

  # Assert that objects exist in SO if they are asked for
  if (is.empty(SOObject@Estimation@PopulationEstimates) && what == "estimates") {
    stop("Tried to fetch the parameter estimates, however section Estimation:PopulationEstimates was not found in the SO Object")
  }
  if (is.empty(SOObject@Estimation@PrecisionPopulationEstimates) && what == "precision") {
    stop("Tried to fetch the parameter precision values, however section Estimation:PrecisionPopulationEstimates was not found in the SO Object")
  }
  if (is.empty(SOObject@Estimation@IndividualEstimates) && what == "intervals") {
    stop("Tried to fetch the parameter interval values, however section Estimation:IndividualEstimates was not found in the SO Object")
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
  
  if (block == "structural") {

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
  
  if (block == "variability") {
    
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


#' Utility to merge extracted dataframe columns by rownames in the following functions
#'
#'  getMLEPopulationParameters
#'  getBayesianPopulationParameters
#'  getBootstrapPopulationParameters
#'
row.merge.cbind <- function(x, y, colNames) {

    # TODO: Temporary fix for inconsistency of column names between SO and the spec 
    colnames(y)[colnames(y)=="parameter"]   <- "Parameter"

    # Input checking for x dataframe 
    if (is.null(x[["Parameter"]])) {
      stop("'Parameter' column not found in SO, cannot merge data frames by parameter values." )
    }  else if (any(rownames(x) != x[["Parameter"]])) {
      rownames(x) <- x[['Parameter']] 
    } 

    # Input checking for y dataframe
    if (is.null(y[["Parameter"]])) {
      stop("'Parameter' column not found in SO, cannot merge data frames by parameter values." )
    }  else if (any(rownames(y) != y[["Parameter"]])) {
      rownames(y) <- y[['Parameter']] 
    } 

    # Append y to x with merge by rownames and fill non overlapping values with NAs
    x <- merge(x, y[colNames], by="row.names", all.x=TRUE)
    rownames(x) <- x[["Row.names"]]

    # Drop unnecessary Row.names column added by merge 
    x <- x[ , -which(names(x) %in% c("Row.names"))]

    return(x)
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
  
  if (what %in% c("all", "precision")) {

    se <- SOObject@Estimation@PrecisionPopulationEstimates$MLE$StandardError$data  
    rse <- SOObject@Estimation@PrecisionPopulationEstimates$MLE$RelativeStandardError$data

    if (is.null(se)) {
      warning(paste0("Tried to fetch the parameter precision values, however section ",
        "Estimation:PrecisionPopulationEstimates$MLE$StandardError was not found in the SO Object\n", 
        "Omitting standard error precision values for MLE section in returned output."))
    } else {

      MLE.output <- row.merge.cbind(MLE.output, se, colNames="SE")

    }
    if (is.null(rse)) {
      warning(paste0("Tried to fetch the parameter precision values, however section ",
        "Estimation:PrecisionPopulationEstimates$MLE$RelativeStandardError was not found in the SO Object\n", 
        "Omitting relative standard error precision values for MLE section in returned output."))
    } else {

      # Append Precision information for parameters
      MLE.output <- row.merge.cbind(MLE.output, rse, colNames="RSE")
    }
  }
  
  if (what %in% c("all", "intervals")) {
    CIs <- SOObject@Estimation@PrecisionPopulationEstimates$MLE$AsymptoticCI$data  
    
    if (is.null(CIs)) {
      warning(paste0("Tried to fetch the parameter interval values, however section ", 
        "PrecisionPopulationEstimates$MLE$AsymptoticCI was not found in the SO Object\n",
        "Omitting interval values for MLE section in returned output.")) 
    } else {

      # Append Confidence Interval information for parameters
      MLE.output <- row.merge.cbind(MLE.output, CIs, colNames=c("CI", "LowerBound", "UpperBound"))
    }
  } 

  # Sort the data frame by parameters names. Merge will do this anyway be default, but its possible to 
  # have no merge operations performed due to missing SO sections. This is added for consistency in output. 
  MLE.output = MLE.output[with(MLE.output, order(Parameter)),]

  # Remove row names as they are also stored in 'Parameter' column
  rownames(MLE.output) <- NULL

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
  
  Bayesian.median.output <- as.data.frame(t(estimate_median))
  colnames(Bayesian.median.output) <- "Median"
  Bayesian.median.output['Parameter'] <- rownames(Bayesian.median.output)
  Bayesian.median.output <- Bayesian.median.output[c("Parameter", "Median")]
  
  Bayesian.mode.output <- as.data.frame(t(estimate_mode))
  colnames(Bayesian.mode.output) <- "Mode"
  Bayesian.mode.output['Parameter'] <- rownames(Bayesian.mode.output)
  Bayesian.mode.output <- Bayesian.mode.output[c("Parameter", "Mode")]
  
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
      warning(paste0("Tried to fetch the parameter precision values, however section ",
        "Estimation:PrecisionPopulationEstimates$Bayesian$StandardDeviationPosterior was not found in the SO Object\n", 
        "Omitting precision values for Bayesian section in returned output."))
    } else {

      Bayesian.output <- row.merge.cbind(Bayesian.output, sdp, colNames="SDP")
    }
  } 
  
  # Append Confidence Interval information for parameters
  if (what %in% c("all", "intervals")) {
    CIs <- SOObject@Estimation@PrecisionPopulationEstimates$Bayesian$PercentilesCI$data 
    if (is.null(CIs)) {
      warning(paste0("Tried to fetch the parameter precision values, however section ",
        "Estimation:PrecisionPopulationEstimates$Bayesian$PercentilesCI was not found in the SO Object\n", 
        "Omitting interval values for Bayesian section in returned output."))
    } else {
    
      # Manipulate into data frame
      CI.output <- as.data.frame(t(CIs)[-1, ])
      colnames(CI.output) <- paste("Perc_", CIs[["Percentile"]], sep="")
      CI.output["Parameter"] <- setdiff(rownames(CI.output), "Percentile")
      CI.output <- CI.output[c("Parameter", paste("Perc_", CIs[["Percentile"]], sep=""))]

      Bayesian.output <- row.merge.cbind(Bayesian.output, CI.output, colNames=setdiff(colnames(CI.output), "Parameter"))
    }
  } 

  if (!is.null(keep.only)) {

    stopifnot(tolower(keep.only) %in% c("mean", "median", "mode"))

    # Drop the other central tendency statistics and only keep the one mentioned. 
    drop.indicies <- grep(tolower(keep.only), tolower(names(Bayesian.output))[2:4], invert = TRUE)
    Bayesian.output <- Bayesian.output[-(drop.indicies+1)]
  }

  # Sort the data frame by parameters names. Merge will do this anyway be default, but its possible to 
  # have no merge operations performed due to missing SO sections. This is added for consistency in output. 
  Bayesian.output <- Bayesian.output[with(Bayesian.output, order(Parameter)),]

  # Remove row names as they are also stored in 'Parameter' column
  rownames(Bayesian.output) <- NULL
  
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
  
  Bootstrap.median.output <- as.data.frame(t(estimate_median))
  colnames(Bootstrap.median.output) <- "Median"
  Bootstrap.median.output['Parameter'] <- rownames(Bootstrap.median.output)
  Bootstrap.median.output <- Bootstrap.median.output[c("Parameter", "Median")]
  
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
      warning(paste0("Tried to fetch the parameter precision values, however section ",
        "Estimation:PrecisionPopulationEstimates$Bootstrap$PrecisionEstimates was not found in the SO Object\n", 
        "Omitting precision values for Bootstrap section in returned output."))
    } else{

      Bootstrap.output <- row.merge.cbind(Bootstrap.output, precision.stats, 
        colNames=setdiff(colnames(precision.stats), "Parameter"))      
    }

  } 
  
  # Append Confidence Interval information for parameters
  if (what %in% c("all", "intervals")) {
    perc <- SOObject@Estimation@PrecisionPopulationEstimates$Bootstrap$Percentiles$data 
    if (is.null(perc)) {
      warning(paste0("Tried to fetch the parameter precision values, however section ",
        "Estimation:PrecisionPopulationEstimates$Bootstrap$Percentiles was not found in the SO Object\n", 
        "Omitting interval values for Bootstrap section in returned output."))
    } else {
      # Manipulate into data frame
      percentiles.output <- as.data.frame(t(perc)[-1, ])
      colnames(percentiles.output) <- paste("Perc_", perc[["Percentile"]], sep="")
      percentiles.output["Parameter"] <- setdiff(rownames(percentiles.output), "Percentile")
      percentiles.output <- percentiles.output[c("Parameter", paste("Perc_", perc[["Percentile"]], sep=""))]
      rownames(percentiles.output) <- NULL

      Bootstrap.output <- row.merge.cbind(Bootstrap.output, percentiles.output, 
        colNames=setdiff(colnames(percentiles.output), "Parameter"))      
    }
    
  } 

  if (!is.null(keep.only)) {

    stopifnot(tolower(keep.only) %in% c("mean", "median"))

    # Drop the other central tendency statistics and only keep the one mentioned. 
    drop.indicies <- grep(tolower(keep.only), tolower(names(Bootstrap.output))[2:3], invert = TRUE)
    Bootstrap.output <- Bootstrap.output[-(drop.indicies+1)]
  }

  # Sort the data frame by parameters names. Merge will do this anyway be default, but its possible to 
  # have no merge operations performed due to missing SO sections. This is added for consistency in output. 
  Bootstrap.output = Bootstrap.output[with(Bootstrap.output, order(Parameter)),]

  # Remove row names as they are also stored in 'Parameter' column
  rownames(Bootstrap.output) <- NULL
  
  return(Bootstrap.output)
  
}






