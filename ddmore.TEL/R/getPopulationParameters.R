# ================================================================================ #
# Higher-level getter Functions for returning data from the Standard Output Object #
# ================================================================================ #

#' getPopulationParameters
#' 
#' This function acts on an object of  class \linkS4class{StandardOutputObject} and is
#' a wrapper to getMLEPopulationParameters(), getBayesianPopulationParameters() and 
#' getBootstrapPopulationParameters() presenting estimates of the STRUCTURAL
#' and VARIABILITY parameters along with any associated measures of uncertainty
#' (standard deviation) and interval estimates if these are available.
#'
#' The values available for return from the StandardOutputObject depend on the 
#' estimation method used and how these are populated - either directly from the
#' estimation task output or subsequently via other methods e.g. bootstrapping. 
#' The function will provide suitable names for the columns depending on the 
#' methods used. So for example, although the argument for "what" accepts 
#' "precisions" this may mean SE assuming Normality, Bootstrap SE, SD from MCMC 
#' or SAEM estimation methods.
#'
#' @param SOObject	an object of class StandardOutputObject, the output from an 
#' 					estimation task.
#' @param block		character string determining which parameters to return. 
#'					Options (case insensitive) are "STRUCTURAL", "VARIABILITY"
#' 					and "all" (default).
#' @param what		character string determining what values to return (all case
#' 					insensitive):
#' 					\itemize{
#' 					  \item "estimates" - returns point estimates for the parameters.
#' 					  \item "precisions" - returns variability / uncertainty estimates
#' 										   for the parameters.
#'					  \item "intervals" - returns interval estimates for the parameters. 
#' 					  \item "all" - returns all of the above in a table if they are
#' 									present in the SOObject (default).
#' 					}
#' @param keep.only character string determining which central tendency statistic to use 
#'					for the estimate when multiple are present. Only applicable to Bayesian
#' 					which has Mean, Median and Mode; and Bootstrap which has Mean and
#' 					Median as options.
#' 
#' @return If only returning estimates by setting \code{what="estimates"} then a named vector of 
#' real values is returned to facilitate the outputs' use with the \code{update} method. 
#' If what is set to "intervals", "precisions" or "all" then a data frame containing one row for
#' each parameter is returned. The columns of which will be the parameter names, estimate values,
#' then followed by the statistics specified e.g. precision values, interval values or both.
#' 
#' @examples 
#' mlx <- LoadSOObject("UseCase2.SO.xml")
#' getPopulationParameters(mlx, what="all")
#' getPopulationParameters(mlx, what="estimates")
#' getPopulationParameters(mlx, block="STRUCTURAL", what="estimates")
#'
#' @export
getPopulationParameters <- function(SOObject, block="all", what="all", keep.only=NULL, fisServer = DDMORE.getServer(), ...) {
  
	# Parameter deprecation warning
	extraParams <- list(...)
	if ('type' %in% names(extraParams)) {
    	block <- extraParams[['type']]
    	message('The "type" parameter has been renamed to "block" for clarity. At current using both is possible though using "type" may become deprecated in the future.')
	}

    block <- tolower(block)
	what <- tolower(what)

	# Error checking
	if (class(SOObject) != "StandardOutputObject") {
		stop(paste0("getPopulationParameters() expected a StandardOutputObject as input, got a ", class(SOObject), '.'))
	}
	if (!(block %in% c("structural", "variability", "all"))) {
		stop("Unrecognised value specified for 'block' parameter. Must be one of: \"STRUCTURAL\", \"VARIABILITY\" or \"all\" (case insensitive).")
	}
	if (!(what %in% c("estimates", "precisions", "intervals", "all"))) {
		stop("Unrecognised value specified for 'what' parameter. Must be one of: \"estimates\", \"precisions\", \"intervals\" or \"all\" (case insensitive).")
	}
	
	# Assert that specific objects exist in the SO structure if they are asked for
	estimationPopulatedSlots <- getPopulatedSlots(SOObject@Estimation)
	switch (what,
		"estimates" = {
			if (!any(grepl(pattern = "^PopulationEstimates", x = estimationPopulatedSlots)))
				stop("Tried to fetch the parameter estimates, however section Estimation::PopulationEstimates was not found in the SO Object.")
		},
		"precisions" = {
			if (!any(grepl(pattern = "^PrecisionPopulationEstimates", x = estimationPopulatedSlots)))
				stop("Tried to fetch the parameter precision values, however section Estimation::PrecisionPopulationEstimates was not found in the SO Object.")
		},
		"intervals" = {
			if (!any(grepl(pattern = "^IndividualEstimates", x = estimationPopulatedSlots)))
				stop("Tried to fetch the parameter interval values, however section Estimation::IndividualEstimates was not found in the SO Object.")
		}, 
        "all" = {
            blocks <- c("^PopulationEstimates", "^PrecisionPopulationEstimates", "^IndividualEstimates")
            blocksPresent <- sapply(X = blocks, 
                FUN = function(block, slots) { 
                    any(grepl(pattern = block, x = slots)) }, 
                slots = estimationPopulatedSlots)
            if (!all(blocksPresent))
				warning("Tried to fetch the parameter estimates, however section", 
                    ifelse(test = sum(!all(blocksPresent) > 1), yes = "s ", no = " "),
                    paste(paste0("Estimation::", gsub(pattern = "\\^", replacement = "", x = blocks)[!blocksPresent], collapse = ", "), "not found in the SO Object."))
        }
	)

	
  # Cycle through estimates present, parsing as necessary

  estimates.output.list <- list()

  if ("MLE" %in% getPopulatedSlots(SOObject@Estimation@PopulationEstimates)) {
      estimates.output.list[["MLE"]] <- getMLEPopulationParameters(SOObject, what=what)
  }

  if ("Bayesian" %in% getPopulatedSlots(SOObject@Estimation@PopulationEstimates)) {
	  estimates.output.list[["Bayesian"]] <- getBayesianPopulationParameters(SOObject, what=what, keep.only=keep.only)
  }

  if (!is.null(SOObject@Estimation@PopulationEstimates@OtherMethod$Bootstrap)) {
      estimates.output.list[["Bootstrap"]] <- getBootstrapPopulationParameters(SOObject, what=what, keep.only=keep.only)
  }
  
  if (block == "structural") {

	structParams <- .deriveStructuralParametersFromAssociatedMDL(SOObject, fisServer=fisServer)
    
    # Only return the structural parameters for each type of estimate
    for (df.name in names(estimates.output.list)) {
        
        if (is.vector(estimates.output.list[[df.name]])) {

          keep.idx <- sapply(names(estimates.output.list[[df.name]]), 
              FUN = function(x) {
                  x %in% structParams
              })
          
          estimates.output.list[[df.name]] <- estimates.output.list[[df.name]][keep.idx]

        } else {

          keep.idx <- sapply(estimates.output.list[[df.name]][["Parameter"]], 
              FUN = function(x) {
				  x %in% structParams
              })
        
          estimates.output.list[[df.name]] <- estimates.output.list[[df.name]][keep.idx, ]
        
        }
    }
  }
  
  if (block == "variability") {
    
	structuralParams <- .deriveStructuralParametersFromAssociatedMDL(SOObject, fisServer=fisServer)
	variabilityParams <- .deriveVariabilityParametersFromAssociatedMDL(SOObject, fisServer=fisServer)
	
    # Only return the variability parameters for each type of estimate
    for (df.name in names(estimates.output.list)) {
		
		if (is.vector(estimates.output.list[[df.name]])) {
			
			# Temporary workaround DDMORE-1533 / SF#320
			# Treat all parameters in the SO output that are not in the STRUCTURAL block, as VARIABILITY.
			# This is because correlation parameter (type=CORR in VARIABILITY block) names
			# don't correspond with the names of the parameters in this bit of the SO.
			# The parameters in the SO are the STRUCTURAL params plus the VARIABILITY params,
			# and there are no such naming issues with the STRUCTURAL params, hence we
			# can apply this workaround to derive the VARIABILITY param names in the SO
			# rather than taking them from the VARIABILITY block.
			allVarNames <- names(estimates.output.list[[df.name]])
			variabilityParams <- setdiff(allVarNames, structuralParams)
			
			keep.idx <- sapply(allVarNames, 
				FUN = function(x) {
					x %in% variabilityParams
				})
			
			estimates.output.list[[df.name]] <- estimates.output.list[[df.name]][keep.idx]
			
		} else {
			
			# Same workaround as described above is applied here
			allVarNames <- estimates.output.list[[df.name]][["Parameter"]]
			variabilityParams <- setdiff(allVarNames, structuralParams)
    
			keep.idx <- sapply(allVarNames,
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
	
	if (is.null(x) || nrow(x) == 0) {
		return(y)
	}
	if (is.null(y) || nrow(y) == 0) {
		return(x)
	}

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


getMLEPopulationParameters <- function(SOObject, what="all") {
  
  # Extract parameter values for MLE
  df <- as.data.frame(SOObject@Estimation@PopulationEstimates@MLE)
  if (nrow(df) == 0) {
    stop("Section Estimation::PopulationEstimates::MLE not found in SO Object.")
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
  
  populatedSlotsPrecisionPopEstimatesMLE <- getPopulatedSlots(SOObject@Estimation@PrecisionPopulationEstimates@MLE, full=TRUE)
  
  if (what %in% c("all", "precisions")) {

	if ("StandardError" %in% populatedSlotsPrecisionPopEstimatesMLE) {
	  se <- as.data.frame(SOObject@Estimation@PrecisionPopulationEstimates@MLE@StandardError)
      # Append standard error precision information for parameters
      MLE.output <- row.merge.cbind(MLE.output, se, colNames="SE")
    }
	else {
      warning(paste0("Tried to fetch the parameter precision values, however section ",
        "Estimation::PrecisionPopulationEstimates::MLE::StandardError was not found in the SO Object.\n ", 
        "Omitting standard error precision values for MLE section in returned output."))
	}
	
	if ("RelativeStandardError" %in% populatedSlotsPrecisionPopEstimatesMLE) {
	  rse <- as.data.frame(SOObject@Estimation@PrecisionPopulationEstimates@MLE@RelativeStandardError)
      # Append relative standard error precision information for parameters
      MLE.output <- row.merge.cbind(MLE.output, rse, colNames="RSE")
    }
	else {
      warning(paste0("Tried to fetch the parameter precision values, however section ",
        "Estimation::PrecisionPopulationEstimates::MLE::RelativeStandardError was not found in the SO Object.\n ", 
        "Omitting relative standard error precision values for MLE section in returned output."))
	}
    
  }
  
  if (what %in% c("all", "intervals")) {
	  
	if ("AsymptoticCI" %in% populatedSlotsPrecisionPopEstimatesMLE) {
	  CIs <- as.data.frame(SOObject@Estimation@PrecisionPopulationEstimates@MLE@AsymptoticCI)
      # Append Confidence Interval information for parameters
      MLE.output <- row.merge.cbind(MLE.output, CIs, colNames=c("CI", "LowerBound", "UpperBound"))
    }
    else {
      warning(paste0("Tried to fetch the parameter interval values, however section ", 
        "Estimation::PrecisionPopulationEstimates::MLE::AsymptoticCI was not found in the SO Object.\n ",
        "Omitting interval values for MLE section in returned output.")) 
    }
	
  } 

  # Sort the data frame by parameters names. Merge will do this anyway be default, but its possible to 
  # have no merge operations performed due to missing SO sections. This is added for consistency in output. 
  MLE.output <- MLE.output[with(MLE.output, order(Parameter)),]

  # Remove row names as they are also stored in 'Parameter' column
  rownames(MLE.output) <- NULL

  return(MLE.output)
}

getBayesianPopulationParameters <- function(SOObject, what="all", keep.only=NULL) {
	
  populatedSlotsPopEstimatesBayesian <- getPopulatedSlots(SOObject@Estimation@PopulationEstimates@Bayesian, full=TRUE)
  
  # Extract parameter values for Bayesian
	
  Bayesian.mean.output <- NULL
  if ("PosteriorMean" %in% populatedSlotsPopEstimatesBayesian) {
	estimate_mean <- as.data.frame(SOObject@Estimation@PopulationEstimates@Bayesian@PosteriorMean)
	Bayesian.mean.output <- as.data.frame(t(estimate_mean))
    colnames(Bayesian.mean.output) <- "Mean"
    Bayesian.mean.output['Parameter'] <- rownames(Bayesian.mean.output)
    Bayesian.mean.output <- Bayesian.mean.output[c("Parameter", "Mean")]
  }
  else {
	warning(paste0("Tried to fetch the population parameter values, however section ", 
        		   "Estimation::PopulationEstimates::Bayesian::PosteriorMean was not found in the SO Object.\n ",
        		   "Omitting interval values for Bayesian section in returned output.")) 
  }
  
  Bayesian.median.output <- NULL
  if ("PosteriorMedian" %in% populatedSlotsPopEstimatesBayesian) {
    estimate_median <- as.data.frame(SOObject@Estimation@PopulationEstimates@Bayesian@PosteriorMedian)
	Bayesian.median.output <- as.data.frame(t(estimate_median))
	colnames(Bayesian.median.output) <- "Median"
	Bayesian.median.output['Parameter'] <- rownames(Bayesian.median.output)
	Bayesian.median.output <- Bayesian.median.output[c("Parameter", "Median")]
  }
  else {
  	warning(paste0("Tried to fetch the population parameter values, however section ", 
       		       "Estimation::PopulationEstimates::Bayesian::PosteriorMedian was not found in the SO Object.\n ",
       		       "Omitting interval values for Bayesian section in returned output.")) 
  }
  
  if (what == "estimates" & !is.null(keep.only)) {
      # Return only named vector of mean/median/mode if that's all that's asked for
      out <- eval(parse(text=paste0("estimate_", tolower(keep.only))))
      out.as.list <- unlist(as.list(out))
      names(out.as.list) <- colnames(out)
      return(out.as.list)
  } 
  
  Bayesian.output <- row.merge.cbind(Bayesian.mean.output, Bayesian.median.output, colNames = "Median")

  # Return this data frame if that's all the user requires
  if (what == "estimates") {
    return(Bayesian.output)
  }
  
  populatedSlotsPrecisPopEstimatesBayesian <- getPopulatedSlots(SOObject@Estimation@PrecisionPopulationEstimates@Bayesian, full=TRUE)
  
  # Append precision information for parameters
  if (what %in% c("all", "precisions")) {
	  if ("StandardDeviation" %in% populatedSlotsPrecisPopEstimatesBayesian) {
    	sdp <- as.data.frame(SOObject@Estimation@PrecisionPopulationEstimates@Bayesian@StandardDeviation)
      	Bayesian.output <- row.merge.cbind(Bayesian.output, sdp, colNames="SDP")
	  }
	  else {
        warning(paste0("Tried to fetch the parameter precision values, however section ",
			           "Estimation::PrecisionPopulationEstimates::Bayesian::StandardDeviation was not found in the SO Object.\n ",
        			   "Omitting precision values for Bayesian section in returned output."))
	  }
  } 
  
  # Append Confidence Interval information for parameters
  if (what %in% c("all", "intervals")) {
	  if ("PercentilesCI" %in% populatedSlotsPrecisPopEstimatesBayesian) {
        CIs <- as.data.frame(SOObject@Estimation@PrecisionPopulationEstimates@Bayesian@PercentilesCI)
		
	      # Manipulate into data frame
	      CI.output <- as.data.frame(t(CIs)[-1, ])
	      colnames(CI.output) <- paste("Perc_", CIs[["Percentile"]], sep="")
	      CI.output["Parameter"] <- setdiff(rownames(CI.output), "Percentile")
	      CI.output <- CI.output[c("Parameter", paste("Perc_", CIs[["Percentile"]], sep=""))]
	
	      Bayesian.output <- row.merge.cbind(Bayesian.output, CI.output, colNames=setdiff(colnames(CI.output), "Parameter"))
	  }
	  else {
	    warning(paste0("Tried to fetch the parameter precision values, however section ",
        			   "Estimation::PrecisionPopulationEstimates::Bayesian::PercentilesCI was not found in the SO Object.\n ", 
        			   "Omitting interval values for Bayesian section in returned output."))
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

getBootstrapPopulationParameters <- function(SOObject, what="all", keep.only=NULL) {

  if (is.null(SOObject@Estimation@PopulationEstimates@OtherMethod$Bootstrap)) {
    stop("Section Estimation::PopulationEstimates::OtherMethod[Bootstrap] not found in SO Object")
  }
  
  populatedSlotsPopEstimatesBootstrap <- getPopulatedSlots(SOObject@Estimation@PopulationEstimates@OtherMethod$Bootstrap, full=TRUE)
  
  # Extract parameter values for Bootstrap
  
  Bootstrap.mean.output <- NULL
  if ("Mean" %in% populatedSlotsPopEstimatesBootstrap) {
  	estimate_mean <- as.data.frame(SOObject@Estimation@PopulationEstimates@OtherMethod$Bootstrap@Mean)
	  
	# Manipulate into data frame
    Bootstrap.mean.output <- as.data.frame(t(estimate_mean))
    colnames(Bootstrap.mean.output) <- "Mean"
    Bootstrap.mean.output['Parameter'] <- rownames(Bootstrap.mean.output)
    Bootstrap.mean.output <- Bootstrap.mean.output[c("Parameter", "Mean")]
  }
  else {
	  warning(paste0("Tried to fetch the population parameter values, however section ", 
        		     "Estimation::PopulationEstimates::OtherMethod[Bootstrap]::Mean was not found in the SO Object.\n ",
        		     "Omitting interval values for Bootstrap section in returned output.")) 
  }
  
  Bootstrap.median.output <- NULL
  if ("Median" %in% populatedSlotsPopEstimatesBootstrap) {
  	estimate_median <- as.data.frame(SOObject@Estimation@PopulationEstimates@OtherMethod$Bootstrap@Median)
	
    Bootstrap.median.output <- as.data.frame(t(estimate_median))
    colnames(Bootstrap.median.output) <- "Median"
    Bootstrap.median.output['Parameter'] <- rownames(Bootstrap.median.output)
    Bootstrap.median.output <- Bootstrap.median.output[c("Parameter", "Median")]
  }
  else {
	  warning(paste0("Tried to fetch the population parameter values, however section ", 
        		     "Estimation::PopulationEstimates::OtherMethod[Bootstrap]::Mean was not found in the SO Object.\n ",
        		     "Omitting interval values for Bootstrap section in returned output.")) 
  }
  
  if (what == "estimates" & !is.null(keep.only)) {
    # Return only named vector of mean/median/mode if that's all that's asked for
    out <- eval(parse(text=paste0("estimate_", tolower(keep.only))))
    out.as.list <- unlist(as.list(out))
    names(out.as.list) <- colnames(out)
    return(out.as.list)
  } 
  
  Bootstrap.output <- row.merge.cbind(Bootstrap.mean.output, Bootstrap.median.output, colNames = "Median")

  # Return this data frame if that's all the user requires
  if (what == "estimates") {
    return(Bootstrap.output)
  }

  if (is.null(SOObject@Estimation@PrecisionPopulationEstimates@OtherMethod$Bootstrap)) {
	  warning(paste0("Tried to fetch the parameter precision values, however section ",
        			  "Estimation::PrecisionPopulationEstimates::OtherMethod[Bootstrap] was not found in the SO Object.\n ", 
        			  "Omitting precision values for Bootstrap section in returned output."))
  }
  
  else {
  
	  populatedSlotsPrecisPopEstimatesBootstrap <- getPopulatedSlots(SOObject@Estimation@PrecisionPopulationEstimates@OtherMethod$Bootstrap, full=TRUE)
	  
	  # Append precision information for parameters
	  if (what %in% c("all", "precisions")) {
		  if ("StandardDeviation" %in% populatedSlotsPrecisPopEstimatesBootstrap) {
	    	precision.stats <- as.data.frame(SOObject@Estimation@PrecisionPopulationEstimates@OtherMethod$Bootstrap@StandardDeviation)
	        Bootstrap.output <- row.merge.cbind(Bootstrap.output, precision.stats, colNames=setdiff(colnames(precision.stats), "Parameter"))      
		  }
    	  else {
		      warning(paste0("Tried to fetch the parameter precision values, however section ",
							 "Estimation::PrecisionPopulationEstimates::OtherMethod[Bootstrap]::StandardDeviation was not found in the SO Object.\n ", 
							 "Omitting precision values for Bootstrap section in returned output."))
				
		  }
	  }
	  
	  # Append Confidence Interval information for parameters
	  if (what %in% c("all", "intervals")) {
		  if ("PercentilesCI" %in% populatedSlotsPrecisPopEstimatesBootstrap) {
		    perc <- as.data.frame(SOObject@Estimation@PrecisionPopulationEstimates@OtherMethod$Bootstrap@PercentilesCI)
			
		      # Manipulate into data frame
		      percentiles.output <- as.data.frame(t(perc)[-1, ])
		      colnames(percentiles.output) <- paste("Perc_", perc[["Percentile"]], sep="")
		      percentiles.output["Parameter"] <- setdiff(rownames(percentiles.output), "Percentile")
		      percentiles.output <- percentiles.output[c("Parameter", paste("Perc_", perc[["Percentile"]], sep=""))]
		      rownames(percentiles.output) <- NULL
		
		      Bootstrap.output <- row.merge.cbind(Bootstrap.output, percentiles.output, 
		    	    colNames=setdiff(colnames(percentiles.output), "Parameter"))
		  }
		  else {
		      warning(paste0("Tried to fetch the parameter precision values, however section ",
					         "Estimation::PrecisionPopulationEstimates::OtherMethod[Bootstrap]::PercentilesCI was not found in the SO Object.\n ", 
	        				 "Omitting interval values for Bootstrap section in returned output."))
		  }
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


