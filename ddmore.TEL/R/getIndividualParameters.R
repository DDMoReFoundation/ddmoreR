#' getIndividualParameters
#'
#' This function acts on an object of class StandardOutputObject 
#' and presents information to the user about the parameter values for the individuals.  
#' 
#' @param SOObject an object of class StandardOutputObject, the output from an 
#'     estimation task.
#' @param what a character vector specifying which measure of central tendency to return, 
#'     either "Mean" (default), "Median" or "Mode".
#'
#' @return A data frame with number of rows equal to the number of individuals,
#'     and number of columns corresponding to the number of parameters.
#'
#' @examples 
#' mlx <- LoadSOObject("UseCase2.SO.xml")
#' getIndividualParameters(mlx)
#'
#' @export 
getIndividualParameters <- function(SOObject, what="Mean") { 

	# Error checking
	if (!(tolower(what) %in% c("mean", "mode", "median"))) {
		stop(paste0("Value for 'what' parameter was not recognised, received: ", what))
	}

	# Determine slots to look up and those available
	estimateSlotNames = list(mean="Mean", mode="Mode", median="Median")
	randomSlotNames = list(mean="EffectMean", mode="EffectMode", median="EffectMedian")

	targetEstimateName = estimateSlotNames[[tolower(what)]]
	targetRandomName = randomSlotNames[[tolower(what)]]

	output = data.frame()

	actualEstimateNames <- names(SOObject@Estimation@IndividualEstimates$Estimates)
	actualRandomNames <- names(SOObject@Estimation@IndividualEstimates$RandomEffects)

	if (targetEstimateName %in% actualEstimateNames) {
	  output = c(output, SOObject@Estimation@IndividualEstimates$Estimates[[targetEstimateName]][["data"]])
	  output = as.data.frame(output)
	}

	if (targetRandomName %in% actualRandomNames) {

	  df2 = SOObject@Estimation@IndividualEstimates$RandomEffects[[targetRandomName]][["data"]]
	  
	  # Check column names
	  duplicateNames = intersect(names(output), names(df2))
	  duplicateNamesIdx = sapply(duplicateNames, FUN=function(x) grep(x, names(df2)))
	  
	  # raise warning if unexpected duplice column names
	  if (length(duplicateNames) > 0 && duplicateNames != "ID") {
	    warning(paste("The following duplicate column names were detected during a merge and will be dropped from the output: ", 
	                  paste(duplicateNames, collapse="\n      "), sep="\n      "))
	  } 
	  # ID column is always dropped
	  df2 <- df2[-duplicateNamesIdx]
	  
	  # Concatonate output 
	  output = c(output, df2)
	  output = as.data.frame(output)
	}

	if (length(output) == 0 ) {
		message("No values found in Estimation:IndividualEstimates slot of the SOObject")
	}

	return(output)
}


