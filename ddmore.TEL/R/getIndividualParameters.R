# ================================================================================ #
# Higher-level getter Functions for returning data from the Standard Output Object #
# ================================================================================ #

#' getIndividualParameters
#'
#' This function acts on an object of class \linkS4class{StandardOutputObject}
#' and returns information about the parameter values for the individuals.  
#' 
#' @param SOObject an object of class StandardOutputObject, the output from an estimation task.
#' @param what a character string (case insensitive) specifying which measure of central tendency to return, 
#' 		  either "Mean" (default), "Median" or "Mode".
#'
#' @return A dataframe, consisting of the data from the relevant slot (Mean, Median or Mode) of the
#' 		   Estimation::IndividualEstimates::Estimates object in the SO structure, merged (on the ID column)
#' 		   with the data from the relevant slot (EffectMean, EffectMode or EffectMedian) of the
#' 		   Estimation::IndividualEstimates::RandomEffects object in the SO stucture. The number of rows
#' 		   in the resulting dataframe will be the number of individuals; the number of columns
#' 		   will usually be \code{2 * number of parameters + 1} in the case that each parameter has
#' 		   an associated random effect, but this is not enforced.
#'
#' @examples 
#' mlx <- LoadSOObject("UseCase2.SO.xml")
#' getIndividualParameters(mlx)
#'
#' @export 
getIndividualParameters <- function(SOObject, what="Mean") { 

	# Ensure 'measure of central tendency' parameter is processed in a case-insensitive manner
	measureOfCentralTendency = tolower(what)

	# Error checking
	if (class(SOObject) != "StandardOutputObject") {
		stop(paste0("getIndividualParameters() expected a StandardOutputObject as input, got a ", class(SOObject), '.'))
  	}
	if (!(tolower(what) %in% c("mean", "mode", "median"))) {
		stop("Unrecognised value specified for 'what' parameter. Must be \"mean\", \"median\" or \"mode\" (case insensitive).")
	}

	# Determine slots to look up from those available
	targetEstimateSlotName = list(mean="Mean", mode="Mode", median="Median")[[tolower(what)]]
	targetRandomEffectsSlotName = list(mean="EffectMean", mode="EffectMode", median="EffectMedian")[[tolower(what)]]
	
	df1 <- NULL
	df2 <- NULL
	
	if (targetEstimateSlotName %in% getPopulatedSlots(SOObject@Estimation@IndividualEstimates@Estimates, full=TRUE)) {
		df1 <- as.data.frame(slot(SOObject@Estimation@IndividualEstimates@Estimates, targetEstimateSlotName))
	}
	
	if (targetRandomEffectsSlotName %in% getPopulatedSlots(SOObject@Estimation@IndividualEstimates@RandomEffects, full=TRUE)) {
		df2 <- as.data.frame(slot(SOObject@Estimation@IndividualEstimates@RandomEffects, targetRandomEffectsSlotName))
	}
	
	if (is.null(df1) && is.null(df2)) {
		warning("No values found for Estimates::", targetEstimateSlotName, " or RandomEffects::", targetRandomEffectsSlotName, " in Estimation::IndividualEstimates slot of the SOObject.")
		return(data.frame())
	}
	if (is.null(df2)) {
		return(df1)
	}
	if (is.null(df1)) {
		return(df2)
	}

	# Check column names for duplicates
	# Excluding ID which is always in both dataframes and is used as the join column
	duplicateNames <- setdiff(intersect(colnames(df1), colnames(df2)), "ID")
  
	# Raise warning if unexpected duplicate column names
	if (length(duplicateNames) > 0) {
    	warning(paste("The following duplicate column names were detected during a merge and the second occurrence in each case will be dropped from the resulting dataframe:", 
			paste(duplicateNames, collapse=", ")))
		df2 <- df2[, !(colnames(df2) %in% duplicateNames)]
	}

	# Finally perform the merge of the two datasets and return the resulting dataframe
	merge(df1, df2, by="ID")
}


