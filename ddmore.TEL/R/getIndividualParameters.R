#' getIndividualParameters
#'
#' This function acts on an object of class StandardOutputObject 
#' and presents information to the user about the parameter values for the individuals.  
#' 
#' Depending on the target software and estimation method, values 
#' such as Objective Function Value (OFV), -2*log-likelihood and/or Information criteria
#' such as AIC, DIC, BIC may be returned. In addition, any warnings, errors and info messages 
#' from the log will also be stored in the returned output. If there are any errors of warnings, 
#' these will also be printed to the console.   
#'
#' @param SOObject an object of class StandardOutputObject, the output from an 
#'     estimation task.
#' @param what a character vector specifying which measure of central tendency to return, 
#'     either "Mean" (default), "Median" or "Mode".
#'
#' @return A nested list with two elements:.
#'   \describe{
#'     \item{"Liklihood"}{All information from the Liklihood slot of the SOObject}
#'  \item{"Messages"}{A nested list for each message grouped by message type ("Info", "Error", and/or "Warning" if present)}
#' }
#'
#' @examples getIndividualParameters(object)
#'
#' @export 
getIndividualParameters <- function(SOObject, what=){ 

	output = data.frame()

	estimateNames <- names(SO@Estimation@IndividualEstimates$Estimates)
	randomNames <- names(SO@Estimation@IndividualEstimates$RandomEffects)


	if ("Mean" %in% estimateNames) {
	  output = c(output, SO@Estimation@IndividualEstimates$Estimates[["Mean"]][["data"]])
	  output = as.data.frame(output)
	}

	if ("EffectMean" %in% randomNames) {

	  df2 = SO@Estimation@IndividualEstimates$RandomEffects[["EffectMean"]][["data"]]
	  
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

}