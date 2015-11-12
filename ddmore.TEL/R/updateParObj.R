##############################################################
#' updateParObj
#'
#' Updates an object of class \linkS4class{parObj}. It allows the user to specify
#' new initial values, bounds (lower, upper), prior distributions, and whether to fix or
#' unfix a variable. Typically this method is used to update initial parameter values
#' from current values to output values from an estimation step.
#' 
#' @usage updateParObj(object, block, item, with)
#'
#' @param object An object of class \linkS4class{parObj}
#' @param block Which block ("STRUCTURAL", "VARIABILITY" or "DECLARED_VARIABLES") to update
#' @param item Identifies which element (e.g. variable) within a block to update;
#'        corresponds to a named list item within the block. Accepts a vector for
#'        updating multiple variables.
#' @param with A named list specifying the attributes and their values, for the items
#'        identified through "block" and "item"; the attributes must already exist, and
#'        boolean attribute values should be enclosed in double quotes as per string
#'        attribute values
#' @return The updated object of class parObj
#' 
#'  
#' @examples
#' # Change the initial value for a structural parameter
#' updateParObj(warfarinMOG@@parObj, "STRUCTURAL", "POP_V", list(value="2"))
#' # Change the bounds of a variability parameter
#' updateParObj(warfarinMOG@@parObj, "VARIABILITY", "CORR_PPV_CL_V", list(lo=-0.5, hi=+0.5))
#' # Fix the value of a parameter
#' updateParObj(nockMOG@parObj, "VARIABILITY", "SIGMA", list(fix="true"))
#' 
#' @note When trying to update multiple attributes across multiple variables e.g. via
#'       \code{p <- updateParObj(p, "STRUCTURAL", names(p<at>STRUCTURAL), list(value=0.5565, lo=0.6656))}
#'       then this won't neccessarily update the correct values, since the names of the
#'       attributes aren't checked in this case, so which attributes get which values is
#'       arbitrary. This is raised as a SourceForge ticket #186.
#'
#' @export
#' @docType methods
#' @rdname updateParObj-methods
#' @include Classes.R

setGeneric("updateParObj", function(object, block, item, with) { 
  standardGeneric("updateParObj")
})

#' @rdname updateParObj-methods
#' @aliases read,parObj,parObj-method
setMethod("updateParObj", signature=signature(object="parObj"), function(object, block, item, with) {
			
  if (!all(block%in%c("STRUCTURAL", "VARIABILITY", "PRIOR_PARAMETERS"))) {
	  stop("Block provided is not one of \"STRUCTURAL\", \"VARIABILITY\" or \"PRIOR_PARAMETERS\"")
  }
  if (!all(item%in%names(eval(parse(text=paste0("object", "@", block)))))) {
	  stop("Item (e.g. variable) provided does not exist in given block \"", block, "\"")
  }
  for (i in item) {
	  if (!all(names(with)%in%names(eval(parse(text=paste0("object", "@", block, "$", i)))))) {
		  stop("Names given do not exist in given block \"", block, "\"and item \"", i, "\"")
	  }
  }
  if (!is.list(with)) {
	  stop("Item to update with is not of list type; if a single parameter is being updated, this should still be enclosed in a list")
  }

  # Save the current value of this option
  useFancyQuotesSetting <- getOption("useFancyQuotes")
  # Double quotes are used to enclose the values of the variables in the update command to be eval()uated
  options(useFancyQuotes=FALSE)
  
  x <- 
  eval(
    parse(
      text = paste0("object", "@", block, "$", item, "$", rep(names(with), each=length(item)), " <- ", dQuote(unlist(with)))
    )
  )

  # Reset this option back to what it was
  options(useFancyQuotes=useFancyQuotesSetting)

  # Return the updated object:
  return(object)

})


