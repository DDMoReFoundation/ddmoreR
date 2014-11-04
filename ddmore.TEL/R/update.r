##############################################################
#' update
#'
#' Updates an object of class parObj. It allows the user to specify new initial values,
#' bounds (lower, upper) and prior distributions. Typically this method is used to update
#' initial parameter values from current values to output values from an estimation step.
#' 
#' @usage update(object, block, item, with)
#'
#' @param object An object of class parObj
#' @param block Which block ("STRUCTURAL", "VARIABILITY" or "PRIOR_PARAMETERS") to update
#' @param item Identifies which element (e.g. variable) within a block to update;
#'        corresponds to a named list item within the block
#' @param with A named list specifying the attributes and their values, for the items
#'        identified through "block" and "item"; the attributes must already exist
#' @return The updated object of class parObj
#'
#' @examples
#' # Change the initial value for a structural parameter
#' update(warfarinMOG@@parObj, "STRUCTURAL", "POP_V", list(value="2"))
#' # Change the bounds of a variability parameter
#' update(warfarinMOG@@parObj, "VARIABILITY", "CORR_PPV_CL_V", list(lo=-0.5, hi=+0.5))
#' 
#' @export
#' @docType methods
#' @rdname update-methods
#' @include telClasses.R

setGeneric("update", function(object, block, item, with) { 
  standardGeneric("update")
})

#' @rdname update-methods
#' @aliases read,parObj,parObj-method
setMethod("update", signature=signature(object="parObj"), function(object, block, item, with) {
			
  if (!all(block%in%c("STRUCTURAL", "VARIABILITY", "PRIOR_PARAMETERS"))) {
	  stop("Block provided is not one of \"STRUCTURAL\", \"VARIABILITY\" or \"PRIOR_PARAMETERS\"")
  }
  if (!all(item%in%names(eval(parse(text=paste0("object", "@", block)))))) {
	  stop("Item (e.g. variable) provided does not exist in given block")
  }
  if (!all(names(with)%in%names(eval(parse(text=paste0("object", "@", block, "$", item)))))){
	  stop("Names given do not exist in given block and item")
  }
  
  # Create a string of the command, then evaluate it:
  x <- 
  eval(
    parse(
      text = paste0("object", "@", block, "$", item, "$", names(with), " <- ", unlist(with))
    )
  )

  # Return the updated object:
  return(object)

})


