##############################################################
#' update
#'
#' Updates an object of class parObj. It allows the user to specify new initial values, 
#' bounds (lower, upper) and prior distributions. Typically this method is used to 
#' update initial parameter values from current values to output values from an estimation step. 
#' Optionally will pick up any changes to parameter objects via the Task Properties Object PARAMETER block.
#' 
#' @usage update(object, block, type, with)
#'
#' @param object An object of class parObj
#' @param block Which block ("STRUCTURAL", "PRIOR" or "VARIABILITY") to update
#' @param type Identifies which element within a block to update. Names correspond 
#' to named list items within each block. E.g. value, lo, hi, matrix, prior.
#' @param with Specifies the new values for the attribute identified through "block" and "type"
#'
#' @examples
#' ## Change the initial values for structural parameters
#' ## Uses the "update" method
#' ThamParObject2<-update(ThamParObject,
#' 				block="STRUCTURAL"
#'				type="value",
#'				  with=list(POP_SIZE0 =7, 
#'					    POP_TOVER =15, 
#'					    POP_AE50e =10000, 
#'					    POP_TEQ   =5)
#'				)
#'ThamParObject3<-update(ThamParObject,
#'				block="STRUCTURAL"
#'				type="fix",
#'				with=list(POP_TOVER = T))
#'
#' @return An object of class parObj
#' @docType methods
#' @rdname update-methods
#' @include telClasses.R

setGeneric("update", function(object, block, type, with){ 
  standardGeneric("update")
})

#' @rdname update-methods
#' @aliases read,parObj,parObj-method
setMethod("update", signature=signature(object="parObj"), function(object, block, type, with){
  if(!all(block%in%c("STRUCTURAL", "PRIOR", "VARIABILITY"))){stop("Block provided is not one of 'STRUCTURAL', 'PRIOR' or 'VARIABILITY'")}
  if(!all(type%in%names(eval(parse(text=paste0("object", "@", block)))))){stop("Type provided does not exist in given block.")}
  if(!all(names(with)%in%names(eval(parse(text=paste0("object", "@", block, "$", type)))))){stop("Names given do not exist in given block and type.")}
  
  # Create a string of the command, then evaluate it:
  x <- paste0("object", "@", block, "$", type, "$", names(with), "<-", unlist(with))
  eval(parse(text=x))

  # Return the updated object:
  return(object)

})








