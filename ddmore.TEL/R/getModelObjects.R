##############################################################
#' getModelObjects
#'
#' Retrieves Model Object (MCL Object of type "modObj") from a locally stored MDL
#' file or from a URL  and returns an S4 object of class "modObj". Slots within 
#' this object are "RAW" – a vector of character strings corresponding to the 
#' lines of MCL code within the MCL Model Object, "MODEL_INPUT_VARIABLES", 
#' "STRUCTURAL_PARAMETERS", "VARIABILITY_PARAMETERS", "GROUP_VARIABLES", 
#' "RANDOM_VARIABLE_DEFINITION", "INDIVIDUAL_VARIABLES", "MODEL_PREDICTION", 
#' "OBSERVATION". MODEL_INPUT_VARIABLES is a named list of lists, 
#' STRUCTURAL_PARAMETERS, VARIABILITY_PARAMETERS, GROUP_VARIABLES, 
#' RANDOM_VARIABLE_DEFINITION and OBSERVATION are vectors of character strings. 
#' MODEL_PREDICTION is a named list containing vectors of strings ODE and LIBRARY.
#'
#' @details Unlike other get...Objects functions, the parsing involved in 
#' getModelObject is minimal, simply identifying model blocks by name. Users are 
#' not generally expected to change the model object via TEL, except in very few 
#' cases e.g. stepwise covariate model building via a configuration file 
#' (as in the "scm" method within Perl speaks NONMEM, PsN).
#' It is assumed that only ONE Model object is contained within an .mdl file.
 
#'
#' @usage getModelObject(file, name)
#'
#' @param x File path or URL of the .mdl file containing the data object.
#'
#' @return an S4 Object of class "modObj".
#'
#' @export
#' @docType methods
#' @rdname getModelObjects-methods
#'
#' @examples
#' ## Retrieve from the DDMoRe Library
#' ThamParObject <- getParameterObjects(file="http://ddmore.eu/model-repository/model/download/127.17?filename=2008ThamJCCR.mdl")
#' 
#' ## retrieve from a local .mdl file
#' ThamParObject <- getParameterObjects("2008ThamJCCR.mdl") 
#' 
#' ## retrieve a named item from a local .mdl file
#' tumourSizeParObject<-getParameterObjects("2008ThamJCCR.mdl",
#'   				name=" tumour_size_par")
#' 
#' ThamParObject[[1]] ## first parameter object within ThamParObject
#' ## Using parameter object name from MCL code
#' tumourSizeParObject<-ThamParObject$tumour_size_par
#' 
#' ## Change the initial values for structural parameters
#' ## Uses the “update” method
#' ThamParObject2<-update(ThamParObject@STRUCTURAL,
#' 				what=”value”,
#' 				with=list(POP_SIZE0=7, 
#' 					    POP_TOVER=15, 
#' 					    POP_AE50=10000, 
#' 					    POP_TEQ=5)
#' 				)
#' 
#'
#' @include telClasses.R

setGeneric("getModelObjects", function(x, name){ 
  ## create object in R from parser:
  #x <- .callParser(loc=file, obType="dataObject", obName=name) # should return a named list of objects

  # Extract only data objects
  #res <-  par[sapply(par, is.dataObj)]
  warning("No parsing method implemented yet")
  standardGeneric("getParameterObjects")
})

#' @rdname getModelObjects-methods
#' @aliases getModelObjects,mogObj,mogObj-method
setMethod("getModelObjects", signature=signature(x="mogObj"), function(x){
   return(x@modObj)
})






