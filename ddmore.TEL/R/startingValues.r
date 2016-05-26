################################################################################
#' @title startingValues
#' 
#' @description Creates parameter values from an object of class parObj or mogObj by sampling 
#' from a user-specified distribution. Default is to use a Uniform distribution 
#' with bounds given by (or derived from) the Parameter object bounds (if available). 
#' In the parameter estimation task, this task can be used to sample new starting 
#' values if the previous run has failed to converge (see "tweak_inits" in PsN). 
#' It can also be used to generate random initial values for an MCMC fit (see inits 
#' functions in R2WinBUGS examples). 
#'
#' @usage startingValues(object, distList = list(STRUCTURAL = list(), VARIABILITY = list()))
#' 
#' @param object an object of class parObj or mogObj
#' @param distList list of lists, which takes the following form: 
#' \code{list(STRUCTURAL = list(paramName1 = list(dist="rnorm", args=list(mean=0, sd=1))), VARIABILITY = list()))}
#'
#' @return A named list containing STRUCTURAL and VARIABILITY items with one value per parameter.
#' 
#' @examples 
#' fpath <- system.file(package = "ddmore", "training", "data", 
#'     "UseCase1.mdl")
#' mockServer <- ddmore:::createMockFISServer(jobStatusPollingDelay = 1)
#' DDMORE.setServer(mockServer)
#' dat <- getMDLObjects(x = fpath) 
#' ucMog <- as.mogObj(dat)
#' startingValues(object = ucMog, distList = list())
#' distList1 <- ddmore:::.createListTemplate(ucMog@@parObj)
#' distList1$STRUCTURAL$POP_CL$dist <- "rnorm"
#' distList1$STRUCTURAL$POP_CL$args <- list(mean = 100, sd = 2)
#' startingValues(object = ucMog, distList = distList1)
#' @include Classes.R
#' @export
#' @docType methods
#' @rdname startingValues-methods
setGeneric("startingValues", 
  function(object, size, replace=FALSE, prob=NULL, by="ID",...){
      standardGeneric("startingValues")
})
#' @rdname startingValues-methods
#' @aliases startingValues,mogObj,mogObj-method
setMethod("startingValues", signature=signature(object="mogObj"), 
  function(object, distList){
  # Extract out dataObj:
  obj <- object@parObj

  # Then call the dataObj method
  startingValues(obj, distList=distList)
  
})
#' @rdname startingValues-methods
#' @aliases startingValues,parObj,parObj-method
setMethod("startingValues", signature=signature(object="parObj"), 
  function(object, distList){
  
  temp <- .createListTemplate(object)
  
  # Split into variability and structural:
  st <- distList$STRUCTURAL
  vb <- distList$VARIABILITY
  
  stNam <- names(st)
  vbNam <- names(vb)
  
  # Loop through the names in distList (split by block), and overwrite the value
  # in the template if it is specified
  for(nms in  names(st)){
      temp$STRUCTURAL[[nms]] <- st[[nms]]
  }
  
  for(nmv in  names(vb)){
      temp$VARIABILITY[[nmv]] <- vb[[nmv]]
  }

  # Sample values from the distributions specified in temp for each argument
  resSt <- lapply(temp$STRUCTURAL, .caller)
  resVb <- lapply(temp$VARIABILITY, .caller)
  
  return(list(STRUCTURAL = resSt, VARIABILITY = resVb))

})

# .caller
# Function to apply do.call down a list
#
.caller <- function(theList){
  # Add argument n=1 if not already present
  funArgs <- theList$args
  funArgs$n <- 1
  res <- do.call(theList$dist, funArgs)
  return(res)
}

# Function to assign values down a list
.defaultAssigner <- function(x){

  list(dist="runif", args=list(min=0, max=1))

}

#' @name createListTemplate
#' @aliases .createListTemplate
#' @title Create List Template
#' @description Function that takes the names from a parObj object, and creates a list containing
#' the default requirements of list(dist="runif", args=list(min=0, max=1)) for each 
#' name in the object
#' @param object parObj
#' @return list with elements STRUCTURAL and VARIABILITY
#' @examples 
#' fpath <- system.file(package = "ddmore", "training", "data", 
#'     "UseCase1.mdl")
#' dat <- getMDLObjects(x = fpath) 
#' ucMog <- as.mogObj(dat)
#' startingValues(object = ucMog, distList = list())
#' ddmore:::.createListTemplate(ucMog@@parObj)

.createListTemplate <- function(object){

  # Create list of distributions and arguments for all named elements in the 
  # SRUCTURAL and VARIABILITY blocks. By default, the values provided will be
  # dist="runif", min=0, max=0 (i.e. sampling from the uniform(0,1) distribution:
  
  # STRUCTURAL block
  namesStruct <- names(object@STRUCTURAL)
  templateStruct <- lapply(namesStruct, .defaultAssigner)
  names(templateStruct) <- namesStruct
  
  # VARIABILITY block
  namesVariab <- names(object@VARIABILITY)
  templateVariab <- lapply(namesVariab, .defaultAssigner)
  names(templateVariab) <- namesVariab
  
  # Create a list containing these two lists:
  res <- list(STRUCTURAL = templateStruct, 
      VARIABILITY = templateVariab )

  return(res)
}

