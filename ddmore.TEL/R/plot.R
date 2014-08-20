##############################################################
#' plot
#'
#' Plots the data within the object of class \code{mogObj}. Uses data specified 
#' in the \code{dataobj} object, information within the \code{mdlobj} INPUT_VARIABLES 
#' to define the dependent and independent variables. Most of the options take 
#' their default values from xpose.data object but may be overridden by supplying 
#' them as arguments.
#'
#' @usage plot(object, by, group, sourceDir=getwd(), deriveVariables=TRUE, categoricalAsFactor=TRUE, recode=TRUE, ...)
#'
#' @param object Object of class \code{mogObj}
#' @param by a data variable specified within the \code{dataObj}. Defines the 
#' conditioning for each panel in the plot.
#' @param group defines grouping variables within each panel, usually varying 
#' graphical parameters for each level of the grouping variable.
#' @param IDVVar (optional) character string denoting data column which is the indepndent
#' variable. Defaults to "IDV"
#' @param ... other arguments to be passed through to the plotting function 
#' lattice graphics options).
#'
#' @details A wide array of extra options controlling \code{xyplot} are available. See 
#' \code{xpose.plot.default} and \code{xpose.panel.default} for details.
#'
#' @return Returns an xyplot of DV vs IDV (as defined in the \code{mdlobj}).
#'
#' @export
#' @docType methods
#' @rdname plot-methods
#'
#' @examples
#' ## Create an S4 object of class mclobj.
#' ThamMDLObjects<- getMDLObjects("2008ThamJCCR.mdl",
#'   				names=c("tumour_size_dat","tumour_size_par",
#' 					"tumour_size_mdl", "tumour_size_task"))
#'
#' myThamMOG <- as.mogObj(ThamMDLObjects)
#' plot(myThamMOG)
#' 
#' @include telClasses.R
#' @include utils.R

setGeneric("plot", function(object, by, group, IDVVar="IDV", sourceDir=getwd(), deriveVariables=FALSE, 
  categoricalAsFactor=FALSE, recode=FALSE, ...) { 
    standardGeneric("plot")
 })

#' @rdname read-methods
#' @aliases read,mogObj,mogObj-method
setMethod("plot", signature=signature(object="mogObj"), 
  function(object, by, group, IDVVar="IDV", sourceDir=getwd(), deriveVariables=FALSE, 
    categoricalAsFactor=FALSE, recode=FALSE, ...){
  
  # Extract out dataObj:
  obj <- object@dataObj
  
  # Then call the dataObj method
  #print(sourceDir)
  plot(obj, by, group, IDVVar, sourceDir, deriveVariables, categoricalAsFactor, recode, ... )

})

#' @rdname read-methods
#' @aliases read,dataObj,dataObj-method
setMethod("plot", signature=signature(object="dataObj"), 
  function(object, by, group, IDVVar="IDV", sourceDir=getwd(), deriveVariables=FALSE, 
    categoricalAsFactor=FALSE, recode=FALSE, ...){
  # First, read in the data:
  dat <- read(object, sourceDir, deriveVariables, categoricalAsFactor,  recode)

  nam <- names(dat)
  
  # Create a formula string to evaluate
  cmd <- paste("DV", IDVVar, sep="~")
  
  # Function requires columns called DV and IDV, and will error if not:
  if(!all(c("DV", IDVVar)%in%nam)){stop("Columns DV and/or IDV are not present in the data set. Unable to produce plot.")}
 
  if(!missing(by)){
    if(!by%in%nam){stop(paste("Column", by, "is not present in the data set. Unable to produce plot."))}
    # If by variable included, add to formula string for evaluation
    cmd <- paste(cmd, "|", by)
  
  }
  

  if(!missing(group)){
    
    if(!group%in%nam){stop(paste("Column", group, "is not present in the data set. Unable to produce plot."))}

    xyplot(formula(cmd), group=dat[,group], data=dat, ...)
  
  } else {
  
    xyplot(formula(cmd), data=dat, ...)
  }
  
})
 
 
 
 
 
 
 
 
 
 
 
 
 