##############################################################
#' getEstimationInfo
#'
#' Acts on an object of class NMRun (from the RNMImport package) and retrieves 
#' information about the estimation process including any warnings, errors from 
#' the log and, depending on the target software and estimation method, values 
#' such as Objective Function Value (OFV), -2*log-likelihood, Information 
#' criteria such as AIC, DIC, BIC.
#' Currently, the function extracts the following information:
#' methodNames, nmVersionMajor, nmVersionMinor, reportFileName, controlFileName, 
#' minInfo, controlComments, objectiveFinal.
#' 
#' @usage getEstimationInfo(object)
#'
#' @param object An object of class NMRun, the output from an estimation task.
#'
#' @return A named list.
#'
#' @export
#' @docType methods
#' @rdname getEstimationInfo-methods
#'
#' @examples
#' ## Create an S4 object of class mclobj.
#' myWarfPKModel <- getMDLObjects ("warfarin_PK_CONC.mdl",
#'   				names=c("warf_PK_CONC_dat","warf_PK_CONC_par",
#' 		"warf_PK_CONC_mdl","warf_PK_CONC_task"))
#' ## Call estimate specifying each component by name
#' myRun1 <- estimate(model= myWarfPKModel$warf_PK_CONC_mdl,
#' 	data= myWarfPKModel$warf_PK_CONC_dat, 
#' 	parameters= myWarfPKModel$warf_PK_CONC_par, 
#' 	task= myWarfPKModel$warf_PK_CONC_task, 	translationLanguage="BUGS", targetSoftware="WinBUGS")
#' getEstimationInfo(myRun1)
#' $EstimationInformation
#' [1]"Inference for Bugs model at "C:/DOCUME~1/smith_mk/LOCALS~1/Temp/RtmpS89Lys/model1fc03d0c13db.txt" Fit using WinBUGS, 2 chains, each with 60000 iterations (first 10000 discarded) n.sims = 1e+05 iterations saved”
#' $DIC
#' [1] 748.0
#' $pD
#' [1] 66.3
#'
#' @include telClasses.R

setGeneric("getEstimationInfo", function(object){ 
  standardGeneric("getEstimationInfo")
})

#' @rdname getEstimationInfo-methods
#' @aliases getEstimationInfo
setMethod("getEstimationInfo", signature=signature(object="NMRun"), function(object){
 
 # Extract information from object
 
 res <- list(methodNames = object@problems[[1]]@methodNames, 
    nmVersionMajor = object@nmVersionMajor, 
    nmVersionMinor = object@nmVersionMinor, 
    reportFileName = object@reportFileInfo$fileName,
    controlFileName = object@controlFileInfo$fileName, 
    minInfo = object@problems[[1]]@minInfo, 
    controlComments = object@controlComments,
    objectiveFinal = object@problems[[1]]@objectiveFinal
  )
 
 return(res)
})


















