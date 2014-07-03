#' Estimate  
#'
#' Passes a MDL file or MOG object (class mogObj) to the target software for execution.
#'
#' @author Jonathan Chard
#' @param x An object of class mogObj or an MDL file.
#' @param target S string specifying the target software. Currently, possible targets are "NONMEM", "PsN" and "BUGS".
#' @param subfolder Specify the name of a subfolder within the current working directory in which to store the results.
#' @param collect Logical dictating if the results should be collected.
#' @param clearUp Logical dictating if the working directory should be deleted on successful job completion
#' @param addargs String specifying additional arguments to be passed to the target software.
#' @return An object of class NMRun.
#' @docType methods
#' @rdname estimate-methods
#' @include telClasses.R
setGeneric("estimate", function(x=NULL, target=NULL, subfolder=format(Sys.time(), "%Y%b%d%H%M%S"), collect=TRUE, clearUp=FALSE, addargs="" ) {
  originalDirectory <- getwd()
  outputObject <- list()
  if(target=="NONMEM") {
    outputObject = estimate.NM(modelfile=x, originalDirectory, addargs)
  } else if( target=="PsN") {
    outputObject = estimate.PsN(modelfile=x, originalDirectory, addargs)    
  } else if(target=="BUGS") {
    estimate.BUGS(MOGObject, addargs)    
  } else {
	stop(sprintf('Unrecognized target: %s.', target))
  }
  
  submitResponse = outputObject$ret
  
  if (submitResponse[[1]]==0 && collect==TRUE) {
    outputObject <- TEL.poll(outputObject) 
    
    target=paste(originalDirectory, subfolder, sep="/")
    
    outputObject$resultsDir = target
    
    if (outputObject$status == "COMPLETED") {
    
      outputObject <- TEL.import(outputObject, target=target, clearUp=clearUp)

      # Create file names:
      ctlFile <- gsub("[.][mM][dD][lL]", ".ctl", x)
      lstFile <- "output.lst"
      
      # Paste in file location:
      ctlFile <- file.path(subfolder, ctlFile)
      lstFile <- file.path(subfolder, lstFile)
      
      # Import data using RMNImport:
      res <- importNm(conFile = ctlFile, reportFile = lstFile)
      
      # Successful execution -> return the imported NONMEM data
      return(res)      
    }
  }

  # Fail with an appropriate error message
  stop(paste(c("Execution of model ", outputObject$modelFile, " failed.\n The contents of the working directory ", outputObject$workingDirectory, " may be useful for tracking down the cause of the failure."), sep=""))

})

#' @rdname estimate-methods
#' @aliases estimate,mogObj,mogObj-method
setMethod("estimate", signature=signature(x="mogObj"), 
  function(x=NULL, target=NULL, subfolder=format(Sys.time(), "%Y%b%d%H%M%S"), collect=TRUE, clearUp=FALSE, addargs="" ) {
    print("mogMethod")
    # First write out MOG to MDL:
    writeMog(x, file="output.mdl")
    
    # Now call the generic method using the mdl file:
    res <- estimate(x="output.mdl", target=target, subfolder=subfolder, collect=collect, clearUp=clearUp, addargs=addargs)
    
    return(res)

  })


estimate.NM<-function(modelfile=NULL, originalDirectory=getwd(), addargs="",...){
  
  workingDirectory <- TEL.prepareWorkingFolder(modelfile, src=originalDirectory)

  oo <- submit.job("execute", workingDirectory, modelfile)
  
  oo$sourceDirectory <- originalDirectory
  
  oo
}

### ----execute.PsN-----------------------------------------------------------
estimate.PsN<-function(modelfile=NULL, originalDirectory=getwd(), addargs="",...){
  workingDirectory <- TEL.prepareWorkingFolder(modelfile, src=originalDirectory)
	
  oo <- submit.job("psn.execute", workingDirectory, modelfile)
  
  oo$sourceDirectory <- originalDirectory
  
  oo
}

estimate.BUGS<-function(modelfile=NULL,addargs="",...){
  cat("Not supported")
}

#estimate(MOGObject="warf_PK_CONC.mdl", target="BUGS", addargs="...")
#estimate(MOGObject="tumour_size.mdl", target="NONMEM")
