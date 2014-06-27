#' Estimate  
#'
#' Passes a MDL file to the target software for execution.
#'
#' @author Jonathan Chard
#' @param file An object of class mogObj.
#' @param target S string specifying the target software. Currently, possible targets are "NONMEM", "PsN" and "BUGS".
#' @param subfolder Specify the name of a subfolder within the current working directory in which to store the results.
#' @param collect Logical dictating if the results should be collected.
#' @param addargs String specifying additional arguments to be passed to the target software.
estimate <- function(file=NULL, target=NULL, subfolder=format(Sys.time(), "%Y%b%d%H%M%S"), collect=TRUE, addargs="" ) {
  originalDirectory <- getwd()
  outputObject <- list()
  if(target=="NONMEM") {
    outputObject = estimate.NM(modelfile=file, originalDirectory, addargs)
  } else if( target=="PsN") {
    outputObject = estimate.PsN(modelfile=file, originalDirectory, addargs)    
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
    
      outputObject <- TEL.import(outputObject, target=target)

      # Create file names:
      ctlFile <- gsub("[.][mM][dD][lL]", ".ctl", file)
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

  # Fallback to returning the output object  
  outputObject
}

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
