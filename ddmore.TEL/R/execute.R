#' Estimate  
#'
#' Passes a MDL file or MOG object (class mogObj) to the target software for execution.
#'
#' @author Jonathan Chard
#' @param x An object of class mogObj or an MDL file.
#' @param target String specifying the target software. Currently, possible targets are "NONMEM", "PsN" and "BUGS".
#' @param subfolder Specify the name of a subfolder within the current working directory in which to store the results,
#'                  defaults to a timestamped folder
#' @param collect Logical dictating if the results should be collected.
#' @param clearUp Logical dictating if the working directory should be deleted on successful job completion
#' @param HOST hostname of the server running the FIS service, defaults to localhost
#' @param PORT port of the server running the FIS service, defaults to 9010
#' @param addargs String specifying additional arguments to be passed to the target software.
#' @return An object of class NMRun.
#' @export
#' @docType methods
#' @rdname estimate-methods
#' @include telClasses.R
setGeneric("estimate", function(x, target=NULL, subfolder=format(Sys.time(), "%Y%b%d%H%M%S"), collect=TRUE, clearUp=FALSE, HOST='localhost', PORT='9010', addargs="") {

  outputObject <- list() # Empty list
  
  if (target=="NONMEM") {
    outputObject = estimate.NM(modelfile=x, HOST=HOST, PORT=PORT, addargs=addargs)
  } else if (target=="PsN") {
    outputObject = estimate.PsN(modelfile=x, HOST=HOST, PORT=PORT, addargs=addargs)
  } else if (target=="BUGS") {
    # TODO: Implement this
    estimate.BUGS(MOGObject, HOST=HOST, PORT=PORT, addargs=addargs)
  } else {
	stop(sprintf('Unrecognised target: %s.', target))
  }
  
  submitResponse = outputObject$ret
  
  if (submitResponse[[1]] == 0) {

	if (collect) {

	    outputObject <- TEL.poll(outputObject, HOST=HOST, PORT=PORT) 
	    
	    outputObject$resultsDir = file.path(outputObject$sourceDirectory, subfolder)
	    
	    if (outputObject$status == "COMPLETED") {
	    
	      outputObject <- TEL.import(outputObject, target=outputObject$resultsDir, clearUp=clearUp)
	
	      # Create file names:
	      ctlFile <- gsub("[.][mM][dD][lL]", ".ctl", outputObject$modelFile)
	      lstFile <- "output.lst"
	      
	      # Paste in file location:
	      ctlFile <- file.path(outputObject$resultsDir, ctlFile)
	      lstFile <- file.path(outputObject$resultsDir, lstFile)
	      
	      # Import data using RMNImport:
	      res <- importNm(conFile = ctlFile, reportFile = lstFile)
	      
	      # Successful execution -> return the imported NONMEM data
	      return(res)
		}
	}
	
  } else {
	# Response was not successul -> Fail with an appropriate error message
	stop(paste(c("Execution of model ", outputObject$modelFile, " failed.\n The contents of the working directory ", outputObject$workingDirectory, " may be useful for tracking down the cause of the failure."), sep=""))
  }

})

#' @rdname estimate-methods
#' @aliases estimate,mogObj,mogObj-method
setMethod("estimate", signature=signature(x="mogObj"), 
  function(x, target=NULL, subfolder=format(Sys.time(), "%Y%b%d%H%M%S"), collect=TRUE, clearUp=FALSE, HOST='localhost', PORT='9010', addargs="") {
    print("mogMethod")
    # First write out MOG to MDL:
    # TODO: This will write out to the current directory - probably not what is desired!
    write(x, f="output.mdl")
    
    # Now call the generic method using the mdl file:
    res <- estimate(x="output.mdl", target=target, subfolder=subfolder, collect=collect, clearUp=clearUp, HOST=HOST, PORT=PORT, addargs=addargs)
    
    return(res)

  })

estimate.NM <- function(modelfile, HOST='localhost', PORT='9010', addargs="", ...) {
  
  workingDirectory <- TEL.prepareWorkingFolder(modelfile)

  outputObject <- submit.job("execute", workingDirectory, modelfile, HOST, PORT)
}

estimate.BUGS<-function(modelfile, HOST='localhost', PORT='9010', addargs="", ...) {
  stop("estimate.BUGS is currently not supported")
}

#estimate(MOGObject="warf_PK_CONC.mdl", target="BUGS", addargs="...")
#estimate(MOGObject="tumour_size.mdl", target="NONMEM")

# Internal generic function to be called by all PsN variants to make the call to the framework
.execute.PsN.command <- function(modelfile, HOST='localhost', PORT='9010', command, args="", ...) {
	modelfileWithoutPath <- tail(strsplit(file_path_as_absolute(modelfile), '/')[[1]], n=1) # Strip off leading path
	
	fullCommand <- paste(command, shQuote(modelfileWithoutPath), "-directory=\"PsN_Execute\"", args)
	cat(paste(paste("PsN command is:", fullCommand), "\n"))
	
	workingDirectory <- TEL.prepareWorkingFolder(modelfile)
	
	outputObject <- submit.job("cmdline.execute", workingDirectory, modelfile, HOST, PORT, addargs=fullCommand)
}

#' execute.PsN
#' Estimates parameters using PsN Execute.
#' @param modelfile
#' @param HOST
#' @param PORT
#' @param command
#' @param addargs
#' @param ...
#' @export
execute.PsN <- function(modelfile, HOST='localhost', PORT='9010', command="execute-3.6.2.bat", addargs="", ...) {
	outputObject <- .execute.PsN.command(modelfile, HOST, PORT, command, addargs)
}

#' VPC.PsN
#' Performs a VPC (Visual Predictive Check) of a given model using PsN.
#' @param modelfile
#' @param HOST
#' @param PORT
#' @param command
#' @param nsamp
#' @param seed
#' @param addargs
#' @param cleanup
#' @param ...
#' @export
VPC.PsN <- function(modelfile, HOST='localhost', PORT='9010', command="vpc-3.6.2.bat", nsamp, seed, addargs="", cleanup=T, ...) {
	args <- paste0("--samples=", nsamp, " --seed=", seed, " ", addargs)
	outputObject <- .execute.PsN.command(modelfile, HOST, PORT, command, args)
}

#' bootstrap.PsN
#' Performs bootstrapping to generate new datasets from the dataset of a given model.
#' @param modelfile
#' @param HOST
#' @param PORT
#' @param command
#' @param nsamp
#' @param seed
#' @param addargs
#' @param cleanup
#' @param ...
#' @export
bootstrap.PsN <- function(modelfile, HOST='localhost', PORT='9010', command="bootstrap-3.6.2.bat", nsamp, seed, addargs="", cleanup=T, ...) {
	args <- paste0("--samples=", nsamp, " --seed=", seed, " ", addargs)
	outputObject <- .execute.PsN.command(modelfile, HOST, PORT, command, args)
}

#' SSE.PsN
#' Performs SSE (Stochastic Simulation and Estimation) on a given model.
#' @param modelfile
#' @param HOST
#' @param PORT
#' @param command
#' @param nsamp
#' @param seed
#' @param addargs
#' @param cleanup
#' @param ...
#' @export
SSE.PsN <- function(modelfile, HOST='localhost', PORT='9010', command="sse-3.6.2.bat", nsamp, seed, addargs="", cleanup=T, ...) {
	args <- paste0("--samples=", nsamp, " --seed=", seed, " ", addargs)
	outputObject <- .execute.PsN.command(modelfile, HOST, PORT, command, args)
}

