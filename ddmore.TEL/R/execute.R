#' Estimate  
#'
#' Passes a MDL file or MOG object (class mogObj) to the target software for execution.
#'
#' @author Jonathan Chard
#' @param x An object of class mogObj or an MDL file.
#' @param target String specifying the target software. Currently, possible 
#'        targets are "NONMEM", "PsN", "monolix" and "BUGS".
#' @param subfolder Specify the name of a subfolder within the current working 
#'        directory in which to store the results, defaults to a timestamped folder
#' @param collect Logical dictating if the results should be collected.
#' @param clearUp Logical dictating if the working directory should be deleted 
#'        on successful job completion
#' @param HOST hostname of the server running the FIS service, defaults to localhost
#' @param PORT port of the server running the FIS service, defaults to 9010
#' @param addargs String specifying additional arguments to be passed to the target software.
#' @return An object of class NMRun.
#' @export
#' @docType methods
#' @rdname estimate-methods
#' @include telClasses.R
setGeneric("estimate", function(x, target=NULL,
	subfolder=format(Sys.time(), "%Y%b%d%H%M%S"), wait=TRUE, collect=TRUE, clearUp=FALSE, HOST='localhost', PORT='9010', addargs="") {
  
  workingDirectory <- TEL.prepareWorkingFolder(modelfile=x)

  submission <- TEL.submitJob(executionType=target, workingDirectory=workingDirectory, 
      modelfile=x, HOST=HOST, PORT=PORT, addargs=addargs)
  
  if (submission$status == "NEW") { # Successfully submitted
	  
	if (wait) {

	    submission <- TEL.poll(submission, HOST=HOST, PORT=PORT)
	    
	    if (submission$status == "COMPLETED") {
			
			if (collect) {
	    
				submission <- TEL.importFiles(submission, target=file.path(submission$sourceDirectory, subfolder), clearUp=clearUp)
		
				# Create file names:
				ctlFile <- paste0(file_path_sans_ext(submission$modelFile), ".ctl")
				lstFile <- paste0(file_path_sans_ext(submission$modelFile), ".lst")
		      
				# Paste in file location:
				ctlFile <- file.path(submission$resultsDir, ctlFile)
				lstFile <- file.path(submission$resultsDir, lstFile)
		      
				# Successful execution, and doing a NONMEM run -> return the RNMImport-imported NONMEM data
				# TODO: Remove this, always use TEL.importSO and ditch RNMImport
				if (file.exists(ctlFile)) {
					importNm(conFile = ctlFile, reportFile = lstFile)
				} else {
					# Create and return the Standard Output object
					TEL.importSO(submission)
				}
				
			} else { # Not collecting the results
				submission
			}
			
		} else { # submission$status != "COMPLETED"

			stop(paste(c("Execution of model ", submission$modelFile, 
        	" failed.\n  The contents of the working directory ",
        	submission$workingDirectory, 
        	" may be useful for tracking down the cause of the failure."), sep=""))
		}
		
	} else { # Don't wait for the job to complete
		submission
	}
	
  } else {
	stop("Submission of execution request was unsuccessful.")
  }

})


#' @rdname estimate-methods
#' @aliases estimate,mogObj,mogObj-method
setMethod("estimate", signature=signature(x="mogObj"), 
  function(x, target=NULL, subfolder=format(Sys.time(), "%Y%b%d%H%M%S"), 
    collect=TRUE, clearUp=FALSE, HOST='localhost', PORT='9010', addargs="") {
    print("mogMethod")
    # First write out MOG to MDL:
    # TODO: This will write out to the current directory - probably not what is desired!
    write(x, f="output.mdl")
    
    # Now call the generic method using the mdl file:
    res <- estimate(x="output.mdl", target=target, subfolder=subfolder, 
      collect=collect, clearUp=clearUp, HOST=HOST, PORT=PORT, addargs=addargs)
    
    return(res)

  })



