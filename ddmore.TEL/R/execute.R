################################################################################
#' Estimate
#'
#' Submits a MDL file or MOG object (class \linkS4class{mogObj}) to the target
#' software for execution and processes the results.
#'
#' @param x An object of class \linkS4class{mogObj} or an MDL file.
#' @param target String specifying the target software. Currently, possible 
#'        targets are "NONMEM", "PsN", "MONOLIX".
#' @param subfolder (Optional) Specify the name of a subfolder, within the directory
#'        containing the model file, in which to store the results. Default
#'        is a timestamped folder.
#' @param wait (Optional) Logical dictating if the function should continuously
#'        poll for results until the job either finishes successfully or fails,
#'        or whether to 'fire and forget' the submission request and manually
#'        collect the results later on. Default is true.
#' @param collect (Optional) Logical dictating if the results should be collected.
#'        Default is true.
#' @param clearUp (Optional) Logical dictating if the job working directory should
#'        be deleted upon successful job completion. Default is false, since this
#'        directory may contain useful information in the event that a job failed
#'        to execute successfully.
#' @param HOST (Optional) Hostname of the server running the FIS service. Default
#'        is localhost.
#' @param PORT (Optional) Port of the server running the FIS service. Default is 9010.
#' @param addargs (Optional) String specifying additional arguments to be passed to the
#'        target software.
#' @return The results from executing the MDL file, in the form of an object of
#'         class \linkS4class{StandardOutputObject}.
#' 
#' @author Jonathan Chard, Matthew Wise
#' 
#' @export
#' @docType methods
#' @rdname estimate-methods
#' 
#' @include server.R
#' @include prepare.R
#' @include import.R
#' @include telClasses.R
#' @include StandardOutputObject.R
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


