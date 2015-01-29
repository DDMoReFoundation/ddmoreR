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
  
	if (is.null(target)) {
		stop('Parameter \"target\" not specified. Possible target tool specifiers include \"NONMEM\", \"PsN\", \"MONOLIX\".');
	}

  workingDirectory <- TEL.prepareWorkingFolder(modelfile=x)

  submission <- TEL.submitJob(executionType=target, workingDirectory=workingDirectory, 
      modelfile=x, HOST=HOST, PORT=PORT, addargs=addargs)
  
  if (submission$status == "NEW") { # Successfully submitted
	  
	if (wait) {

	    submission <- TEL.poll(submission, HOST=HOST, PORT=PORT)
	    
	    if (submission$status == "COMPLETED") {
			
			if (collect) {
	    
				submission <- TEL.importFiles(submission, target=file.path(submission$sourceDirectory, subfolder), clearUp=clearUp)
		
				# Create and return the Standard Output object
				TEL.importSO(submission)
				
			} else { # Not collecting the results
				submission
			}
			
		} else { # submission$status != "COMPLETED"
			stop("Execution of model ", submission$modelFile, " failed.\n  The contents of the working directory ",
				submission$workingDirectory, " may be useful for tracking down the cause of the failure.")
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
    # Should it maybe use MOG object's name?
    write(x, f="output.mdl")
    
    # Now call the generic method using the mdl file:
    res <- estimate(x="output.mdl", target=target, subfolder=subfolder, 
      collect=collect, clearUp=clearUp, HOST=HOST, PORT=PORT, addargs=addargs)
    
    return(res)

  })

################################################################################
#' Resolve Results Directory
#'
#' Resolves a directory where the results of the execution would be placed if the MDL file was executed.
#' 
#' @return absolute path to the directory holding result files from given MDL file exectution
#'
#' @param x a model file path
#' @param subFolderName name of the directory holding the result files 
setGeneric(".resolveResultsDirectory", function(x, subFolderName=NULL) {
        if(is.null(x)) {
            stop("Tried to resolve results directory for null model file.");
        }
        if(is.null(subFolderName)) {
            stop("Tried to resolve results directory for null sub folder name.");
        }
        if(!file.exists(x)) {
            stop("Model file does not exist");
        }
        
        return(file.path(parent.folder(x), subFolderName))
    })


#' @param x a model file path
#' @param subFolderName name of the directory holding the result files 
setMethod(".resolveResultsDirectory", signature=signature(x="mogObj"), function(x, subFolderName=NULL) {
        # TODO: based on the 'estimate' method implementation
        return(file.path(getwd(), subFolderName))
    })
