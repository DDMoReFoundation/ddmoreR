################################################################################
#' Estimate
#'
#' Submits a MDL file or MOG object (class \linkS4class{mogObj}) to the target
#' software for execution and processes the results.
#'
#' @param x An MDL file, or an object of class \linkS4class{mogObj}.
#' @param target String specifying the target software. Currently, possible 
#'        targets are "NONMEM", "PsN", "MONOLIX".
#' @param addargs (Optional) String specifying additional arguments to be passed
#'        to the target software.
#' @param subfolder (Optional) Specify the name of a subfolder, within the directory
#'        containing the model file, in which to store the results. Default
#'        is a timestamped folder.
#' @param clearUp (Optional) Logical dictating if the job working directory should
#'        be deleted upon successful job completion. Default is false, since this
#'        directory may contain useful information in the event that a job failed
#'        to execute successfully.
#' @param fisServer FISServer instance.
#' @return The results from executing the MDL file, in the form of an object of
#'         class \linkS4class{StandardOutputObject}.
#' 
#' @author Jonathan Chard, Matthew Wise
#' 
#' @seealso \code{DDMORE.submitJob}
#' @seealso \code{DDMORE.poll}
#' @seealso \code{DDMORE.importFiles}
#' 
#' @export
#' @docType methods
#' @rdname estimate-methods
#' 
#' @include server.R
#' @include import.R
#' @include Classes.R
#' @include StandardOutputObject.R
setGeneric("estimate", function(x, target=NULL,
	addargs=NULL, subfolder=format(Sys.time(), "%Y%b%d%H%M%S"), clearUp=FALSE,
	fisServer=DDMORE.getServer()) {
  
	execute(x=x, target=target,
			addargs=addargs, subfolder=subfolder, clearUp=clearUp,
			fisServer=fisServer)
})

#' @rdname estimate-methods
#' @aliases estimate,mogObj,mogObj-method
setMethod("estimate", signature=signature(x="mogObj"), 
	function(x, target=NULL,
			 addargs=NULL, subfolder=format(Sys.time(), "%Y%b%d%H%M%S"), clearUp=FALSE,
			 fisServer=DDMORE.getServer()) {

    # First write out MOG to MDL.
    # TODO: This will write out to the current directory - probably not what is desired!
    # Should it maybe use MOG object's name?
    write(x, f="output.mdl")
    
    # Now call the generic method using the mdl file
	execute(x="output.mdl", target=target,
			addargs=addargs, subfolder=subfolder, clearUp=clearUp,
			fisServer=fisServer)
  })
  

################################################################################
#' INTERNAL Common execute function called by estimate() and the psnWrappers functions
#'
#' Submits a MDL file to the target software for execution and processes the results.
#'
#' @param x An MDL file to execute.
#' @param target String specifying the target software. Currently, possible 
#'        targets are "NONMEM", "PsN", "MONOLIX".
#' @param addargs (Optional) String specifying additional arguments to be passed
#'        to the target software.
#' @param subfolder (Optional) Specify the name of a subfolder, within the directory
#'        containing the model file, in which to store the results. Default
#'        is a timestamped folder.
#' @param extraInputFileExts (Optional) A vector of file extensions (excluding the
#'        dot) that will be used in identifying additional files, from the same
#'        directory as the model file and having the same base name as the model
#'        file, to be included in the execution. Default is null/empty.
#'        Primarily used by PsN e.g. to provide the .lst file for a PsN execution of
#'        an already-executed NONMEM run.
#' @param extraInputFiles (Optional) A vector of paths, either absolute or relative
#'        (to the model file), to any additional files to be included in the execution.
#'        Used as an alternative, and/or in conjunction with, extraInputFileExts.
#'        Default is null/empty.
#' @param clearUp (Optional Advanced) Logical dictating if the job working directory should
#'        be deleted upon successful job completion. Default is false, since this
#'        directory may contain useful information in the event that a job failed
#'        to execute successfully.
#' @param importSO (Optional Advanced) Whether to create and return a Standard Output
#'        Object representing the results of the execution. Mutually exclusive
#'        with the \code{importMultipleSO} parameter. Default is true.
#' @param importMultipleSO (Optional Advanced) Whether to create and return a list of
#'        Standard Output Objects representing the results of the execution.
#'        Mutually exclusive with the \code{importSO} parameter (which would be
#'        used for the specific case where it is known that only one SOBlock will
#'        be present in the Standard Output results). Default is false.
#' @param fisServer FISServer instance.
#' @return The results from executing the MDL file, in the form of an object of
#'         class \linkS4class{StandardOutputObject}, or a list thereof, depending
#'         on the parameters \code{importSO} and  \code{importMultipleSO}
#' @author Jonathan Chard, Matthew Wise
#' 
#' @seealso \code{DDMORE.prepareSubmissionStep}
#' @seealso \code{DDMORE.submitJobStep}
#' @seealso \code{DDMORE.pollStep}
#' @seealso \code{DDMORE.importFilesStep}
#' @seealso \code{DDMORE.importSOStep}
#' 
#' @include server.R
#' @include FISServer.R
#' @include jobExecution.R
#' @include import.R
#' @include Classes.R
#' @include StandardOutputObject.R
setGeneric("execute", function(x, target = NULL,
                               addargs = NULL, subfolder = format(Sys.time(), "%Y%b%d%H%M%S"),
                               extraInputFileExts = NULL, extraInputFiles = NULL,
                               fisServer = DDMORE.getServer(), ...) {
    .precondition.checkArgument(is.FISServer(fisServer), "fisServer", "FIS Server instance is required.")
    if (is.null(target)) {
        stop(
            'Parameter \"target\" not specified. Possible target tool specifiers might include \"NONMEM\", \"PsN\", \"MONOLIX\".'
        );
    }
    if (is.null(x)) {
        stop("Illegal Argument: x must be set and can't be NULL.")
    }
    absoluteModelFilePath <- normalizePath(x)
    if (!file.exists(absoluteModelFilePath)) {
        stop(paste('Illegal Argument: file ', x, ' does not exist.'))
    }
    
    inargs <- list(...)
    submission <- DDMORE.prepareSubmissionStep(
            executionType = target, workingDirectory = tempfile("DDMORE.job",tempdir()),
            modelfile = absoluteModelFilePath, extraInputFileExts = extraInputFileExts, 
            extraInputFiles = extraInputFiles, outputSubFolderName = subfolder,
            commandParameters = addargs, fisServer = fisServer, 
            extraParams = inargs
        )
    submission <- DDMORE.performExecutionWorkflow(submission, fisServer = fisServer, workflowSteps = .buildWorkflow(params = inargs))
    return(submission$so)
})

################################################################################
#' .buildWorkflow
#' Builds execution workflow based on a named list of parameters.
#'
#' @param clearUp see \code{execute} function
#' @param importSO see \code{execute} function
#'
#' @return a list of steps for execution
.buildWorkflow <- function( params ) {
    workflow <- list()
    workflow <- c(workflow,DDMORE.submitJobStep)
    workflow <- c(workflow,DDMORE.pollStep)
    workflow <- c(workflow,DDMORE.importFilesStep)
    if(!is.null(params$clearUp) && params$clearUp) {
        workflow <- c(workflow,DDMORE.clearUpStep)
    }
    if(is.null(params$importSO) || params$importSO) {
        workflow <- c(workflow,DDMORE.importSOStep)
        workflow <- c(workflow,DDMORE.verifySOStep)
    }
    workflow
}
################################################################################
#' DDMORE.performSyncExecutionWorkflow
#' 
#' Function performing a workflow involved in execution of a Job in FIS. 
#' 
#' If execution fails, the submission object together with the error (if any) that provoked the failure is available
#' as '.DdmoreFailedSubmission' global variable.
#' 
#' @param submission a list representing a job that holds all parameters required for successful submission of a job.
#' @param fisServer FISServer instance.
#' @param workflowSteps a list of workflow steps to be executed during execution.
#' 
#'
#' @seealso \code{DDMORE.prepareSubmissionStep}
#' 
#' @include jobExecution.R
#' 
#' @export
#' 
DDMORE.performExecutionWorkflow <-
    function(submission = NULL,
             fisServer = DDMORE.getServer(), workflowSteps = list(), ...) {
        .precondition.checkArgument(is.FISServer(fisServer), "fisServer", "FIS Server instance is required.")
        .precondition.checkArgument(!is.null(submission), "submission", "Object is required.")
        .precondition.checkArgument(("parameters" %in% names(submission)) && !is.null(submission$parameters),"submission$parameters", "Element must be set and can't be NULL.")
        
        message(sprintf('-- %s', submission$start))
        submission <- tryCatch(
            {
                message(submission$status)
                for(stepFunction in workflowSteps) {
                    submission <- do.call(what = stepFunction, 
                                               args = list("submission" = submission, "fisServer" = fisServer, "..." = ...)
                                               )
                    if(submission$status == SUBMISSION_CANCELLED) {
                        break
                    }
                }
                submission
            }, error = function(err) {
                submission$status <- SUBMISSION_FAILED
                submission$error <- err
                log.debug(err)
                submission
            }
        )
        submission$end <- date()
        submission <- .setFinalSubmissionStatus(submission)
        message(SUBMISSION_STATUSES[[submission$status]]$label)
        message(sprintf('-- %s', submission$end))
        if (submission$status == SUBMISSION_FAILED) {
            .DdmoreFailedSubmission <<- submission
            stop(
                "Execution of model ", submission$parameters$modelFile, " failed.\n  The contents of the working directory ",
                submission$parameters$importDirectory, " may be useful for tracking down the cause of the failure."
            )
        }
        return(submission)
    }
    
#'
#' Ensures that the submission has correct predefined (one of SUBMISSION_FAILED, SUBMISSION_CANCELLED, SUBMISSION_COMPLETED, SUBMISSION_COMPLETED_WITH_ERRORS) final status set. 
#'
#' The use of the submission$status is optional by the *Step functions, and they are free to to use it if there is a need and set these to free-style text. 
#' 
#'
.setFinalSubmissionStatus <- function(submission) {
    .precondition.checkArgument("status" %in% names(submission) && !is.null(submission$status) , "submission$status", "Is required and should be not-null.")
    if(!submission$status %in% names(SUBMISSION_STATUSES) || !SUBMISSION_STATUSES[[submission$status]]$final) {
        if (!"fisJob" %in% names(submission) || submission$fisJob@status %in% c("FAILED")) {
            submission$status <- SUBMISSION_FAILED
        } else {
            submission$status <- SUBMISSION_COMPLETED
        }
    }
	return(submission)
}
	

#' @seealso \code{execute}
setMethod("execute", signature=signature(x="mogObj"), 
    function(x, target=NULL,
                    addargs=NULL, subfolder=format(Sys.time(), "%Y%b%d%H%M%S"), clearUp=FALSE,
                    extraInputFileExts=NULL, extraInputFiles=NULL, importSO=TRUE, importMultipleSO=FALSE,
                    fisServer = DDMORE.getServer()) {

    # First write out MOG to MDL.
    # TODO: This will write out to the current directory - probably not what is desired!
    # Should it maybe use MOG object's name?
    write(x, f="output.mdl")
    
    # Now call the generic method using the mdl file
    execute(x="output.mdl", target=target,
            addargs=addargs, subfolder=subfolder, clearUp=clearUp,
            extraInputFileExts=extraInputFileExts,extraInputFiles=extraInputFiles,
            importSO=importSO,importMultipleSO=importMultipleSO,
            fisServer = fisServer)
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
