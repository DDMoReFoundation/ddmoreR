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
#' @param wait (Optional) Logical dictating if the function should continuously
#'        poll for results until the job either finishes successfully or fails,
#'        or whether to 'fire and forget' the submission request and manually
#'        collect the results later on. Default is true.
#' @param clearUp (Optional) Logical dictating if the job working directory should
#'        be deleted upon successful job completion. Default is false, since this
#'        directory may contain useful information in the event that a job failed
#'        to execute successfully.
#' @param HOST (Optional) Hostname of the server running the FIS service. Default
#'        is localhost.
#' @param PORT (Optional) Port of the server running the FIS service. Default is 9010.
#' @return The results from executing the MDL file, in the form of an object of
#'         class \linkS4class{StandardOutputObject}.
#' 
#' @author Jonathan Chard, Matthew Wise
#' 
#' @seealso \code{TEL.prepareWorkingFolder}
#' @seealso \code{TEL.submitJob}
#' @seealso \code{TEL.poll}
#' @seealso \code{TEL.importFiles}
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
	addargs=NULL, subfolder=format(Sys.time(), "%Y%b%d%H%M%S"), wait=TRUE, clearUp=FALSE,
	HOST='localhost', PORT='9010') {
  
	execute(x=x, target=target,
			addargs=addargs, subfolder=subfolder, wait=wait, clearUp=clearUp,
			HOST=HOST, PORT=PORT)
})

#' @rdname estimate-methods
#' @aliases estimate,mogObj,mogObj-method
setMethod("estimate", signature=signature(x="mogObj"), 
	function(x, target=NULL,
			 addargs=NULL, subfolder=format(Sys.time(), "%Y%b%d%H%M%S"), wait=TRUE, clearUp=FALSE,
			 HOST='localhost', PORT='9010') {

    # First write out MOG to MDL.
    # TODO: This will write out to the current directory - probably not what is desired!
    # Should it maybe use MOG object's name?
    write(x, f="output.mdl")
    
    # Now call the generic method using the mdl file
	execute(x="output.mdl", target=target,
			addargs=addargs, subfolder=subfolder, wait=wait, clearUp=clearUp,
			HOST=HOST, PORT=PORT)
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
#' @param wait (Optional) Logical dictating if the function should continuously
#'        poll for results until the job either finishes successfully or fails,
#'        or whether to 'fire and forget' the submission request and manually
#'        collect the results later on. Default is true.
#' @param clearUp (Optional) Logical dictating if the job working directory should
#'        be deleted upon successful job completion. Default is false, since this
#'        directory may contain useful information in the event that a job failed
#'        to execute successfully.
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
#' @param importSO (Optional) Whether to create and return a Standard Output
#'        Object representing the results of the execution. Mutually exclusive
#'        with the \code{importMultipleSO} parameter. Default is true.
#' @param importMultipleSO (Optional) Whether to create and return a list of
#'        Standard Output Objects representing the results of the execution.
#'        Mutually exclusive with the \code{importSO} parameter (which would be
#'        used for the specific case where it is known that only one SOBlock will
#'        be present in the Standard Output results). Default is false.
#' @param HOST (Optional) Hostname of the server running the FIS service. Default
#'        is localhost.
#' @param PORT (Optional) Port of the server running the FIS service. Default is 9010.
#' @return The results from executing the MDL file, in the form of an object of
#'         class \linkS4class{StandardOutputObject}, or a list thereof, depending
#'         on the parameters \code{importSO} and  \code{importMultipleSO}
#' 
#' @author Jonathan Chard, Matthew Wise
#' 
#' @seealso \code{TEL.prepareWorkingFolder}
#' @seealso \code{TEL.submitJob}
#' @seealso \code{TEL.poll}
#' @seealso \code{TEL.importFiles}
#' 
#' @include server.R
#' @include prepare.R
#' @include import.R
#' @include telClasses.R
#' @include StandardOutputObject.R
setGeneric("execute", function(x, target = NULL,
                               addargs = NULL, subfolder = format(Sys.time(), "%Y%b%d%H%M%S"), wait =
                                   TRUE, clearUp = FALSE,
                               extraInputFileExts = NULL, extraInputFiles = NULL, importSO = TRUE, importMultipleSO =
                                   FALSE,
                               HOST = 'localhost', PORT = '9010', ...) {
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
    
    # FIXME: This is to enable mocking of functions responsible for integration with FIS REST API.
    # To be removed after 'testthat' upgrade and introducing global FIS Server instance
    inargs <- list(...)
    if (!is.null(inargs) && !is.null(inargs$server)) {
        SERVER = inargs$server
    } else {
        SERVER = .SERVER
    }
    if (!is.null(inargs) && !is.null(inargs$tel)) {
        TEL = inargs$tel
    } else {
        TEL = .TEL
    }
    
    # Create a working folder in which FIS will create the Archive for conversion and execution
    workingDirectory <- tempfile("TEL.job",tempdir())
    if (!file.exists(workingDirectory)) {
        dir.create(workingDirectory)
    }
    
    submission <-
        SERVER$submitJob(
            executionType = target, workingDirectory = workingDirectory,
            modelfile = absoluteModelFilePath, extraInputFileExts = extraInputFileExts, extraInputFiles =
                extraInputFiles,
            addargs = addargs, HOST = HOST, PORT = PORT
        )
    
    result <- NULL
    if (submission$status == "Submitted") {
        if (wait) {
            result <-
                TEL.monitor(
                    submission, importDirectory = file.path(submission$parameters$sourceDirectory, subfolder), clearUp, importSO, importMultipleSO, HOST, PORT
                )
        } else {
            result <- submission
        }
    } else {
        stop("Submission of execution request was unsuccessful.")
    }
    
    result
})

################################################################################
#' Monitor
#' 
#' Function performing monitoring and import of FIS job results. 
#' 
#' Note: This method will be refactored in the future to resolve SRP violation 
#' and also so the import of the results happens even if a job fails 
#' (SO may still be available in such case)
#'
#' @param submission a list representing a job that was submitted to FIS.
#' For other params see execute function.
#'
#' @param importDirectory a directory where the result files should be imported into.
#' 
#' For the rest of the parameters see \code{execute} function
#'
#' @seealso \code{execute}
#' 
#' @export
#' 
TEL.monitor <-
    function(submission = NULL, importDirectory = NULL, clearUp = FALSE, importSO =
                 TRUE, importMultipleSO = FALSE,
             HOST = 'localhost', PORT = '9010', ...) {
        if (is.null(submission)) {
            stop('Illegal Argument: submission object was null.')
        }
        if (is.null(importDirectory)) {
            stop('Illegal Argument: target directory was null.')
        }
        if (!("fisJob" %in% names(submission)) ||
            is.null(submission$fisJob)) {
            stop("Illegal Argument: submission's fisJob element must be set and can't be NULL.")
        }
        
        # FIXME: This is to enable mocking of functions responsible for integration with FIS REST API.
        # To be removed after 'testthat' upgrade and introducing global FIS Server instance
        inargs <- list(...)
        if (!is.null(inargs) && !is.null(inargs$server)) {
            SERVER = inargs$server
        } else {
            SERVER = .SERVER
        }
        if (!is.null(inargs) && !is.null(inargs$tel)) {
            TEL = inargs$tel
        } else {
            TEL = .TEL
        }
        
        message(sprintf('-- %s', submission$start))
        message(sprintf('Job %s progress:', submission$fisJob$id))
        message(submission$status)
        
        submission <- SERVER$poll(submission, HOST = HOST, PORT = PORT)
        result <- NULL
        if (submission$fisJob$status == "COMPLETED") {
            submission$status = "Importing Results"
            message(submission$status)
            submission <-
                TEL$importFiles(submission, target = importDirectory, clearUp = clearUp)
            if (importMultipleSO) {
                result <- TEL$importSO(submission, multiple = TRUE)
            } else if (importSO) {
                result <- TEL$importSO(submission)
            }
            submission$status <- "Completed"
        } else {
            submission$status <- "Failed"
        }
        
        message(submission$status)
        submission$end <- date()
        message(sprintf('-- %s', submission$end))
        
        if (submission$status == "Failed") {
            stop(
                "Execution of model ", submission$parameters$modelFile, " failed.\n  The contents of the working directory ",
                submission$parameters$workingDirectory, " may be useful for tracking down the cause of the failure."
            )
        }
        
        if (is.null(result)) {
            submission
        } else {
            result
        }
    }


#' @seealso \code{execute}
setMethod("execute", signature=signature(x="mogObj"), 
    function(x, target=NULL,
                    addargs=NULL, subfolder=format(Sys.time(), "%Y%b%d%H%M%S"), wait=TRUE, clearUp=FALSE,
                    extraInputFileExts=NULL, extraInputFiles=NULL, importSO=TRUE, importMultipleSO=FALSE,
                    HOST='localhost', PORT='9010') {

    # First write out MOG to MDL.
    # TODO: This will write out to the current directory - probably not what is desired!
    # Should it maybe use MOG object's name?
    write(x, f="output.mdl")
    
    # Now call the generic method using the mdl file
    execute(x="output.mdl", target=target,
            addargs=addargs, subfolder=subfolder, wait=wait, clearUp=clearUp,
            extraInputFileExts=extraInputFileExts,extraInputFiles=extraInputFiles,
            importSO=importSO,importMultipleSO=importMultipleSO,
            HOST=HOST, PORT=PORT)
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
