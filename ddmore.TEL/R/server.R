# Package-local environment for storing shared variables
# This will be replaced in the future by Server class and its instance, which will hold 
# all FIS integration properties (ports, urls, polling settings, etc.) and this instance will be passed
# to execute (and other) functions
TEL.server.env <- new.env()

################################################################################
#' TEL.getServer
#'
#' Gets a handle on FIS Server instance
#' 
#' If non exists, it will create an instance using default values.
#' 
#' @return FISServer instance
#'
TEL.getServer <- function() {
    if(!"fisServer" %in% ls(TEL.server.env)) {
        stop("FIS Server instance not configured")
    }
    return(TEL.server.env$fisServer)
}

TEL.setServer <- function(fisServer) {
    .precondition.checkArgument(is.FISServer(fisServer), "fisServer", "FIS Server instance is required.")
    if("fisServer" %in% ls(TEL.server.env)) {
        warning(sprintf("One FIS Server instance is already configured pointing to %s. ", TEL.server.env$fisServer@url))
    }
    assign("fisServer", fisServer, envir = TEL.server.env)
}

TEL.initSEEFISServerInstance <- function(startupScript = file.path(TEL.checkConfiguration(), "startup.bat")) {
    if(!file.exists(startupScript)) {
        stop(paste("Services startup script", startupScript," does not exist."))
    }
    TEL.setServer(createFISServer(startupScript=startupScript))
}

################################################################################
#' TEL.startServer
#'
#' Starts the TEL server, by launching the startup.bat script within the root
#' of the SEE.
#' 
#' This is automatically called as part of launching the TEL.R console within
#' the MDL IDE within SEE; if the R console is not running within SEE in that
#' setup then this function is a no-op.
#' 
#'
TEL.startServer <- 
    function(fisServer) {
    .precondition.checkArgument(is.FISServer(fisServer), "fisServer", "FIS Server instance is required.")
    message("Starting servers [ ", appendLF=FALSE)
    if (!TEL.serverRunning(fisServer)) {
      startupScriptDir <- dirname(fisServer@startupScript)
      startupScriptName <- basename(fisServer@startupScript)
      startupScriptStdErr <- file.path(startupScriptDir, paste0(".",startupScriptName, ".stderr"))
      startupScriptStdOut <- file.path(startupScriptDir, paste0(".",startupScriptName, ".stdout"))
      system2(fisServer@startupScript,'/B', wait=F, stdout=startupScriptStdOut, stderr=startupScriptStdErr)  # /B argument suppresses the display of the command windows
      count = 0
      while ( count < fisServer@startupPollingMax && !TEL.serverRunning(fisServer) ) {
        Sys.sleep(fisServer@startupPollingDelay)
        count <- count+1
        message(".", appendLF=FALSE)
        flush.console()
      }
      message(" ]", appendLF=FALSE)
      message() # Append a newline
      if (!TEL.serverRunning(fisServer)) {
      	healthStatuses <- TEL.serverHealthcheck(fisServer)
        stop(paste("Failure!", healthStatuses, sep="\n"))
      }
      message("Success!")
    } else {
      message(". ]", appendLF=FALSE)
      message() # Append a newline
      message("Success!")
    }
  }

################################################################################
#' TEL.serverRunning
#'
#' Checks if the FIS server, and its dependent servers, are running.
#' As a side effect, populates a TEL.serverHealthDetails global object so that the
#' user can access the health statuses of the individual server components.
#'
#' @param fisServer FISServer instance.
#' 
#' @return True if the server is running, false if the server is not running.
#' 
#' @export
#' 
TEL.serverRunning <- 
  function(fisServer) {

	healthStatuses <- health(fisServer)
	
	# Make the server health details available to the user
	TEL.serverHealthDetails <<- healthStatuses
	
	(healthStatuses[[1]] == 'UP') && # Status of FIS itself
		all(lapply(healthStatuses[-1], function(srvStatus) { srvStatus$status }) == 'UP')
  }
  
################################################################################
#' TEL.stopServer
#'
#' Stops the TEL server.
#' 
#' @param fisServer FISServer instance.
#' @param dontWait (Internal usage only) Defaults to FALSE; only the R console
#' 				   termination should call this with TRUE
#'
TEL.stopServer <-
  function(fisServer, dontWait = FALSE) {
    message("Stopping server...")
	if (shutdown(fisServer)) {
		# The servers might report themselves as not running immediately but they still
		# take a few seconds to actually shut down; put in a pause to avoid the user
		# attempting to restart the servers while they haven't fully shut down
		if (!dontWait) {
			Sys.sleep(10)
		}
		message("Server is now stopped.")
	} else {
		warning("Server could not be stopped.")
	}
  }

################################################################################
#' Quit
#' 
#' Override the default R quit function to try to stop the server before
#' quitting normally.
#' 
#' This is automatically called when the TEL.R console is terminated.
#' 
#' @param fisServer FISServer instance.
#' 
#' @export
#'
q <- function(fisServer=TEL.getServer()) {
	TEL.safeStop(fisServer, dontWait=TRUE);
	base:::q()
}

################################################################################
#' TEL.safeStop
#'
#' Checks that the TEL server is running and if so, stops it.
#' 
#' @param fisServer FISServer instance.
#' @param dontWait (Internal usage only) Defaults to FALSE; only the R console
#' 				   termination should call this with TRUE
#'
TEL.safeStop <-
  function(fisServer, dontWait = FALSE) {
    if (TEL.serverRunning(fisServer)) {
      TEL.stopServer(fisServer, dontWait)
    }
  }

################################################################################
#' TEL.checkConfiguration
#'
#' Checks if this DDMoRe.TEL package is actually loaded (the R scripts may have
#' been \code{source}d instead). If so, then determine the path to the package
#' and navigate up the directory tree from this location to obtain the SEE home
#' directory, returning this directory. Therefore this assumes that the TEL.R
#' console has been launched from within the MDL IDE within SEE; otherwise
#' the function will raise an error.
#'
TEL.checkConfiguration <-
  function() {

	if ("package:DDMoRe.TEL" %in% search()) {
		# The package is loaded, proceed

    	packagePath <- path.package("DDMoRe.TEL")
    
    	see.home <- file_path_as_absolute(file.path(packagePath, '..', '..', '..', '..'))
    
    	fis.home <- file.path(see.home, 'fis')
    	mif.home <- file.path(see.home, 'mif-exec')

    	errMsg <- '';
    	if (!file.exists(fis.home)) {
        	errMsg <- paste(errMsg, 'Derived FIS directory ', fis.home, ' does not exist.\n', sep='')
    	}
    	if (!file.exists(mif.home)) {
        	errMsg <- paste(errMsg, 'Derived MIF directory ', mif.home, ' does not exist.\n', sep='')
    	}
    	if (errMsg != "") {
        	stop(paste(errMsg, 'This is probably because you are not running the TEL console from within an SEE environment.\nThe FIS and MIF servers must be started manually.', sep=""))
    	}
    
    	see.home # Return value
		
	} else {
		stop("The DDMoRe.TEL package is not loaded. The FIS and MIF servers must be started manually.")
	}
    
}

################################################################################
#' TEL.submitJob
#'
#' Submits a job to the TEL server.
#' 
#' @param executionType Identifies the target software to use to execute this job.
#'        E.g. NONMEM, MONOLIX.
#' @param workingDirectory Directory, normally within the system temporary directory,
#'        into which the model file and any data files are expected to have been
#'        copied, and within which the target software will execute the job.
#' @param modelfile Path-less filename, or absolute path, to the model file to be
#'        executed. Note that relative paths are currently not supported.
#' @param extraInputFileExts (Optional) A vector of file extensions (excluding the
#'        dot) that will be used in identifying additional files, from the same
#'        directory as the model file and having the same base name as the model file,
#'        to be included in the execution. Default is null/empty.
#'        Primarily used by PsN e.g. to provide the .lst file for a PsN execution of
#'        an already-executed NONMEM run.
#' @param extraInputFiles (Optional) A vector of paths, either absolute or relative
#'        (to the model file), to any additional files to be included in the execution.
#'        Used as an alternative, and/or in conjunction with, extraInputFileExts.
#'        Default is null/empty.
#' @param addargs (Optional) Additional arguments to be passed to the target software.
#' @param fisServer FISServer instance.
#' 
#' @return \code{submission} named list containing information relating to the
#'         submission of the job:
#'         \itemize{
#'           \item{\code{status}}
#'             - The status of the execution of the model file, i.e. 'NEW'.
#'           \item{\code{fisJob}} - FIS Job entity returned on submission.
#'           \item{\code{start}} - Submission time (generated by R, not FIS)
#'           \item{\code{parameters}}
#'             - A list capturing the input parameters for submission.
#'           \item{\code{parameters$modelFile}}
#'             - An absolute path to the model file.
#'           \item{\code{parameters$sourceDirectory}}
#'             - The absolute path to the directory in which the MDL file lives.
#'           \item{\code{parameters$workingDirectory}}
#'             - The location used for the execution of the job, normally within
#'               the system temporary directory.
#'           \item{\code{parameters$extraInputFiles}}
#'             - List of extra input files for a job.
#'           \item{\code{parameters$extraInputFiles}}
#'             - List of extra input files for a job.
#'           \item{\code{parameters$commandParameters}}
#'             - Additional command line parameters for the third party tool.
#'           \item{\code{parameters$executionFile}}
#'             - An absolute path to the model file.
#'         }
#' 
#' @export
#' 
TEL.submitJob <- function( executionType=NULL, workingDirectory, modelfile, extraInputFileExts=NULL, extraInputFiles=NULL, addargs=NULL, fisServer=TEL.getServer() ) {
    if(is.null(executionType)) {
        stop("Illegal Argument: executionType must be set and can't be NULL.")
    }
    if(is.null(modelfile)) {
        stop("Illegal Argument: modelfile must be set and can't be NULL.")
    }
    if(is.null(workingDirectory)) {
        stop("Illegal Argument: workingDirectory must be set and can't be NULL.")
    }
    if (!TEL.serverRunning(fisServer)) {
        stop("Server(s) is/are not running, unable to submit job. Server health details have been made available in the TEL.serverHealthDetails object.")
    }

    absoluteModelFilePath <- normalizePath(modelfile)
    
    if (!file.exists(absoluteModelFilePath)) {
      stop(paste('Illegal Argument: file ', absoluteModelFilePath, ' does not exist.'))
    }

    submission <- list()
    submission$start <- date()

    # Parent folder of the model file is the source directory
    sourceDirectory <- parent.folder(absoluteModelFilePath)

    # Resolve the extraInputFileExts against files in the model file's directory
    # and add these files to the existing vector of extraInputFiles
    for (fileExt in extraInputFileExts) {
        fileName <- paste0(file_path_sans_ext(basename(modelfile)), ".", fileExt)
        filePath <- file.path(sourceDirectory, fileName)
        if ( file.exists(filePath) && !file.info(filePath)$isdir ) {
            extraInputFiles <- c(extraInputFiles, fileName)
        }
    }
    
    submission$status <- ''
    
    parameters <- list()
    parameters$modelFile <- absoluteModelFilePath
    parameters$sourceDirectory <- sourceDirectory
    parameters$executionType <- executionType
    parameters$workingDirectory <- workingDirectory
    parameters$extraInputFiles <- as.list(extraInputFiles)
    parameters$commandParameters <- addargs
    
    submission$parameters <- parameters
    
    # Submit
    submission$fisJob <- submitJob(fisServer, .buildSubmissionJob(submission$parameters))

    if (!is.null(submission$fisJob)&&!is.null(submission$fisJob$id)) {
        submission$status <- 'Submitted'
    } else {
        submission$status <- 'Failed'
        stop(paste("Failed to submit job.\n  Server returned:", response$status, response$error, "\n ", response$exception, ":", response$message))
    }

    submission
}

################################################################################
#' .buildSubmissionJob
#'
#' Builds a list representing a job to be submitted from parameters list
#'
#' @param parameters named list with the following elements:
#'        \itemize{
#'          \item{\code{executionType}} - Execution type (e.g. NONMEM).
#'          \item{\code{modelFile}} - Path to a model file.
#'          \item{\code{workingDirectory}} - Job working directory.
#'          \item{\code{extraInputFiles}} - List for additional input files for job.
#'          \item{\code{commandParameters}} - Command parameters for Third Party Tool.
#'        }
#'
#' @return \code{job} named list representing FIS' job.
#'
.buildSubmissionJob <- function(parameters) {
    submittedJob <- list()
    submittedJob$executionType <- parameters$executionType
    submittedJob$executionFile <- parameters$modelFile
    submittedJob$workingDirectory <- parameters$workingDirectory
    submittedJob$extraInputFiles <- parameters$extraInputFiles
    submittedJob$commandParameters <- parameters$commandParameters
    return(submittedJob)
}

################################################################################
#' TEL.poll
#'
#' Continously polls the TEL server (at roughly 20 second intervals) for a
#' specified execution request jobID, until the TEL server either reports the
#' job as having completed successfully or as having failed.
#' 
#' @param submission Named list containing information relating to the
#'        submission of the execution request. The only items required by
#'        this function are:
#'        \itemize{
#'          \item{\code{fisJob}} - A job returned by FIS during submission.
#'        }
#' @param fisServer FISServer instance.
#' 
#' @return Updated \code{submission} named list augmented with the \code{status}
#'         of the job.
#' @export
#'
TEL.poll <- function(submission, fisServer=TEL.getServer(), ...) {
    if(is.null(submission)) {
        stop("Illegal Argument: submission can't be null")
    }
    if(!("fisJob" %in% names(submission)) || is.null(submission$fisJob)) {
        stop("Illegal Argument: submission's fisJob element must be set and can't be NULL. Was the job submitted?")
    }
    
    message("Running [ ", appendLF=FALSE )
    while (submission$fisJob$status != 'COMPLETED' && submission$fisJob$status != 'FAILED' ) {
        message(".", appendLF=FALSE)
        job = getJob(fisServer, submission$fisJob$id);
        if(is.null(job)) {
            stop(sprintf("Illegal State: job with id %s doesn't exist.",submission$fisJob$id))
        }
        submission$fisJob <- job
        Sys.sleep(fisServer@jobStatusPollingDelay)
    }
    message(" ]")
    submission
}


#' TEL.getNotImportedJobIDs
#'
#' @param fisServer FISServer instance.
#' 
#' Gets the IDs of the jobs that have completed but haven't been imported (assuming clearUp=TRUE)
#
TEL.getNotImportedJobIDs <- function(fisServer=TEL.getServer()) {
	
	jobs <- getJobs(fisServer)
	
	notImported <- list()
	
	for (job in jobs) {
		if (job$status == "COMPLETED" && file.exists(job$workingDirectory)) {
			notImported <- c( notImported, job$id )
		}
	}
	
	notImported
}
#
# What follows enables mocking of TEL functions during tests, will be removed once testthat has been upgraded.
#
if(!exists(".TEL")) {
    .TEL<-list()
}
.TEL$poll = TEL.poll
.TEL$submitJob = TEL.submitJob