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
TEL.startServer <- 
  function() {
  
    see.home <- TEL.checkConfiguration()
    
    startupScriptName = "startup.bat"
	startupScript <- file.path(see.home, startupScriptName)
    
    if(!file.exists(startupScript)) {
        stop(paste("Services startup script", startupScript," does not exist."))
    }
    
    message("Starting servers [ ", appendLF=FALSE)
    if (!TEL.serverRunning()) {
      # message("Server not running; starting server...")
      startupScriptStdErr <- file.path(see.home, paste0(".",startupScriptName, ".stderr"))
      startupScriptStdOut <- file.path(see.home, paste0(".",startupScriptName, ".stdout"))
      system2(startupScript,'/B', wait=F, stdout=startupScriptStdOut, stderr=startupScriptStdErr)  # /B argument suppresses the display of the command windows
      count = 0
      while ( count < 60 && !TEL.serverRunning() ) {
        Sys.sleep(1)
        count <- count+1
        message(".", appendLF=FALSE)
      }
      message(" ]", appendLF=FALSE)
      message() # Append a newline
      if (!TEL.serverRunning()) {
      	healthStatuses <- TEL.serverHealthcheck()
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
#' @param HOST Hostname of the server running the FIS service. Defaults to localhost.
#' @param OPERATIONAL_PORT Operational (as opposed to Service) port of the server
#'						   running the FIS service. Defaults to 9011.
#' @return True if the server is running, false if the server is not running.
#' 
#' @export
#' 
TEL.serverRunning <- 
  function(HOST='localhost', OPERATIONAL_PORT=9011) {

	healthStatuses <- TEL.serverHealthcheck(HOST, OPERATIONAL_PORT)
	
	# Make the server health details available to the user
	TEL.serverHealthDetails <<- healthStatuses
	
	(healthStatuses[[1]] == 'UP') && # Status of FIS itself
		all(lapply(healthStatuses[-1], function(srvStatus) { srvStatus$status }) == 'UP')
  }

################################################################################
#' TEL.serverHealthcheck
#'
#' Queries the health of FIS and its dependent servers.
#' 
#' @param HOST Hostname of the server running the FIS service. Defaults to localhost.
#' @param OPERATIONAL_PORT Operational (as opposed to Service) port of the server
#'						   running the FIS service. Defaults to 9011.
#' @return a named list detailing the status of FIS and its dependent services, i.e.:
#' $status
#' [1] "UP"
#' $ctsHealth
#' $ctsHealth$status
#' [1] "UP"
#' $mifHealth
#' $mifHealth$status
#' [1] "UP"
#' $db
#' $db$status
#' [1] "UP"
#' $db$database
#' [1] "H2"
#' $db$hello
#' [1] 1
#'
#' @export
#' 
TEL.serverHealthcheck <-
  function (HOST='localhost', OPERATIONAL_PORT=9011) {

	healthcheckUrl <- sprintf('http://%s:%s/health', HOST, OPERATIONAL_PORT)

	# Some explanation of this HTTP call and response handling.
	# We use getURL() rather than postForm() to ensure that a non-OK HTTP Status (i.e. the 503 Service
	# Unavailable returned from FIS if a component e.g. CTS is down) doesn't throw an error and we can
	# still access the JSON response data.
	# In fact, the HTTP status code doesn't actually seem to be available using getURL() - it is with
	# postForm() - but this is probably ok since I don't think we need it if we have the JSON statuses.
	tryCatch(
		{
			response <- RCurl:::getURL(healthcheckUrl)
			return(fromJSON(response))
		}, error = function(e) {
			return(list(status='DOWN', error=conditionMessage(e)))
		}
	)
	
}
  
################################################################################
#' TEL.stopServer
#'
#' Stops the TEL server.
#' 
#' @param HOST Hostname of the server running the FIS service. Defaults to localhost.
#' @param PORT Port of the server running the FIS service. Defaults to 9010.
#' @param dontWait (Internal usage only) Defaults to FALSE; only the R console
#' 				   termination should call this with TRUE
#'
TEL.stopServer <-
  function(HOST='localhost', PORT=9010, dontWait = FALSE) {
    message("Stopping server...")
	shutdownURL = sprintf('http://%s:%s/shutdown', HOST, PORT)
	ret = RCurl:::postForm(shutdownURL, style="HTTPPOST", shutdown="yes")
	if (ret[1]=="OK") {
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
#' @param HOST Hostname of the server running the FIS service. Defaults to localhost.
#' @param PORT Port of the server running the FIS service. Defaults to 9010.
#' @param OPERATIONAL_PORT Operational (as opposed to Service) port of the server
#'						   running the FIS service. Defaults to 9011.
#' 
#' @export
#'
q <- function(HOST='localhost', PORT=9010, OPERATIONAL_PORT=9011) {
	TEL.safeStop(HOST, PORT, OPERATIONAL_PORT, dontWait=TRUE);
	base:::q()
}

################################################################################
#' TEL.safeStop
#'
#' Checks that the TEL server is running and if so, stops it.
#' 
#' @param HOST Hostname of the server running the FIS service. Defaults to localhost.
#' @param PORT Port of the server running the FIS service. Defaults to 9010.
#' @param OPERATIONAL_PORT Operational (as opposed to Service) port of the server
#'						   running the FIS service. Defaults to 9011.
#' @param dontWait (Internal usage only) Defaults to FALSE; only the R console
#' 				   termination should call this with TRUE
#'
TEL.safeStop <-
  function(HOST='localhost', PORT=9010, OPERATIONAL_PORT=9011, dontWait = FALSE) {
    if (TEL.serverRunning(HOST, OPERATIONAL_PORT)) {
      TEL.stopServer(HOST, PORT, dontWait)
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
#'        directory as the model file, to be included in the execution.
#'        Primarily used by PsN e.g. to provide the .lst file for a PsN execution of
#'        an already-executed NONMEM run.
#' @param extraInputFiles (Optional) A vector of paths, either absolute or relative
#'        (to the model file), to any additional files to be included in the execution.
#'        Used as an alternative, and/or in conjunction with, extraInputFileExts.
#' @param addargs (Optional) Additional arguments to be passed to the target software.
#' @param HOST (Optional) Hostname of the server running the FIS service. Defaults
#'        to localhost.
#' @param PORT (Optional) Port of the server running the FIS service. Defaults to 9010.
#' @param OPERATIONAL_PORT (Optional) Operational (as opposed to Service) port of the
#'        server running the FIS service. Defaults to 9011.
#' 
#' @return \code{submission} named list containing information relating to the
#'         submission of the execution request:
#'         \itemize{
#'           \item{\code{executionType}}
#'             - Identifying the target software to use for the execution.
#'           \item{\code{modelFile}}
#'             - MDL file that was executed, without any leading path.
#'           \item{\code{sourceDirectory}}
#'             - The absolute path to the directory in which the MDL file lives.
#'           \item{\code{workingDirectory}}
#'             - The location used for the execution of the job, normally within
#'               the system temporary directory.
#'           \item{\code{status}}
#'             - The status of the execution of the model file, i.e. 'NEW'.
#'           \item{\code{requestId}} - Unique identifier for the submission request.
#'         }
#' 
#' @export
#' 
TEL.submitJob <- function( executionType=NULL, workingDirectory, modelfile, extraInputFileExts=NULL, extraInputFiles=NULL, addargs=NULL, HOST='localhost', PORT=9010, OPERATIONAL_PORT=9011 ) {
	
	if (!TEL.serverRunning(HOST, OPERATIONAL_PORT)) {
		stop("Server(s) is/are not running, unable to submit job. Server health details have been made available in the TEL.serverHealthDetails object.")
	}
	
    submission <- list()
	
	# Strip off the path to the model file leaving just the file name itself
	# TODO: Cater for relative paths of model files too? (currently just path-less files and absolute-path files are supported)
    modelfile_without_path <- basename(modelfile)
	
    # Parent folder of the model file is the source directory
	sourceDirectory <- parent.folder(modelfile)
	
	# Resolve the extraInputFileExts against files in the model file's directory
	# and add these files to the existing vector of extraInputFiles
	for (fileExt in extraInputFileExts) {
		matchingFilesAndDirs <- list.files(sourceDirectory, paste0(".*\\.", fileExt))
		matchingFiles <- matchingFilesAndDirs[ !file.info(paste0(sourceDirectory,"/",matchingFilesAndDirs))$isdir ]
		extraInputFiles <- c(extraInputFiles, matchingFiles)
	}
	
    submission$executionType <- executionType
    submission$modelFile <- modelfile
    submission$sourceDirectory <- sourceDirectory
    submission$workingDirectory <- workingDirectory
	
	submission$status <- ''

    # Build form
    parameters <- list(command=executionType, workingDirectory=workingDirectory, executionFile=modelfile, extraInputFiles=as.list(extraInputFiles), commandParameters=addargs)

    json <- toJSON(parameters)
    formParams=sprintf('%s%s','submissionRequest=',json)

	# Submit
	
	h <- basicTextGatherer()
	
	submitURL <- sprintf('http://%s:%s/submit', HOST, PORT)
	
	curlRet <- RCurl:::curlPerform(url=submitURL, postfields=formParams, writefunction=h$update)
	
	response <- fromJSON(h$value())
	
	if (!is.null(response$requestID)) {
		submission$requestID <- response$requestID
		submission$status <- 'NEW'
	} else {
		stop(paste("Failed to submit job.\n  Server returned:", response$status, response$error, "\n ", response$exception, ":", response$message))
	}
	
	submission
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
#'          \item{\code{requestID}} - Unique identifier of the submission request.
#'        }
#' @param HOST Hostname of the server running the FIS service. Defaults to localhost.
#' @param PORT Port of the server running the FIS service. Defaults to 9010.
#' 
#' @return Updated \code{submission} named list augmented with the \code{status}
#'         of the job.
#'
TEL.poll <- function(submission, HOST='localhost', PORT=9010) {
    
    jobID <- submission$requestID
    
    # poll status service (need to have jobID set before this)
    statusURL <- sprintf('%s/%s', sprintf('http://%s:%s/jobs', HOST, PORT), jobID)
    submission$status = fromJSON(httpGET(statusURL))$status
    
    while (submission$status != 'COMPLETED' && submission$status != 'FAILED' ) {
        message('Job ', jobID, ' is ', submission$status)
        Sys.sleep(20)
        submission$status <- fromJSON(httpGET(statusURL))$status
    }
    
    message('Job ', jobID, ' has ', submission$status, "\n")
    
    submission
}

#' TEL.getJobs
#'
#' Gets all jobs
#
TEL.getJobs <- function(HOST='localhost', PORT=9010) {
	
	jobsURL <- sprintf('http://%s:%s/jobs', HOST, PORT)
	
	jobs <- fromJSON(httpGET(jobsURL))
	
	jobs
}

#' TEL.getNotImportedJobIDs
#'
#' Gets the IDs of the jobs that have completed but haven't been imported (assuming clearUp=TRUE)
#
TEL.getNotImportedJobIDs <- function(HOST='localhost', PORT=9010) {
	
	jobs <- TEL.getJobs()
	
	notImported <- list()
	
	for (job in jobs) {
		if (job$status == "COMPLETED" && file.exists(job$workingDirectory)) {
			notImported <- c( notImported, job$id )
		}
	}
	
	notImported
}

#' TEL.getJob
#'
#' Gets a TEL job
#
TEL.getJob <- function(submission, HOST='localhost', PORT=9010) {
  
	jobID <- submission$requestID
  
	jobsURL = paste("http://", HOST, ":", PORT, "/jobs/", jobID, sep="")
	
	fromJSON(httpGET(jobsURL))
	
}
