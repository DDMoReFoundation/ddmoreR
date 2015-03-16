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
    
    message("Starting FIS and MIF servers...")
    
	startupScript <- file.path(see.home, "startup.bat")
    
    if (!TEL.serverRunning()) {
      message("Server not running; starting server...")
      system(paste(shQuote(startupScript), '/B'), wait=F)  # /B argument suppresses the display of the command windows
      count = 0
      message("Retries: ", appendLF=FALSE)
      while ( count < 60 && !TEL.serverRunning() ) {
        Sys.sleep(1)
        count <- count+1
        message(count, " ", appendLF=FALSE)
      }
      message() # Append a newline
      if (!TEL.serverRunning()) {
        stop("Server was unable to start")
      }
    }
    message("Server is running!")
  }

################################################################################
#' TEL.serverRunning
#'
#' Checks if the TEL server is running.
#'
#' @param HOST Hostname of the server running the FIS service. Defaults to localhost.
#' @param PORT Port of the server running the FIS service. Defaults to 9010.
#' @return True if the server is running, false if the server is not running.
#' 
TEL.serverRunning <- 
  function(HOST='localhost', PORT='9010') {

    healthcheckUrl <- sprintf('http://%s:%s/healthcheck', HOST, PORT)
    
    tryCatch(
        {
            response <- RCurl:::postForm(healthcheckUrl, style="POST")
            return(response == "ok")
        }, error = function(e) {
            return(FALSE)
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
  function(HOST='localhost', PORT='9010', dontWait = FALSE) {
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
#' 
#' @export
#'
q <- function(HOST='localhost', PORT='9010') {
	TEL.safeStop(HOST, PORT, dontWait=TRUE);
	base:::q()
}

################################################################################
#' TEL.safeStop
#'
#' Checks that the TEL server is running and if so, stops it.
#' 
#' @param HOST Hostname of the server running the FIS service. Defaults to localhost.
#' @param PORT Port of the server running the FIS service. Defaults to 9010.
#' @param dontWait (Internal usage only) Defaults to FALSE; only the R console
#' 				   termination should call this with TRUE
#'
TEL.safeStop <-
  function(HOST='localhost', PORT='9010', dontWait = FALSE) {
    if (TEL.serverRunning(HOST, PORT)) {
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
#' @param modelfile Path-less filename, or absolute path, to the MDL file to be
#'        executed. Note that relative paths are currently not supported.
#' @param HOST Hostname of the server running the FIS service. Defaults to localhost.
#' @param PORT Port of the server running the FIS service. Defaults to 9010.
#' @param addargs Additional arguments to be passed to the target software.
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
TEL.submitJob <- function( executionType=NULL, workingDirectory, modelfile, HOST='localhost', PORT='9010', addargs="", ... ) {
	
    submission <- list()
    
    # Strip off the path to the model file leaving just the file name itself
    # TODO: Cater for relative paths of model files too? (currently just path-less files and absolute-path files are supported)
    modelfile_without_path <- tail(strsplit(modelfile, "[\\\\|/]")[[1]], n=1)

    submission$executionType <- executionType
    submission$modelFile <- modelfile_without_path
    # Parent folder of the model file is the source directory
    submission$sourceDirectory <- parent.folder(modelfile)
    submission$workingDirectory <- workingDirectory
	
	submission$status <- ''

    # Build form
    parameters <- c(command=executionType, workingDirectory=workingDirectory, executionFile=(modelfile_without_path))
	if (nchar(addargs) > 0) {
        parameters <- c(parameters, commandParameters=addargs)
    }

    json <- toJSON(parameters)
    formParams=sprintf('%s%s','submissionRequest=',json)

    # Submit

    h<-basicTextGatherer()

    submitURL <- sprintf('http://%s:%s/submit', HOST, PORT)
	  
    ret <- RCurl:::curlPerform(url=submitURL, postfields=formParams, writefunction=h$update)
	
	if (ret[[1]] == 0) {

	    response <- fromJSON(h$value())
	
	    submission$requestID <- response$requestID
		
		submission$status <- 'NEW'

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
TEL.poll <- function(submission, HOST='localhost', PORT='9010') {
    
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
TEL.getJobs <- function(HOST='localhost', PORT='9010') {
	
	jobsURL <- sprintf('http://%s:%s/jobs', HOST, PORT)
	
	jobs <- fromJSON(httpGET(jobsURL))
	
	jobs
}

#' TEL.getNotImportedJobIDs
#'
#' Gets the IDs of the jobs that have completed but haven't been imported (assuming clearUp=TRUE)
#
TEL.getNotImportedJobIDs <- function(HOST='localhost', PORT='9010') {
	
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
TEL.getJob <- function(submission, HOST='localhost', PORT='9010') {
  
	jobID <- submission$requestID
  
	jobsURL = paste("http://", HOST, ":", PORT, "/jobs/", jobID, sep="")
	
	fromJSON(httpGET(jobsURL))
	
}
