# Package-local environment for storing shared variables
# This will be replaced in the future by Server class and its instance, which will hold 
# all FIS integration properties (ports, urls, polling settings, etc.) and this instance will be passed
# to execute (and other) functions
DDMORE.server.env <- new.env()

################################################################################
#' DDMORE.getServer
#'
#' Gets a handle on FIS Server instance.
#' 
#' @seealso \code{DDMORE.setServer}
#' @seealso \code{createFISServer}
#' 
#' @return FISServer instance
#' @export
DDMORE.getServer <- function() {
    if(!"fisServer" %in% ls(DDMORE.server.env)) {
        stop("FIS Server instance not configured")
    }
    return(DDMORE.server.env$fisServer)
}
################################################################################
#' DDMORE.setServer
#'
#' Sets a handle to default FIS Server instance
#' 
#' @seealso \code{DDMORE.getServer}
#' @seealso \code{createFISServer}
#' 
#' @return FISServer instance
#' @export
DDMORE.setServer <- function(fisServer) {
    .precondition.checkArgument(is.FISServer(fisServer), "fisServer", "FIS Server instance is required.")
    if("fisServer" %in% ls(DDMORE.server.env)) {
        warning(sprintf("One FIS Server instance is already configured pointing to %s. ", DDMORE.server.env$fisServer@url))
    }
    if(length(fisServer)==0) {
        warning("User authentication details not specified on the server object, jobs will be executed using framework service account. Use DDMORE.setUserInfo to specify authentication details.")
    }
    assign("fisServer", fisServer, envir = DDMORE.server.env)
}

################################################################################
#' DDMORE.startServer
#'
#' Starts the DDMORE server, by launching the startup.bat script within the root
#' of the SEE.
#' 
#' This is automatically called as part of launching the R console within
#' the MDL IDE within SEE; if the R console is not running within SEE in that
#' setup then this function is a no-op.
#' 
#' @export
DDMORE.startServer <- 
    function(fisServer) {
    .precondition.checkArgument(is.FISServer(fisServer), "fisServer", "FIS Server instance is required.")
    message("Starting servers [ ", appendLF=FALSE)
    if (!DDMORE.serverRunning(fisServer)) {
      if(fisServer@serverMode) {
        warning("Remote FIS server is not running. Check FIS server configuration.")
        return()
      }
      startupScriptDir <- dirname(fisServer@startupScript)
      startupScriptName <- basename(fisServer@startupScript)
      startupScriptStdErr <- file.path(startupScriptDir, paste0(".",startupScriptName, ".stderr"))
      startupScriptStdOut <- file.path(startupScriptDir, paste0(".",startupScriptName, ".stdout"))
      system2(fisServer@startupScript,'/B', wait=F, stdout=startupScriptStdOut, stderr=startupScriptStdErr)  # /B argument suppresses the display of the command windows
      count = 0
      while ( count < fisServer@startupPollingMax && !DDMORE.serverRunning(fisServer) ) {
        Sys.sleep(fisServer@startupPollingDelay)
        count <- count+1
        message(".", appendLF=FALSE)
        flush.console()
      }
      message(" ]", appendLF=FALSE)
      message() # Append a newline
      if (!DDMORE.serverRunning(fisServer)) {
      	healthStatuses <- DDMORE.serverHealthcheck(fisServer)
        stop(paste("Failure!", healthStatuses, sep="\n"))
        stop(paste("Server was unable to start. Refer to", startupScriptStdErr, " and ", startupScriptStdOut, "for details."))
      }
      message("Success!")
    } else {
      message(". ]", appendLF=FALSE)
      message() # Append a newline
      message("Success!")
    }
  }

################################################################################
#' DDMORE.serverRunning
#'
#' Checks if the FIS server, and its dependent servers, are running.
#' As a side effect, populates a DDMORE.serverHealthDetails global object so that the
#' user can access the health statuses of the individual server components.
#'
#' @param fisServer FISServer instance.
#' 
#' @return True if the server is running, false if the server is not running.
#' 
#' @export
#' 
DDMORE.serverRunning <- 
  function(fisServer) {

	healthStatuses <- health(fisServer)
	
	# Make the server health details available to the user
	DDMORE.serverHealthDetails <<- healthStatuses
	
	(healthStatuses[[1]] == 'UP') && # Status of FIS itself
		all(lapply(healthStatuses[-1], function(srvStatus) { srvStatus$status }) == 'UP')
  }
  
################################################################################
#' DDMORE.stopServer
#'
#' Stops the DDMORE server.
#' 
#' @param fisServer FISServer instance.
#' @param dontWait (Internal usage only) Defaults to FALSE; only the R console
#' 				   termination should call this with TRUE
#'
DDMORE.stopServer <-
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
#' This is automatically called when the R console is terminated.
#' 
#' @param fisServer FISServer instance.
#' 
#' @export
#'
q <- function(fisServer=DDMORE.getServer()) {
    if(fisServer@serverMode) {
        message("FIS is running in server mode, skipping shut down of FIS instance.")
    } else {
        status <- try(DDMORE.stopServer(fisServer, dontWait=TRUE))
        if(class(status)=="try-error") {
            warning("Could not stop FIS instance.")
        }
    }
	base:::q()
}

##############################################################
#' DDMORE.setUserInfo
#'
#' Function to set user authentication information that will be used to execute jobs.
#'
#' @examples 
#' DDMORE.setUserInfo(userName="Me", password="My-password")
#' DDMORE.setUserInfo(userName="Me", identityFile = "~/.ssh/id_rsa", identityFilePassphrase = "my-passphrase")
#' DDMORE.setUserInfo(userName="", executeAsUser = FALSE) 
#'
#' @param userName user name which executes the job, required.
#' @param password password of the user (optional if identity file (private key) is used for authentication)
#' @param identityFilePath path to the identity file (private key) on TES host which will be used by TES service account to authenticate as the user. (Optional if password-based authentication is used.)
#' @param identityFilePassphrase passphrase for the private key file. (Optional if password-based authentication is used.)
#' @param executeAsUser flag indicating if jobs should be invoked using service account or using account provided with this object. Setting this to FALSE is equivalent to not setting user info at all.
#'
#' @return user info list
#' @export
DDMORE.setUserInfo <- function(userName = NULL, password = NULL, identityFilePath = NULL, identityFilePassphrase = NULL, executeAsUser = TRUE) {
    .precondition.checkArgument(!is.null(userName),"userName", "User name must be set." )
    
    userInfo <- list("userName" = userName, 
    				"password" = password,
    				"identityFilePath" = identityFilePath,
    				"identityFilePassphrase" = identityFilePassphrase,
    				"executeAsUser" = executeAsUser
    				)
    
    fisServer <- DDMORE.getServer()
    fisServer@userInfo <- userInfo
    assign("fisServer", fisServer, envir = DDMORE.server.env)
    
    return(userInfo)
}

################################################################################
#' DDMORE.checkConfiguration
#'
#' Checks if this ddmore package is actually loaded (the R scripts may have
#' been \code{source}d instead). If so, then determine the path to the package
#' and navigate up the directory tree from this location to obtain the SEE home
#' directory, returning this directory. Therefore this assumes that the R
#' console has been launched from within the MDL IDE within SEE; otherwise
#' the function will raise an error.
#'
DDMORE.checkConfiguration <-
  function() {

	if ("package:ddmore" %in% search()) {
		# The package is loaded, proceed

    	packagePath <- path.package("ddmore")
    
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
        	stop(paste(errMsg, 'This is probably because you are not running the DDMORE R console from within an SEE environment.\nThe FIS and MIF servers must be started manually.', sep=""))
    	}
    
    	see.home # Return value
		
	} else {
		stop("The ddmore package is not loaded. The FIS and MIF servers must be started manually.")
	}
    
  }
################################################################################
#' DDMORE.submitJob
#' 
#' Submits a given job to FIS. Wrapper for \code{submitJob(FISServer)} method.
#' 
#' @param fisJob FISJob instance.
#' @param fisServer FISServer instance.
#' 
#' @return submitted FISJob
#' @export
#'
DDMORE.submitJob <- function(fisJob, fisServer = DDMORE.getServer()) {
    .precondition.checkArgument(is.FISJob(fisJob), "fisJob", "FISJob instance required.")
    .precondition.checkArgument(is.FISServer(fisServer), "fisServer", "FISServer instance required.")
    submitJob(fisServer, fisJob)
}

################################################################################
#' DDMORE.getJobs
#' 
#' Get list of jobs being executed by FIS. Wrapper for \code{getJobs(FISServer)} method.
#' 
#' @param fisServer FISServer instance.
#' 
#' @return list of FISJob object
#' @export
#'
DDMORE.getJobs <- function(fisServer = DDMORE.getServer()) {
    .precondition.checkArgument(is.FISServer(fisServer), "fisServer", "Server instance required.")
    getJobs(fisServer)
}

################################################################################
#' DDMORE.getJob
#' 
#' Get state of the job with a given Id. Wrapper for \code{getJob(FISServer, jobId)} method.
#' 
#' @param jobId job's id.
#' @param fisServer FISServer instance.
#' 
#' @return FISJob object
#' @export
#'
DDMORE.getJob <- function(jobId = NULL, fisServer = DDMORE.getServer()) {
    .precondition.checkArgument(is.FISServer(fisServer), "fisServer", "FISServer instance required.")
    .precondition.checkArgument(!is.null(jobId), "jobId", "Job id must be specified.")
    getJob(fisServer, jobId)
}

################################################################################
#' DDMORE.cancelJob
#' 
#' Cancels a given job. Wrapper for \code{cancelJob(FISServer, jobId)} method.
#' 
#' @param job FISJob instance.
#' @param fisServer FISServer instance.
#' 
#' @return FISJob object
#' @export
#'
DDMORE.cancelJob <- function(job = NULL, fisServer = DDMORE.getServer()) {
    .precondition.checkArgument(is.FISServer(fisServer), "fisServer", "FISServer instance required.")
    .precondition.checkArgument(is.FISJob(job), "job", "FISJob instance required.")
    cancelJob(fisServer, job)
}


################################################################################
#' DDMORE.printJobs
#' 
#' Print jobs with a given statuses. 
#' 
#' @param fisServer FISServer instance.
#' @param status FISJob statuses that should be included in the printout.
#' 
#' @return data.frame with jobs data
#' @export
#'
DDMORE.printJobs <- function(fisServer = DDMORE.getServer(), statuses = c('NEW', 'RUNNING', 'CANCELLING', 'FAILED', 'COMPLETED', 'CANCELLED')) {
    .precondition.checkArgument(is.FISServer(fisServer), "fisServer", "Server instance required.")
    .precondition.checkArgument(!is.null(statuses), "statuses", "Can't be null.")
    jobs <- getJobs(fisServer)
    tresult <- lapply(jobs, function(x) { .convertObjectToNamedList(x) })
    #We need to post-process the named lists so list elements are correctly displayed
    tresult <- lapply(tresult, 
                      function(x) { 
                            lapply(x, function(y) { 
                                if(length(y)==0) {
                                    if(is.numeric(y)) {
                                        return(0)
                                    } else {
                                        return("")
                                    }
                                } else {
                                    if(is.list(y)){
                                        return(paste(y, collapse=","))
                                    }
                                    return(y)
                                }
                            })
                        }
                        )
    df <- do.call(rbind.data.frame, tresult)
    return(df[df$status %in% statuses,])
}