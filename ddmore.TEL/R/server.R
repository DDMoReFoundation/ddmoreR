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