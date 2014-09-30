#'TEL.startServer 
#'
#' Starts TEL sever
#'
TEL.startServer <- 
  function() {
  
    homeDirs <- TEL.checkConfiguration()
    fis.home <- homeDirs[1]
    mif.home <- homeDirs[2]
    
    cat("Starting FIS and MIF servers...\n")
    
    startFIS <- file.path(fis.home, "startup.bat")
    startMIF <- file.path(mif.home, "startup.bat")
    
    if (!TEL.serverRunning()) {
      cat("FIS server not running; starting server...\n")
      system(shQuote(startMIF), wait=F)
      system(shQuote(startFIS), wait=F)
      count = 0
      cat("Retries: ")
      while ( count < 60 && !TEL.serverRunning() ) {
        Sys.sleep(1)
        count <- count+1
        cat(count)
        cat(" ")
      }
      cat("\n")
      if (!TEL.serverRunning()) {
        stop("Server was unable to start")
      }
    }
    cat("Server is running!")
  }

#' TEL.serverRunning 
#'
#' Checks if TEL server is running
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
  
#' TEL.stopServer 
#'
#' Stops TEL server
#'
TEL.stopServer <-
  function(HOST='localhost', PORT='9010') {
    cat("Stopping server...\n")
	shutdownURL = sprintf('http://%s:%s/shutdown', HOST, PORT)
	ret = RCurl:::postForm(shutdownURL, style="HTTPPOST", shutdown="yes")
	if (ret[1]=="OK") {
		cat("Server is now stopped.")
	} else {
		cat("Server could not be stopped.")
	}
  }
  
#' Override the default quit function to try to stop the server before quitting normally.
#' @export
q <- function() {
	TEL.safeStop();
	base:::q()
}

#' TEL.safeStop
#'
#' Safely stops TEL server
#'
TEL.safeStop <-
  function(HOST='localhost', PORT='9010') {
    if (TEL.serverRunning(HOST, PORT)) {
      TEL.stopServer(HOST, PORT)
    }
  }

#' TEL.checkConfiguration
#'
#' Checks TEL Configuration
#'
TEL.checkConfiguration <-
  function() {
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
        warning(paste(errMsg, 'This is probably because you are not running the TEL console from within an SEE environment.\nThe FIS and/or MIF servers must be started manually.', sep=""))
    }
    
    c(fis.home, mif.home) # Return value
    
    
#    configFilename = paste(packagePath, "/exec/mif/etc/mif.properties", sep="")
#    if(!file.exists(paste(configFilename))) {
#      success = file.create(configFilename, showWarnings = FALSE, overwrite=TRUE, recursive=TRUE)
#      if(success == TRUE) {
#        service.home=paste(packagePath, "\\\\exec", sep="")
#        mif.home=paste(service.home, "\\\\mif", sep="")
#  
#        mifProperties <- file(description=configFilename, open="wt")
#        
#        mif.configuration.dir      <- cat("mif.configuration.dir=", mif.home, "\\\\etc", sep="")
#        mif.working.dir            <- cat("mif.working.dir=", mif.home, "\\\\metadata", sep="")
#        mif.templatesDirectory     <- cat("mif.templatesDirectory=", service.home, "\\\\templates", sep="")
#        mif.commonScriptsDirectory <- cat("mif.commonScriptsDirectory=", service.home, "\\\\scripts", sep="")
#        mif.genericScriptsDirectory <- cat("mif.genericScriptsDirectory=", service.home, "\\\\scripts", sep="")
#  
#        cat( mif.configuration.dir,
#             mif.working.dir,
#             mif.templatesDirectory,
#             mif.commonScriptsDirectory,
#             mif.genericScriptsDirectory, sep="\n", file=mifProperties)
#        close(mifProperties)
#      }
#    }
  }

#' submit.job
#'
#' Submits a job to the TEL server
#'  
submit.job <- function( command=NULL, workingDirectory, modelfile, HOST='localhost', PORT='9010', addargs="", ... ) {
    outputObject <- list()
    attributes(outputObject) <- list(class="outputObject")
    
    # Strip off the path to the model file leaving just the file name itself
    # TODO: Cater for relative paths of model files too? (currently just path-less files and absolute-path files are supported)
    modelfile_without_path <- tail(strsplit(modelfile, "[\\\\|/]")[[1]], n=1)

    outputObject$command <- command
    outputObject$modelFile <- modelfile_without_path
    # Parent folder of the model file is the source directory
    outputObject$sourceDirectory <- parent.folder(modelfile)
    outputObject$workingDirectory <- workingDirectory

    # Build form
    parameters <- c(command=command, workingDirectory=workingDirectory, executionFile=(modelfile_without_path))
	if (nchar(addargs) > 0) {
        parameters <- c(parameters, commandParameters=addargs)
    }

    json <- toJSON(parameters)
    formParams=sprintf('%s%s','submissionRequest=',json)

    # Submit

    h<-basicTextGatherer()

    submitURL <- sprintf('http://%s:%s/submit', HOST, PORT)
	  
    ret <- RCurl:::curlPerform(url=submitURL, postfields=formParams, writefunction=h$update)

    response <- fromJSON(h$value())

    outputObject$requestID <- fromJSON(h$value())$requestID

    outputObject$ret <- ret[1]

    outputObject
}
 
#' TEL.poll
#'
#' Polls TEL
#
TEL.poll <- function(outputObject = NULL, HOST='localhost', PORT='9010') {
    
    jobID <- outputObject.getJobID(outputObject)  # outputObject$submitResponse[2]$requestID
    
    # poll status service (need to have jobID set before this)
    statusURL <- sprintf('%s/%s', sprintf('http://%s:%s/jobs', HOST, PORT), jobID)
    outputObject$status = fromJSON(httpGET(statusURL))$status
    
    while(outputObject$status != 'COMPLETED' && outputObject$status != 'FAILED' ) {
        cat(sprintf('Job %s still executing ... (status %s)\n', jobID, outputObject$status))
        Sys.sleep(20)
        outputObject$status <- fromJSON(httpGET(statusURL))$status
    }
    
    cat(sprintf('\nJob %s finished with status %s.\n\n', jobID, outputObject$status))
    
    outputObject
}

#' TEL.getJobs
#'
#' Gets TEL jobs
#
TEL.getJobs <- function(HOST='localhost', PORT='9010') {
	
	jobsURL = sprintf('http://%s:%s/jobs', HOST, PORT)
	
	jobs = fromJSON(httpGET(jobsURL))
	
	notImported = list()
	
	for(job in jobs) {
		if(job$status == "COMPLETED" && file.exists(job$workingDirectory)) {
			notImported <- c( notImported, job$id )
		}	
	}
	notImported
}

#' TEL.getJob
#'
#' Gets a TEL job
#
TEL.getJob <- function(outputObject = NULL, HOST='localhost', PORT='9010') {
  
  jobID <- jobID <- outputObject.getJobID(outputObject)
  
	jobsURL = paste("http://", HOST, ":", PORT, "/jobs/", jobID, sep="")
	
	fromJSON(httpGET(jobsURL))
	
}
