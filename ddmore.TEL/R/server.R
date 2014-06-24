
TEL.startServer <- 
  function() {
    TEL.checkConfiguration()
    cat("TEL.startServer\n")
    #path.package("DDMoRe.TEL")
    packagePath <- "C:/Dev/workspace/mif-exec-exec" 
    startMIF <- paste(packagePath, "/mif-exec/startup-mif.bat", sep="")
    startFIS <- paste(packagePath, "/fis/startup.bat", sep="")
    
    if(file.exists(startMIF) && file.exists(startFIS) && TEL.serverRunning() == FALSE) {
      cat("Not started, so starting server\n")
      shell(shQuote(startMIF), wait=F)
      shell(shQuote(startFIS), wait=F)
      count = 0
      while( count < 20 && TEL.serverRunning() == FALSE ) {
        Sys.sleep(1)
        count <- count+1
        cat("Paused\n")
      }
    }
    cat("Started server\n")
  }

TEL.serverRunning <- 
  function(HOST='localhost', PORT='9010') {

    ret <- "notok"

	  healthURL = sprintf('http://%s:%s/healthcheck', HOST, PORT)
	
    # Need some way to work out whether the server is running, apart from this. When the server isn't running, this throws an error
    ret = RCurl:::postForm(healthURL, style="HTTPPOST", health="yes")

    #if(RCurl:::url.exists(healthURL)) {
     # ret = RCurl:::postForm(healthURL, style="HTTPPOST", health="yes")
    #}
    ret[1]=="ok"
  }

TEL.stopServer <-
  function(HOST='localhost', PORT='9010') {
    cat("Stopping server\n")
	shutdownURL = sprintf('http://%s:%s/shutdown', HOST, PORT)
	ret = RCurl:::postForm(shutdownURL, style="HTTPPOST", shutdown="yes")
    ret[1]=="OK"
  }

TEL.safeStop <-
  function() {
    cat("Safestop\n")
    if(TEL.serverRunning()) {
      TEL.stopServer()
    }
  }

TEL.checkConfiguration <-
  function() {
#    packagePath <- path.package("DDMoRe.TEL")
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
  
#        cat( mif.configuration.dir,
#             mif.working.dir,
#             mif.templatesDirectory,
#             mif.commonScriptsDirectory,
#             mif.genericScriptsDirectory, sep="\n", file=mifProperties)
#        close(mifProperties)
#      }
#    }
  }

  submit.job <- function( command=NULL, workingDirectory = NULL, modelfile = NULL, addargs="", HOST='localhost', PORT='9010', ... ) {
    outputObject <- list()
    attributes(outputObject) <- list(class="outputObject")
    
    outputObject$command <- command
    outputObject$modelFile <- modelfile
    outputObject$workingDirectory <- workingDirectory
    
    # Build form
	  parameters <- c(command=command, workingDirectory=workingDirectory, executionFile=modelfile)
	  if(nchar(addargs) > 0) {
		  parameters <- c(parameters, commandParameters=addargs)
	  }
	  
	  json <- toJSON(parameters)
	  formParams=sprintf('%s%s','submissionRequest=',json)
	  
	  # Submit
	  
	  h<-basicTextGatherer()
	  
	  submitURL <- sprintf('http://%s:%s/submit', HOST, PORT)
	  
	  ret <- RCurl:::curlPerform(url=submitURL, postfields=formParams, writefunction=h$update)
	  #postForm("http://localhost:9010/submit", style="HTTPPOST", submissionRequest = json )
	  
    
	  response <- fromJSON(h$value())
    
    outputObject$requestID <- fromJSON(h$value())$requestID
    
    outputObject$ret <- ret[1]
  
    outputObject
    
	  #c(ret[1], response, workingDirectory )
  }
  
  TEL.poll <- function(outputObject = NULL, HOST='localhost', PORT='9010') {
	  
    jobID <- outputObject.getJobID(outputObject)# outputObject$submitResponse[2]$requestID
    
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
  
TEL.getJob <- function(outputObject = NULL, HOST='localhost', PORT='9010') {
  
  jobID <- jobID <- outputObject.getJobID(outputObject)
  
	jobsURL = paste("http://", HOST, ":", PORT, "/jobs/", jobID, sep="")
	
	fromJSON(httpGET(jobsURL))
	
}