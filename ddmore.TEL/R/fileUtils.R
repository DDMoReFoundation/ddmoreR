TEL.prepareWorkingFolder <- function(modelfile=NULL, src=getwd(), tmpdir=tempdir()) {

  if(file.exists(tmpdir) == FALSE) {
    stop("Temporary directory does not exist!")
  }
  tempFolder = tempfile("TEL.job",tmpdir)
  if(file.exists(modelfile) == FALSE) {
		stop("Model file missing")
  } else {
    if(file.exists(tempFolder) == FALSE) {
      dir.create(tempFolder)
    }
  
    modelfile = paste(src, modelfile, sep="/")
    
    inputs = paste(src, TEL.getInputs(modelfile), sep="/")
    
    flist = c(modelfile, inputs)
  
    file.copy(flist, tempFolder)
    
  }
  tempFolder
}

TEL.import <- function(outputObject=NULL, target=NULL, clearUp=FALSE) {

  jobID <- outputObject$requestID
  modelfile <- outputObject$modelFile
  jobDirectory <- outputObject$workingDirectory
    
	if (nchar(modelfile) == 0 || nchar(jobDirectory) == 0) {
    # Model file and/or job directory not specified, so get them from Framework Integration service
		job = TEL.getJob(jobID)	
		modelfile <- job$controlFile
		jobDirectory <- job$workingDirectory
	}
	
	workingFolder = paste(jobDirectory, jobID, sep="/" )
	
	if (file.exists(workingFolder) == FALSE) {
		cat("Job ", jobID, " already imported")
	} else {
		# import the data
		cat(sprintf('Copying the result data back to local machine for job ID: %s ...\n', jobID))
		cat(sprintf('From: %s\\%s to %s\n', jobDirectory, jobID, target))
		
		if(file.exists(target)==FALSE) {
			dir.create(target)
		}
    
		#  local.out.dir = sprintf("%s/%s", PROTO_HOME, jobID)
		file.name.base = gsub('.mdl', '', modelfile)
		file.name.base = gsub('.xml', '', file.name.base)
		file.name.base = gsub('.ctl', '', file.name.base)
		
		#  dir.create(local.out.dir)
		
		#  file.copy(sprintf("%s/%s/%s/%s", MAPPED_DRIVE_NAME, jobID, 'inputs', paste(fn, sep = "")), local.out.dir)
		
        # Get anything that matches the filename from the job folder
		similarnamedfilelist <- list.files(workingFolder, pattern=paste(file.name.base, "*.*", sep=""), full.names = TRUE)
		
		# Get any files that look like datafiles
		# TODO: Determine what the actual data file(s) is
		datafilelist <- list.files(workingFolder, "*.csv", full.names = TRUE)
		
		# Get any files that look like fit files
		# TODO: Determine what the actual .fit file(s) is
		fitfilelist <- list.files(workingFolder, "*.fit", full.names = TRUE)
		
		# Get any files that look like table files
		# TODO: Determine what the actual table files are
		tablefilelist <- list.files(workingFolder, pattern="^[a-z][a-z]tab[0-9]+$", full.names = TRUE)
    
        # Get lst file from NMFE execution
        lstFile = paste(workingFolder, "output.lst", sep="\\")
        
        fileslist <- c(similarnamedfilelist, datafilelist, fitfilelist, tablefilelist)
        
        if (file.exists(lstFile) == TRUE) {
            fileslist <- c(fileslist, lstFile)
        }
    
		file.copy(fileslist, target)
		
		if (clearUp==TRUE) {
			unlink(jobDirectory, recursive=TRUE)
		}		
		cat('Done.\n\n')
    
		outputObject
	}
}
