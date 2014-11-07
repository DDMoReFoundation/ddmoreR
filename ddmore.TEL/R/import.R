
TEL.importFiles <- function(submission, target=file.path(submission$sourceDirectory, format(Sys.time(), "%Y%b%d%H%M%S")), clearUp=FALSE) {
	
	submission$resultsDir <- target
	
	jobID <- submission$requestID
	modelfile <- submission$modelFile
	jobDirectory <- submission$workingDirectory
	
	if (nchar(modelfile) == 0 || nchar(jobDirectory) == 0) {
		# Model file and/or job directory not specified, so get them from Framework Integration service
		job = TEL.getJob(jobID)	
		modelfile <- job$controlFile
		jobDirectory <- job$workingDirectory
	}
	
	workingFolder = file.path(jobDirectory) # should be FIS working directory not MIF working directory
	
	if (file.exists(workingFolder) == FALSE) {
		cat("Working directory", workingFolder, "does not exist; Job", jobID, "may have already been imported.\n")
	} else {
		# import the data
		cat(sprintf('Copying the result data back to the local machine for job ID: %s ...\n', jobID))
		cat(sprintf('From: %s to %s\n', workingFolder, target))
		
		if (file.exists(target)==FALSE) {
			dir.create(target)
		}
		
		all.regular.files <- list.files(workingFolder, pattern="^[^.].*")
		files.to.copy <- all.regular.files[-which(all.regular.files==jobID)] # Exclude the job directory (i.e. MIF working directory)
		files.to.copy <- paste0(workingFolder, "/", files.to.copy) # Turn the filenames into full paths
		
		file.copy(files.to.copy, target, recursive=TRUE)
		
		
		
# WAS the following. TODO: Check how the above file copying compares to the below (specifically, finding similarly named files to the control file).    
#		#  local.out.dir = sprintf("%s/%s", PROTO_HOME, jobID)
#		file.name.base = file_path_sans_ext(modelfile)
#		
#		#  dir.create(local.out.dir)
#		
#		#  file.copy(sprintf("%s/%s/%s/%s", MAPPED_DRIVE_NAME, jobID, 'inputs', paste(fn, sep = "")), local.out.dir)
#		
#        # Get anything that matches the filename from the job folder
#		similarnamedfilelist <- list.files(workingFolder, pattern=paste(file.name.base, "*.*", sep=""), full.names = TRUE)
#		
#		# Get any files that look like datafiles
#		# TODO: Determine what the actual data file(s) is
#		datafilelist <- list.files(workingFolder, "*.csv", full.names = TRUE)
#		
#		# Get any files that look like fit files
#		# TODO: Determine what the actual .fit file(s) is
#		fitfilelist <- list.files(workingFolder, "*.fit", full.names = TRUE)
#		
#		# Get any files that look like table files
#		# TODO: Determine what the actual table files are
#		tablefilelist <- list.files(workingFolder, pattern="^[a-z][a-z]tab[0-9]+$", full.names = TRUE)
#    
#        # Get lst file from NMFE execution
#        lstFile = paste(workingFolder, "output.lst", sep="\\")
#        
#        fileslist <- c(similarnamedfilelist, datafilelist, fitfilelist, tablefilelist)
#        
#        if (file.exists(lstFile) == TRUE) {
#            fileslist <- c(fileslist, lstFile)
#        }
#    
#		file.copy(fileslist, target)
		
		
		
		if (clearUp==TRUE) {
			unlink(workingFolder, recursive=TRUE)
		}		
		cat('Done.\n\n')
		
		submission
	}
}


################################################################################
#' Import the results retrieved from an execution of an MDL file, into an object
#' of class \linkS4class{StandardOutputObject}.
#' 
#' @param submission Named list containing information relating to the
#'        submission of an execution request:
#'        \itemize{
#'          \item{executionType} - Identifying the target software to use for
#'                                 the execution.
#'          \item{modelFile} - MDL file that was executed.
#'          \item{sourceDirectory} - The directory in which the MDL file lives.
#'          \item{workingDirectory} - The location used for the execution of the
#'                                    job, within the system temporary directory.
#'          \item{status} - The status of the execution of the model file.
#'                          If not "COMPLETED" then this import into Standard
#'                          Output object will not work.
#'          \item{requestId} - Unique identifier for the submission request.
#'        }
#' @return Object of class \linkS4class{StandardOutputObject}.
#'
#' @author mwise
#' 
#' @export
#' 
#' @include StandardOutputObject.R

TEL.importSO <- function(submission) {
	
	soXMLFilename <- paste0(file_path_sans_ext(submission$modelFile), "_SO.xml")
	
	SOObject = LoadSOObject(file.path(submission$resultsDir, soXMLFilename))
	
	# Begin Workaround Hack: To load in required part of Residuals into SO
	
	indwres = read.csv(file.path(submission$resultsDir,"ddmore_indwres.csv"), header=T) # TODO: Filename and 'sep'arator need to be taken from the S.O.
	names(indwres) <- c("ID", "TIME", "IWRES")
	# Add an 'i' to the begining of the ID column: may not be needed in future  
	indwres$ID <- sub("^", "i", indwres$ID )
	
	popwres = read.csv(file.path(submission$resultsDir,"ddmore_popwres.csv"), header=T) # TODO: Filename and 'sep'arator need to be taken from the S.O.
	names(popwres) <- c("ID", "TIME", "WRES")
	# Add an 'i' to the begining of the ID column: may not be needed in future  
	popwres$ID <- sub("^", "i", popwres$ID )
	
	SOObject@Estimation@Residuals$Population = popwres
	SOObject@Estimation@Residuals$Individual = indwres
	
	# End Workaround Hack
	
	SOObject
}

