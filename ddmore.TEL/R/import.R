################################################################################
#' Import result files.
#' 
#' Retrieve the result files generated from an execution of an MDL file and copy
#' them into a suitable results subdirectory alongside the original MDL file.
#' 
#' @param submission Named list containing information relating to the
#'        submission of an execution request:
#'        \itemize{
#'          \item{\code{executionType}}
#'            - Identifying the target software to use for the execution.
#'          \item{\code{modelFile}}
#'            - MDL file that was executed.
#'          \item{\code{sourceDirectory}}
#'            - The directory in which the MDL file lives.
#'          \item{\code{workingDirectory}}
#'            - The location used for the execution of the job, within the
#'              system temporary directory.
#'          \item{\code{status}}
#'            - The status of the execution of the model file. If not "COMPLETED"
#'              then this import into Standard Output object will not work.
#'          \item{\code{requestId}} - Unique identifier for the submission request.
#'          \item{\code{job}} - structure returned from \link{TEL.getJob}
#'        }
#' @param target (Optional) Specify the path of a directory, into which to copy the results. Default
#'        is a timestamped subdirectory of the input model source directory.
#' @param clearUp (Optional) Logical dictating if the job working directory should
#'        be deleted upon successful job completion. Default is false, since this
#'        directory may contain useful information in the event that a job failed
#'        to execute successfully.
#' @return The 'submission' named list, augmented with a \code{resultsDir} element
#'         specifying the directory into which the result files from the execution
#'         were copied, as specified by the \code{target} input argument.
#'
#' @author mwise
#' 
#' @export
#' 
#' @include StandardOutputObject.R

TEL.importFiles <- function(submission, target=file.path(submission$sourceDirectory, format(Sys.time(), "%Y%b%d%H%M%S")), clearUp=FALSE) {
    if(is.null(submission)) {
        stop("Illegal Argument: submission can't be NULL.")
    }
    if(!("job" %in% names(submission)) || is.null(submission$job)) {
        stop("Illegal Argument: submission's 'job' element must be set and can't be NULL.")
    }
    if(!("id" %in% names(submission$job)) || is.null(submission$job$id)) {
        stop("Illegal Argument: job's id element must be set and can't be NULL.")
    }
    if(is.null(target)) {
        stop("Illegal Argument: target must be set and can't be NULL.")
    }
	
	jobID <- submission$requestID
	modelfile <- submission$modelFile
	jobDirectory <- submission$workingDirectory
	
	# FIXME: verify if the following logic is correct and if it is needed, 
	# if so make it caller's responsibility since it violates SRP
	if (nchar(modelfile) == 0 || nchar(jobDirectory) == 0) {
		# Model file and/or job directory not specified, so get them from Framework Integration service
		job = submission$job
		modelfile <- job$controlFile
		jobDirectory <- job$workingDirectory
	}
	
	workingFolder = file.path(jobDirectory) # should be FIS working directory not MIF working directory
	
	if (file.exists(workingFolder) == FALSE) {
		warning('Working directory ', workingFolder, ' does not exist; Job ', jobID, ' may have already been imported.\n')
	} else {
		# import the data
		message('Copying the result data back to the local machine for job ID ', jobID, '...')
		message('From ', workingFolder, ' to ', target)
		
		if (file.exists(target) == FALSE) {
			dir.create(target)
		}
		
		all.regular.files <- list.files(workingFolder, pattern="^[^.].*")
		# TODO: Remove the line below which is redundant from FIS version >= 0.0.5 introducing the execution.host.fileshare property (done under DDMORE-859)
		files.to.copy <- all.regular.files[which(all.regular.files!=jobID)] # Exclude the job directory (i.e. TES working directory)
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
		
		
        submission$resultsDir <- target
		
		if (clearUp==TRUE) {
			unlink(workingFolder, recursive=TRUE)
		}		
		message('Done.\n')
		
		submission
	}
}


################################################################################
#' Import results as Standard Output object(s)
#' 
#' Import the results retrieved from an execution of an MDL file, into an object
#' of class \linkS4class{StandardOutputObject}, or a list thereof.
#' 
#' @param submission Named list containing information relating to the
#'        submission of an execution request:
#'        \itemize{
#'          \item{\code{executionType}}
#'            - Identifying the target software to use for the execution.
#'          \item{\code{modelFile}}
#'            - MDL file that was executed; any leading path will be stripped off,
#'              and the .mdl file extension replaced with .SO.xml, to derive the
#'              filename of the Standard Output XML file. If \code{submission$job}
#'              is available (which it will be if called as part of \link{TEL.monitor})
#'              then the \code{job$controlFile} is used instead of the \code{modelFile}
#'              since this will include any relative file path prefix on the control
#'              file i.e. if the model file references its data file via a relative path.
#'          \item{\code{sourceDirectory}}
#'            - The directory in which the MDL file lives.
#'          \item{\code{resultsDir}}
#'            - The directory into which the result files from the execution
#'              were copied.
#'          \item{\code{workingDirectory}}
#'            - The location used for the execution of the job, within the
#'              system temporary directory.
#'          \item{\code{status}}
#'            - The status of the execution of the model file. If not "COMPLETED"
#'              then this import into Standard Output object will not work.
#'          \item{\code{requestId}} - Unique identifier for the submission request.
#'          \item{\code{job}} - structure returned from \link{TEL.getJob}
#'        }
#' @param multiple Whether multiple SOBlocks are expected in the SO XML results file.
#'        Default is FALSE. Note that if FALSE and multiple SOBlocks are encountered,
#'        an error will be thrown.
#' @return Object of class \linkS4class{StandardOutputObject}, or if \code{multiple}
#'         is TRUE, then a list of such objects.
#' 
#' @author mwise
#' 
#' @export
#' 
#' @include LoadSOObject.R
#' @include StandardOutputObject.R

TEL.importSO <- function(submission, multiple=FALSE) {
    if(is.null(submission)) {
      stop("Illegal Argument: submission can't be NULL.")
    }
    if(!("modelFile" %in% names(submission)) || is.null(submission$modelFile)) {
      stop("Illegal Argument: submission's 'modelFile' element must be set and can't be NULL.")
    }
    if(!("resultsDir" %in% names(submission)) || is.null(submission$resultsDir)) {
      stop("Illegal Argument: submission's 'resultsDir' element must be set and can't be NULL.")
    }

	soXMLFileName <- paste0(file_path_sans_ext(basename(submission$modelFile)), ".SO.xml")
	if (!is.null(submission$job)) { # Should always be the case, if called as part of \link{TEL.monitor})
		# Take into account the fact that the control file, and thus the SO XML file, might be in a subdirectory
		soXMLFilePath <- file.path(submission$resultsDir, dirname(submission$job$controlFile), soXMLFileName)
	} else {
		soXMLFilePath <- file.path(submission$resultsDir, soXMLFileName)
	}

	if (class(soXMLFilePath) == "character" && file.exists(soXMLFilePath)) {
		if (multiple) {
			LoadSOObjects(soXMLFilePath)
		} else {
			LoadSOObject(soXMLFilePath)
		}
	}
	else {
		stop(
			"No Standard Output results file produced from execution of model ", submission$modelFile,
			".\n  The contents of the working directory ", submission$workingDirectory,
			" may be useful for tracking down the cause of the failure."
		)
	}

}


#
# What follows enables mocking of TEL functions during tests, will be removed once testthat has been upgraded.
#
if(!exists(".TEL")) {
    .TEL<-list()
}
.TEL$importFiles = TEL.importFiles
.TEL$importSO = TEL.importSO
