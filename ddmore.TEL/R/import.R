################################################################################
#' TEL.importFilesStep.
#' 
#' Retrieve the result files generated from an execution of an MDL file and copy
#' them into a suitable results subdirectory alongside the original MDL file.
#' 
#' @param submission Named list containing information relating to the
#'        submission of a job:
#'        \itemize{
#'          \item{\code{parameters}}
#'            - Input parameters used to submit job.
#'          \item{\code{parameters$importDirectory}}
#'            - the path of a directory, into which to copy the results. Default
#'              is a timestamped subdirectory of the input model source directory.
#'          \item{\code{status}}
#'            - The status of the execution of the model file. If not "COMPLETED"
#'              then this import into Standard Output object will not work.
#'          \item{\code{fisJob}} - structure returned from \link{TEL.getJob}
#'        }
#' @param fisServer - FISServer instance
#' @return The 'submission' named list, augmented with a \code{resultsDir} element
#'         specifying the directory into which the result files from the execution
#'         were copied, as specified by the \code{target} input argument.
#'
#' @author mwise
#' 
#' @export
#' 
#' @include StandardOutputObject.R
TEL.importFilesStep <- function(submission, fisServer, ... ) {
    if(is.null(submission)) {
        stop("Illegal Argument: submission can't be NULL.")
    }
    if(!("parameters" %in% names(submission)) || is.null(submission$parameters)) {
        stop("Illegal Argument: submission's 'parameters' element must be set and can't be NULL.")
    }
    if(!("fisJob" %in% names(submission)) || is.null(submission$fisJob)) {
        stop("Illegal Argument: submission's 'fisJob' element must be set and can't be NULL.")
    }
    if(!("id" %in% names(submission$fisJob)) || is.null(submission$fisJob$id)) {
        stop("Illegal Argument: fisJob's id element must be set and can't be NULL.")
    }
    if(!("importDirectory" %in% names(submission$parameters)) || is.null(submission$parameters$importDirectory)) {
        stop("Illegal Argument: importDirectory must be set and can't be NULL.")
    }
    submission$status = "Importing Results"
	
	jobID <- submission$fisJob$id
	modelfile <- submission$parameters$modelFile
	jobDirectory <- submission$parameters$workingDirectory
	target <- submission$parameters$importDirectory
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
		
		message('Done.\n')
		
		return(submission)
	}
}

################################################################################
#' TEL.importSOStep
#' 
#' Import results as Standard Output object(s)
#' 
#' Import the results retrieved from an execution of an MDL file, into an object
#' of class \linkS4class{StandardOutputObject}, or a list thereof.
#' 
#' @param submission Named list containing information relating to the
#'        submission of a job:
#'        \itemize{
#'          \item{\code{executionType}}
#'            - Identifying the target software to use for the execution.
#'          \item{\code{parameters}}
#'            - Input parameters used to submit job.
#'          \item{\code{parameters$modelFile}}
#'            - MDL file that was executed; any leading path will be stripped off,
#'              and the .mdl file extension replaced with .SO.xml, to derive the
#'              filename of the Standard Output XML file. If \code{submission$job}
#'              is available (which it will be if called as part of \link{TEL.monitor})
#'              then the \code{job$controlFile} is used instead of the \code{modelFile}
#'              since this will include any relative file path prefix on the control
#'              file i.e. if the model file references its data file via a relative path.
#'          \item{\code{parameters$importMultipleSO}}
#'            - Whether multiple SOBlocks are expected in the SO XML results file.
#'              Default is FALSE. Note that if FALSE and multiple SOBlocks are encountered,
#'              an error will be thrown.
#'          \item{\code{resultsDir}}
#'            - The directory into which the result files from the execution
#'              were copied.
#'          \item{\code{status}}
#'            - The status of the execution of the model file. If not "COMPLETED"
#'              then this import into Standard Output object will not work.
#'          \item{\code{fisJob}} - structure returned from \link{TEL.getJob}
#'        }
#' @return submission named list with 'so' element being an object of class \linkS4class{StandardOutputObject}, or if \code{multiple}
#'         is TRUE, then a list of such objects.
#' 
#' @author mwise
#' 
#' @export
#' 
#' @include LoadSOObject.R
#' @include StandardOutputObject.R
TEL.importSOStep <- function(submission, fisServer, ...) {
    if(is.null(submission)) {
      stop("Illegal Argument: submission can't be NULL.")
    }
    if(!("parameters" %in% names(submission)) || is.null(submission$parameters)) {
        stop("Illegal Argument: submission's 'parameters' element must be set and can't be NULL.")
    }
    if(!("modelFile" %in% names(submission$parameters)) || is.null(submission$parameters$modelFile)) {
      stop("Illegal Argument: parameters's 'modelFile' element must be set and can't be NULL.")
    }
    if(!("resultsDir" %in% names(submission)) || is.null(submission$resultsDir)) {
      stop("Illegal Argument: submission's 'resultsDir' element must be set and can't be NULL.")
    }
    multiple <- ifelse("importMultipleSO" %in% submission$parameters, submission$parameters$importMultipleSO, FALSE)
    
	soXMLFileName <- paste0(file_path_sans_ext(basename(submission$parameters$modelFile)), ".SO.xml")
	if (!is.null(submission$fisJob)) { # Should always be the case, if called as part of \link{TEL.monitor})
		# Take into account the fact that the control file, and thus the SO XML file, might be in a subdirectory
		soXMLFilePath <- file.path(submission$resultsDir, dirname(submission$fisJob$executionFile), soXMLFileName)
	} else {
		soXMLFilePath <- file.path(submission$resultsDir, soXMLFileName)
	}

	so <- NULL
	if (class(soXMLFilePath) == "character" && file.exists(soXMLFilePath)) {
		if (multiple) {
		    so <- LoadSOObjects(soXMLFilePath)
		} else {
		    so <- LoadSOObject(soXMLFilePath)
		}
	}
	else {
		stop(
			"No Standard Output results file produced from execution of model ", submission$parameters$modelFile,
			".\n  The contents of the working directory ", submission$parameters$workingDirectory,
			" may be useful for tracking down the cause of the failure."
		)
	}
	submission$so <- so
    return(submission)
}
