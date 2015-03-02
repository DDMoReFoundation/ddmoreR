

################################################################################
#' TEL.prepareWorkingFolder
#' 
#' Copies the model file and any associated data files or other related files,
#' into a newly created working directory that is a subdirectory of the specified
#' temporary directory (which defaults to the system temporary directory).
#' 
#' @param modelFile The path to the model file (normally .mdl; other file types
#'                  may or may not be supported) to copy
#' @param tmpdir The temporary directory in which to create the 'working directory'
#'               subfolder that is the target of the file copying; defaults to the
#'               system temporary directory
#' @param extraInputFileExts An optional vector of file extensions (excluding the
#'                           dot) that will be used in identifying additional files
#'                           to copy
#' @param extraInputFiles An optional vector of file paths (relative to the model
#'                        file) that will be used in identifying additional files
#'                        to copy
#' 
#' @note The data files are within the same directory as the model file. Also, if
#'       the data files and additional files are in a directory structure, i.e. not
#'       all within the same directory as the model file, then this directory
#'       structure is 'flattened' when copying to the job working directory.
#'       This behaviour should probably be reviewed at some point.
#'
TEL.prepareWorkingFolder <- function(modelfile, tmpdir=tempdir(), extraInputFileExts=NULL, extraInputFiles=NULL) {

	if (!file.exists(tmpdir)) {
		stop("Temporary directory does not exist!")
	}
	tempFolder = tempfile("TEL.job",tmpdir)
  
	if (!file.exists(modelfile)) {
    	stop("Model file missing")
	} else {
    if (!file.exists(tempFolder)) {
		dir.create(tempFolder)
    }
    
    # Obtain the full path to the model file
    modelfile <- file_path_as_absolute(modelfile)

    srcdir <- parent.folder(modelfile)
    
    inputs = file.path(srcdir, TEL.getInputs(modelfile, srcdir))
    
	# Initialise the additional files list
	additionalFiles <- c()
	
	if (length(extraInputFileExts) > 0) { # The length check also handles null
		
		extraInputFileExtsRegex <- paste0("\\.(", paste(extraInputFileExts, collapse="|"), ")$")
	
		# Include any extra files identified by the specified list of file extensions,
		# in the file copying to remote too (e.g. for PsN with a pre-executed NONMEM run)

		additionalFiles <- c(additionalFiles, file.path(srcdir, list.files(srcdir, pattern=extraInputFileExtsRegex)))
	}
	
	if (length(extraInputFiles) > 0) { # The length check also handles null
		
		# Include any extra files identified by the specified list of relative file paths,
		# in the file copying to remote too (e.g. for PsN with a pre-executed NONMEM run)

		additionalFiles <- c(additionalFiles, file.path(srcdir, extraInputFiles))
	}
	
	flist = c(modelfile, inputs, additionalFiles)

    file.copy(flist, tempFolder)
    
  }
  
  tempFolder
}

