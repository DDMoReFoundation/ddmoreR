

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
#' 
#' NB: Assumption: The data files are within the same directory as the model file. 
#' 
TEL.prepareWorkingFolder <- function(modelfile, tmpdir=tempdir(), extraInputFileExts=NULL) {

  if (file.exists(tmpdir) == FALSE) {
    stop("Temporary directory does not exist!")
  }
  tempFolder = tempfile("TEL.job",tmpdir)
  
  if (file.exists(modelfile) == FALSE) {
    stop("Model file missing")
  } else {
    if (file.exists(tempFolder) == FALSE) {
      dir.create(tempFolder)
    }
    
    # Obtain the full path to the model file
    modelfile <- file_path_as_absolute(modelfile)

    srcdir <- parent.folder(modelfile)
    
    inputs = file.path(srcdir, TEL.getInputs(modelfile, srcdir))
    
	if (!is.null(extraInputFileExts)) {
		
		extraInputFileExtsRegex <- paste0("\\.(", paste(extraInputFileExts, collapse="|"), ")$")
	
		# Include any extra files identified by the specified list of file extensions,
		# in the file copying to remote too (e.g. for PsN with a pre-executed NONMEM run)

		additionalFiles <- file.path(srcdir, list.files(srcdir, pattern=extraInputFileExtsRegex))
		
	} else {
		additionalFiles <- c()
	}
	
	flist = c(modelfile, inputs, additionalFiles)

    file.copy(flist, tempFolder)
    
  }
  
  tempFolder
}

