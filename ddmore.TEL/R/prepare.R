
# Assumption: The data files are within the same directory as the model file.
TEL.prepareWorkingFolder <- function(modelfile, tmpdir=tempdir()) {

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
    
	# Include any *.lst files in the file copying to remote too (for PsN with a pre-executed NONMEM run)
	lstfiles <- file.path(srcdir, list.files(srcdir, pattern=".*\\.lst"))
	
    flist = c(modelfile, inputs, lstfiles)

    file.copy(flist, tempFolder)
    
  }
  
  tempFolder
}

