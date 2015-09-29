################################################################################
#' importJobResultFiles
#' 
#' Retrieve the result files generated from an execution of the job working directory to
#' specified directory.
#' 
#' @param fisJob the FIS Job for which results should be retrieved
#' @param targetDirectory the directory where the result files should be copied into
#' @param fisServer the FIS Server
#' 
#' @export
#' @author mrogalski
#' 
#' @return \code{FISJob} instance.
#' 
importJobResultFiles <- function(fisJob, targetDirectory = NULL, fisServer = TEL.getServer()) {
    .precondition.checkArgument(is.FISJob(fisJob), "fisJob", "Needs to be of type FISJob.")
    .precondition.checkArgument(is.FISServer(fisServer), "fisServer", "Needs to be of type FISServer.")
    .precondition.checkArgument(!is.null(targetDirectory), "targetDirectory", "Must be set and can't be NULL.")
    .precondition.checkArgument(file.exists(fisJob@workingDirectory), "fisJob@workingDirectory", sprintf("%s does not exist.",fisJob@workingDirectory))
    message('Copying the result data back to the local machine for job ID ', fisJob@id, '...')
    message('From ', fisJob@workingDirectory, ' to ', targetDirectory)
    
    if (!file.exists(targetDirectory)) {
        dir.create(targetDirectory)
    }
    
    all.regular.files <- list.files(fisJob@workingDirectory, pattern=".*", all.files=TRUE, no..=TRUE)
    exclusionPattern <- sprintf("^(?!(%s))", str_replace_all(FIS_JOB_METADATA_DIR, "\\.", "\\\\."))
    all.regular.files <- all.regular.files[grepl(exclusionPattern, all.regular.files, perl=TRUE)]
    files.to.copy <- paste0(fisJob@workingDirectory, "/", all.regular.files) # Turn the filenames into full paths
    log.debug(paste0("Copying result files [", paste(files.to.copy, collapse=",") ,"] to ",targetDirectory))
    file.copy(files.to.copy, targetDirectory, recursive=TRUE)
    
    # copying std out and error stream files
    stdOutFile<-getStdOutFile(fisJob)
    stdErrFile<-getStdErrFile(fisJob)
    
    if(file.exists(stdOutFile)) {
        file.copy(stdOutFile, file.path(targetDirectory,basename(stdOutFile)))
    }
    if(file.exists(stdErrFile)) {
        file.copy(stdErrFile, file.path(targetDirectory,basename(stdErrFile)))
    }
    
    message('Done.\n')
    return(fisJob)
}