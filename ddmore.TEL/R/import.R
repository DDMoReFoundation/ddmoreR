################################################################################
# Copyright (C) 2016 Mango Business Solutions Ltd, http://www.mango-solutions.com
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU Affero General Public License as published by the
# Free Software Foundation, version 3.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
# for more details.
#
# You should have received a copy of the GNU Affero General Public License along
# with this program. If not, see <http://www.gnu.org/licenses/agpl-3.0.html>.
################################################################################

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
importJobResultFiles <- function(fisJob, targetDirectory = NULL, fisServer = DDMORE.getServer()) {
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
	if (!is.empty(all.regular.files)) {
    	files.to.copy <- paste0(fisJob@workingDirectory, "/", all.regular.files) # Turn the filenames into full paths
    	log.debug(paste0("Copying result files [", paste(files.to.copy, collapse=",") ,"] to ", targetDirectory))
    	file.copy(files.to.copy, targetDirectory, recursive=TRUE)
	}
    
    # Copying std out, std err and conversion report log files
    
    stdOutFile <- getStdOutFile(fisJob)
    stdErrFile <- getStdErrFile(fisJob)
	convReportLogFile <- getConvReportLogFile(fisJob)
    
    if (file.exists(stdOutFile)) {
        file.copy(stdOutFile, file.path(targetDirectory, basename(stdOutFile)))
    }
    if (file.exists(stdErrFile)) {
        file.copy(stdErrFile, file.path(targetDirectory, basename(stdErrFile)))
    }
	if (file.exists(convReportLogFile)) {
		file.copy(convReportLogFile, file.path(targetDirectory, basename(convReportLogFile)))
	}
    
    message('Done.\n')
    return(fisJob)
}