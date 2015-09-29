#
# Author: mrogalski
################################################################################
#### Global Settings
#Name of the FIS job metadata directory
FIS_JOB_METADATA_DIR<-".fis"

#### FIS Job class


validity.FISJob <- function(object)
{
    stopifnot(is.character(object@executionType))
    stopifnot(is.character(object@workingDirectory))
    stopifnot(is.character(object@executionFile))
    return(TRUE)
}

################################################################################
#' FIS Job S4 Class Definition.
#'
#' Objects of this class represent a FIS job
#'
#' @slot id of a job
#' @slot executionType type of execution
#' @slot commandParameters command-line parameters for the third-party tool
#' @slot workingDirectory job working directory
#' @slot executionFile file that should be executed.
#' @slot extraInputFiles list of additional job input files being included
#' @slot submitTime time of submission
#' @slot status status of a job
#' @slot resultsIncludeRegex regular expression specifying additional input files
#' @slot resultsExcludeRegex list of additional job input files being included
#' @slot version - internal - entity version identifier
#'
#'
#' @author mrogalski
#'
#' @export
setClass(
    "FISJob",
    slots = c(
        id = "character",
        executionType = "character",
        commandParameters = "character",
        workingDirectory = "character",
        executionFile = "character",
        extraInputFiles = "list",
        submitTime = "character",
        status = "character",
        resultsIncludeRegex = "character",
        resultsExcludeRegex = "character",
        version = "numeric"
    ),
    validity = validity.FISJob
)

##############################################################
#' createFISJob
#'
#' Function to create an object of class FISJob with explicit list of attributes.
#'
#' @usage createFISJob(executionType = "NONMEM", executionFile = "PATH_TO_FILE", workingDirectory = "PATH_TO_WORKING_DIR")
#'
#' @param executionType Execution type - e.g. "NONMEM"
#' @param executionFile Execution file (e.g. MDL file path)
#' @param workingDirectory job's working directory 
#' @param commandParameters (Optional) command-line parameters for Third-Party Tool
#' @param extraInputFiles (Optional) Additional input files
#' @param resultsIncludeRegex (Optional) regular expression for file names that should be included in the files result-set
#' @param resultsExcludeRegex (Optional) regular expression for file names that should not be included in the files result-set
#'
#' @return An S4 Object of class "FISJob".
#'
#' @export
#' @docType methods
#' @rdname createFISJob
createFISJob <-
    function(executionType, executionFile, workingDirectory = NULL, commandParameters = "",
             extraInputFiles = list(), resultsIncludeRegex = "", resultsExcludeRegex = "") {
        fisJob <- new(
            "FISJob",
            executionType = executionType,
            executionFile = executionFile,
            workingDirectory = workingDirectory
        )
        if(!is.null(commandParameters)) fisJob@commandParameters <- commandParameters
        if(!is.null(extraInputFiles)) fisJob@extraInputFiles <- extraInputFiles
        if(!is.null(resultsIncludeRegex)) fisJob@resultsIncludeRegex <- resultsIncludeRegex
        if(!is.null(resultsExcludeRegex)) fisJob@resultsExcludeRegex <- resultsExcludeRegex
        return(fisJob)
    }

##############################################################
#' createFISJobFromNamedList
#'
#' Function to create an object of class FISJob from key-value list of properties, used to convert JSON to FISJob objects
#'
#' @usage createFISJobFromNamedList(namedList)
#'
#' @param namedList key-value list representing a FISJob
#'
#' @return An S4 Object of class "FISJob".
#'
#' @export
#' @docType methods
#' @rdname createFISJobFromNamedList
createFISJobFromNamedList <- function(namedList) {
    .precondition.checkArgument(!is.null(namedList),"namedList", "Must not be null." )
    
    fisJob <- new("FISJob")
    if(!is.null(namedList$executionType)) fisJob@executionType <- namedList$executionType
    if(!is.null(namedList$executionFile)) fisJob@executionFile <- namedList$executionFile
    if(!is.null(namedList$workingDirectory)) fisJob@workingDirectory <- namedList$workingDirectory
    if(!is.null(namedList$commandParameters)) fisJob@commandParameters <- namedList$commandParameters
    if(!is.null(namedList$extraInputFiles)) fisJob@extraInputFiles <- namedList$extraInputFiles
    if(!is.null(namedList$resultsIncludeRegex)) fisJob@resultsIncludeRegex <- namedList$resultsIncludeRegex
    if(!is.null(namedList$resultsExcludeRegex)) fisJob@resultsExcludeRegex <- namedList$resultsExcludeRegex
    if(!is.null(namedList$id)) fisJob@id <- namedList$id
    if(!is.null(namedList$submitTime)) fisJob@submitTime <- namedList$submitTime
    if(!is.null(namedList$status)) fisJob@status <- namedList$status
    if(!is.null(namedList$version)) fisJob@version <- namedList$version
    return(fisJob)
}

##############################################################
#' is.FISJob
#'
#' Determines if an object is of class "FISJob"
#'
#' @usage is.FISJob(object)
#'
#' @return TRUE or FALSE
#' @export
is.FISJob <- function(obj) {
    is(obj,"FISJob")
}

##############################################################
#' getStdOutFile
#'
#' Resolves location of a file containing standard output stream of third-party tool
#'
#' @usage getStdOutFile(object)
#'
#' @return path to the STD OUT file
setGeneric(
    name = "getStdOutFile",
    def = function(fisJob)
    {
        standardGeneric("getStdOutFile")
    }
)
setMethod("getStdOutFile", signature = signature("FISJob"),
          function(fisJob) {
              return(file.path(fisJob@workingDirectory, sprintf("%s/stdout",FIS_JOB_METADATA_DIR)))
          })

##############################################################
#' getStdErrFile
#'
#' Resolves location of a file containing standard error stream of third-party tool
#'
#' @usage getStdErrFile(object)
#'
#' @return path to the STD ERR file
setGeneric(
    name = "getStdErrFile",
    def = function(fisJob)
    {
        standardGeneric("getStdErrFile")
    }
)
setMethod("getStdErrFile", signature = signature("FISJob"),
          function(fisJob) {
              return(file.path(fisJob@workingDirectory, sprintf("%s/stderr",FIS_JOB_METADATA_DIR)))
          })

##############################################################
#' .fisJobToJSON
#'
#' Converts FISJob objects to JSON representation
#'
#' @usage .fisJobToJSON(object)
#'
#' @return json character vector
.fisJobToJSON <- function(fisJob) {
            .precondition.checkArgument(is.FISJob(fisJob),"fisJob", "Must be of type FISJob." )
              tmp <- .convertObjectToNamedList(fisJob)
              return(toJSON(.removeNullEntries(tmp)))
          }