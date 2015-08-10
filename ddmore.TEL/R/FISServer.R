#
# Author: mrogalski
################################################################################


#### FIS Server class


validity.FISServer <- function(object)
{
    stopifnot(is.character(object@url))
    stopifnot(is.character(object@operationalUrl))
    stopifnot(is.character(object@startupScript))
    stopifnot(is.numeric(object@jobStatusPollingDelay))
    stopifnot(is.numeric(object@startupPollingMax))
    stopifnot(is.numeric(object@startupPollingDelay))
    return(TRUE)
}

################################################################################
#' FIS Server S4 Class Definition.
#'
#' Objects of this class represent a FIS instance.
#'
#' @slot url a URL of FIS instance.
#' @slot operationalUrl a URL of FIS instance's operational HTTP endpoints.
#' @slot startupScript path to a startup script that should be used to startup fis
#' @slot jobStatusPollingDelay what should be delay between subsequent retrievals of job status. (in seconds)
#' @slot startupPollingMax how many times package should check if FIS has started up before reporting failure.
#' @slot startupPollingDelay what should be the delay between subsequent checks if FIS has started up. (in seconds)
#'
#'
#' @author mrogalski
#'
#' @export
setClass(
    "FISServer",
    slots = c(
        url = "character",
        operationalUrl = "character",
        startupScript = "character",
        jobStatusPollingDelay = "numeric",
        startupPollingMax = "numeric",
        startupPollingDelay = "numeric"
    ),
    validity = validity.FISServer
)

##############################################################
#' createFISServer
#'
#' Function to create an object of class FIServer with explic list of attributes.
#'
#' @usage createFISServer(url, operationalUrl, startupScript, jobStatusPollingDelay, myMdlObj, myTaskObj)
#'
#' @param url An object of class dataObj
#' @param operationalUrl An object of class parObj
#' @param startupScript An object of class mdlObj
#' @param taskObj An object of class taskObj
#' @param mogName (Optional) The name to assign to the new mogObj
#'
#' @return An S4 Object of class "fisServer".
#'
#' @export
#' @docType methods
#' @rdname createFISServer
createFISServer <-
    function(url = "http://localhost:9010", operationalUrl = "http://localhost:9011",
             startupScript = NULL,
             jobStatusPollingDelay = 20, startupPollingMax = 60, startupPollingDelay = 1) {
        new(
            "FISServer",
            url = url,
            operationalUrl = operationalUrl,
            startupScript = startupScript,
            jobStatusPollingDelay = jobStatusPollingDelay,
            startupPollingMax = startupPollingMax,
            startupPollingDelay = startupPollingDelay
        )
    }

##############################################################
#' createFISServerFromProperties
#'
#' Function to create an object of class FIServer from properties read from an external JSON-formatted file
#'
#' @usage createFISServer(propertiesFile)
#'
#' @param propertiesFile Path to a properties file in json format
#'
#' @return An S4 Object of class "fisServer".
#'
#' @export
#' @docType methods
#' @rdname createFISServerFromProperties
createFISServerFromProperties <- function(propertiesFile) {
    .precondition.checkArgument(!is.null(propertiesFile),"propertiesFile", "Properties file must be set." )
    .precondition.checkArgument(file.exists(propertiesFile),"propertiesFile", paste("File", propertiesFile,"does not exist." ))
    
    props <- rjson:::fromJSON(file = propertiesFile)
    createFISServer(url = props$url, 
                    operationalUrl = props$operationalUrl,
                    startupScript = props$startupScript,
                    jobStatusPollingDelay = props$jobStatusPollingDelay, 
                    startupPollingMax = props$startupPollingMax, 
                    startupPollingDelay = props$startupPollingDelay)
}

##############################################################
#' is.FISServer
#'
#' Determines if an object is of class "FISServer"
#'
#' @usage is.FISServer(object)
#'
#' @return TRUE or FALSE
#' @export
is.FISServer <- function(obj) {
    is(obj,"FISServer")
}


setGeneric(
    name = "getWriteMDLUrl",
    def = function(fisServer)
    {
        standardGeneric("getWriteMDLUrl")
    }
)
setMethod("getWriteMDLUrl", signature = signature("FISServer"),
          function(fisServer) {
              return(sprintf('%s/writemdl', fisServer@url))
          })

setGeneric(
    name = "getMDLToPharmML",
    def = function(fisServer)
    {
        standardGeneric("getMDLToPharmML")
    }
)
setMethod("getMDLToPharmML", signature = signature("FISServer"),
          function(fisServer) {
              return(sprintf('%s/convertmdl', fisServer@url))
          })

################################################################################
#' getJobs
#'
#' Gets all jobs
#'
#' @param fisServer FISServer instance.
#'
#' @export
#'
setGeneric(
    name = "getJobs",
    def = function(fisServer)
    {
        standardGeneric("getJobs")
    }
)
setMethod("getJobs", signature = signature("FISServer"),
          function(fisServer) {
              jobsURL <- sprintf('%s/jobs', fisServer@url)
              jobs <- fromJSON(httpGET(jobsURL))
              return(jobs)
          })


################################################################################
#' getJob
#'
#' Gets a FIS job
#'
#' @param fisServer FISServer instance.
#' @param jobID id of a FIS job
#'
#' @export
setGeneric(
    name = "getJob",
    def = function(fisServer, jobID)
    {
        standardGeneric("getJob")
    }
)
setMethod("getJob", signature = signature("FISServer"),
          function(fisServer, jobID) {
              if (is.null(jobID)) {
                  stop("Illegal Argument: jobID can't be null")
              }
              jobsURL = sprintf('%s/jobs/%s', fisServer@url, jobID)
              
              return(fromJSON(httpGET(jobsURL)))
          })


################################################################################
#' submitJob
#'
#' Gets a FIS job
#'
#' @param fisServer FISServer instance.
#' @param job named list representing job to be sent off to FIS
#'
#' @export
setGeneric(
    name = "submitJob",
    def = function(fisServer, job)
    {
        standardGeneric("submitJob")
    }
)
setMethod("submitJob", signature = signature("FISServer"),
          function(fisServer, job) {
              if (is.null(job)) {
                  stop("Illegal Argument: job list can't be null")
              }
              json <- toJSON(job)
              h <- basicTextGatherer()
              submitURL <- sprintf('%s/jobs', fisServer@url)
              httpheader <-
                  c(Accept = "application/json; charset=UTF-8",
                    "Content-Type" = "application/json")
              curlRet <-
                  RCurl:::curlPerform(
                      url = submitURL, postfields = json, httpheader = httpheader, writefunction =
                          h$update
                  )
              response <- fromJSON(h$value())
              return(response)
          })

################################################################################
#' readMDL
#'
#' Reads MDL into JSON
#'
#' @param fisServer FISServer instance.
#' @param filePath absolute path to an MDL file that should be converted
#'
#' @export
setGeneric(
    name = "readMDL",
    def = function(fisServer, filePath)
    {
        standardGeneric("readMDL")
    }
)
setMethod("readMDL", signature = signature("FISServer"),
          function(fisServer, filePath) {
              if (is.null(filePath)) {
                  stop("Illegal Argument: filePath can't be null")
              }
              # Call parser and read in the JSON data:
              cmd <-
                  URLencode(sprintf(
                      '%s/readmdl?fileName=%s', fisServer@url, normalizePath(filePath, winslash = "/")
                  ))
              
              json <- httpGET(cmd)
              return(json)
          })

################################################################################
#' health
#'
#' Queries the health of FIS and its dependent servers.
#'
#' @param fisServer FISServer instance.
#'
#' @return a named list detailing the status of FIS and its dependent services, i.e.:
#' $status
#' [1] "UP"
#' $ctsHealth
#' $ctsHealth$status
#' [1] "UP"
#' $mifHealth
#' $mifHealth$status
#' [1] "UP"
#' $db
#' $db$status
#' [1] "UP"
#' $db$database
#' [1] "H2"
#' $db$hello
#' [1] 1
#'
#' @export
#'
setGeneric(
    name = "health",
    def = function(fisServer)
    {
        standardGeneric("health")
    }
)
setMethod("health", signature = signature("FISServer"),
          function (fisServer) {
              healthcheckUrl <- sprintf('%s/health', fisServer@operationalUrl)
              
              # Some explanation of this HTTP call and response handling.
              # We use getURL() rather than postForm() to ensure that a non-OK HTTP Status (i.e. the 503 Service
              # Unavailable returned from FIS if a component e.g. CTS is down) doesn't throw an error and we can
              # still access the JSON response data.
              # In fact, the HTTP status code doesn't actually seem to be available using getURL() - it is with
              # postForm() - but this is probably ok since I don't think we need it if we have the JSON statuses.
              tryCatch({
                  response <- RCurl:::getURL(healthcheckUrl)
                  return(fromJSON(response))
              }, error = function(e) {
                  return(list(status = 'DOWN', error = conditionMessage(e)))
              })
              
          })

################################################################################
#' shutdown
#'
#' Requests FIS to shut down
#'
#' @param fisServer FISServer instance.
#'
#' @return TRUE if shut down went ok, false otherwise
#'
#' @export
#'
setGeneric(
    name = "shutdown",
    def = function(fisServer)
    {
        standardGeneric("shutdown")
    }
)
setMethod("shutdown", signature = signature("FISServer"),
          function (fisServer) {
              shutdownURL <- sprintf('%s/shutdown', fisServer@url)
              ret <- RCurl:::postForm(shutdownURL, style = "HTTPPOST", shutdown =
                                         "yes")
              return(ret[1] == "OK")
          })