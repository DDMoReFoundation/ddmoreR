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
#' @import methods
#' @import XML
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
#' @param url
#' @param operationalUrl
#' @param startupScript
#' @param jobStatusPollingDelay
#' @param startupPollingMax
#' @param startupPollingDelay
#'
#' @return An S4 Object of class "FISServer".
#'
#' @export
#' @docType methods
#' @rdname createFISServer
createFISServer <-
    function(url = "http://localhost:9010", operationalUrl = "http://localhost:9011",
             startupScript = NULL,
             jobStatusPollingDelay = 20, startupPollingMax = 120, startupPollingDelay = 1) {
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
#' @return An S4 Object of class "FISServer".
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

################################################################################
#' MDLToPharmML
#'
#' Converts an MDL file to a PharmML file.
#' 
#' Internal method, users should use as.PharmML function.
#'
#' @param fisServer FISServer instance.
#' @param filePath absolute path to an MDL file that should be converted
#' 
#' @return a path to the result PharmML file, in the current working directory
#'
setGeneric(
    name = "MDLToPharmML",
    def = function(fisServer, filePath)
    {
        standardGeneric("MDLToPharmML")
    }
)
setMethod("MDLToPharmML", signature = signature("FISServer"),
          function(fisServer, filePath) {
              .precondition.checkArgument(!is.null(filePath), "filePath", sprintf("MDL file %s must be specified.",filePath))
              .precondition.checkArgument(file.exists(filePath), "filePath", sprintf("MDL file %s must exist.",filePath))
              body <- URLencode(paste0(
                  "filePath=", normalizePath(filePath, winslash="/"),
                  "&outputDir=", normalizePath(getwd())
              ))
              url <- sprintf('%s/convertmdl', fisServer@url)
              response <- .httpPost(url = url, body = body)
              
              if (response$header['status'] != 200) {
                  # in case of error, the body contains JSON representation of the exception
                  log.debug(response$body)
                  exception <- fromJSON(response$body)
                  stop(sprintf("Failed to convert MDL to PharmML. Error: %s.", exception$message))
              }
              return(response$body)
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
              response <- httpGET(jobsURL)
              log.debug(sprintf("Received from FIS Server: %s", response))
              jobs <- fromJSON(response)
              return(lapply(jobs, function(x) { createFISJobFromNamedList(x) }))
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
              response <- httpGET(jobsURL)
              log.debug(sprintf("Received from FIS Server: %s", response))
              return(createFISJobFromNamedList(fromJSON(response)))
          })


################################################################################
#' submitJob
#'
#' Gets a FIS job
#'
#' @param fisServer FISServer instance.
#' @param job FISJob instance
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
              .precondition.checkArgument(is.FISJob(job), "job", "FISJob instance required.")
              json <- .fisJobToJSON(job)
              submitURL <- sprintf('%s/jobs', fisServer@url)
              httpheader <-c(Accept = "application/json; charset=UTF-8",
                             "Content-Type" = "application/json")
              response <- .httpPost(url = submitURL, body=json, headers = httpheader )
              if(response$header['status']!=200) {
                  exception <- fromJSON(response$body)
                  stop(sprintf("Could not submit job. Error: %s", exception$message))
              }
              bodyAsList <- fromJSON(response$body)
              return(createFISJobFromNamedList(bodyAsList))
          })

################################################################################
#' cancelJob
#'
#' Cancels a FIS job
#'
#' @param fisServer FISServer instance.
#' @param job FISJob instance
#'
#' @export
setGeneric(
    name = "cancelJob",
    def = function(fisServer, job)
    {
        standardGeneric("cancelJob")
    }
)
setMethod("cancelJob", signature = signature("FISServer"),
          function(fisServer, job) {
              .precondition.checkArgument(is.FISJob(job), "job", "FISJob instance required.")
              .precondition.checkArgument(length(job@id)>0, "job@id", "FISJob must have id specified.")
              cancelURL <- sprintf('%s/cmd/jobs/cancel/%s', fisServer@url, job@id)
              
              response <- .httpPost(url = cancelURL, body="", headers=c(Accept = "application/json; charset=UTF-8"))
              
              if(response$header['status']!=200) {
                  exception <- fromJSON(response$body)
                  stop(sprintf("Could not cancel job %s. Error: %s", job@id, exception$message))
              }
              bodyAsList <- fromJSON(response$body)
              return(createFISJobFromNamedList(bodyAsList))
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
                      '%s/readmdl?filePath=%s', fisServer@url, normalizePath(filePath, winslash = "/")
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
