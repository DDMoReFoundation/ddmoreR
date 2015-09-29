#' Tests functions involved in task execution
library("DDMoRe.TEL")
require("methods")
require("testthat")
rm(list = ls())
setClass(
    "MockFISServer",
    contains = "FISServer"
)
createMockFISServer <-
    function(url = "http://localhost:9010", operationalUrl = "http://localhost:9011",
             startupScript = "MOCK",
             jobStatusPollingDelay = 20, startupPollingMax = 60, startupPollingDelay = 1) {
        new(
            "MockFISServer",
            url = url,
            operationalUrl = operationalUrl,
            startupScript = startupScript,
            jobStatusPollingDelay = jobStatusPollingDelay,
            startupPollingMax = startupPollingMax,
            startupPollingDelay = startupPollingDelay
        )
    }


mockServer<- createMockFISServer(jobStatusPollingDelay=1)
TEL.setServer(mockServer)
context("Monitoring job")

test_that("TEL.performExecutionWorkflow with default import flags results in non-null result for successful job", {
    completedJob <- createFISJobFromNamedList(list(executionType = "Mock-Execution", executionFile = "mock-file", id="MOCK_ID", status ='COMPLETED'))
    telMock <- list(
        submitJobStep = function(submission, fisServer, ...) {
            log.debug("submitJob")
            submission$fisJob <- createFISJobFromNamedList(list(executionType = "Mock-Execution", executionFile = "mock-file", id="MOCK_ID", status ='NEW'))
            return(submission)
        },
        pollStep = function(submission, fisServer, ...) {
            log.debug("pollStep")
            submission$fisJob <- completedJob
            return(submission)
        },
        importFilesStep = function(submission, fisServer, ...) {
            log.debug("importFilesStep")
            if(list(...)$mockParam != "mock") {
                message("'mockParam' is not set")
                stop("Missing extra parameter 'mockParam'")
            }
            submission$status <- "importing"
            return(submission)
        },
        importSOStep = function(submission, fisServer, ...) {
            log.debug("importSOStep")
            submission$so <- "mockSO"
            return(submission)
        },
        clearUpStep = function(submission, fisServer, ...) {
            log.debug("clearUpStep")
            return(submission)
        }
    )
    
    submission <- list()
    submission$start <- date()
    submission$status <- "New"
    submission$parameters <- list()
    submission$parameters$modelFile <- "MOCK_MODEL"
    submission$parameters$workingDirectory <- "MOCK_WORKING_DIR"
    # when
    result = TEL.performExecutionWorkflow(
        submission, fisServer = mockServer, workflowSteps = telMock, mockParam = "mock"
    )
    
    #then
    expect_true(!is.null(result), info = "Result should not be null.")
    expect_equal(class(result)[[1]],"list", info  = "Result should be of type list.")
    expect_true("so" %in% names(result), info  = "so element should be set on the result list")
})

test_that("TEL.performExecutionWorkflow without import results in non-null result but empty 'so' attribute", {
    completedJob <<- createFISJobFromNamedList(list(executionType = "Mock-Execution", executionFile = "mock-file", id="MOCK_ID", status ='COMPLETED'))
    telMock <- list(
        submitJobStep = function(submission, fisServer, ...) {
            log.debug("submitJob")
            submission$fisJob <- createFISJobFromNamedList(list(executionType = "Mock-Execution", executionFile = "mock-file", id="MOCK_ID", status ='NEW'))
            return(submission)
        },
        pollStep = function(submission, fisServer, ...) {
            log.debug("pollStep")
            submission$fisJob <- completedJob
            return(submission)
        },
        importFilesStep = function(submission, fisServer, ...) {
            log.debug("importFilesStep")
            submission$status <- "importing"
            return(submission)
        },
        clearUpStep = function(submission, fisServer, ...) {
            log.debug("clearUpStep")
            return(submission)
        }
    )
    
    submission <- list()
    submission$start <- date()
    submission$status <- "New"
    submission$parameters <- list()
    submission$parameters$modelFile <- "MOCK_MODEL"
    submission$parameters$workingDirectory <- "MOCK_WORKING_DIR"
    # when
    result = TEL.performExecutionWorkflow(
        submission, fisServer = mockServer, workflowSteps = telMock
    )
    
    #then
    expect_true(!is.null(result), info = "Result should not be null.")
    expect_equal(class(result)[[1]],"list", info  = "Result should be of type list.")
    expect_false("so" %in% names(result), info  = "so element should NOT be set on the result list")
})

test_that("TEL.performExecutionWorkflow results in error for failed job", {
    telMock <- list(
        submitJobStep = function(submission, fisServer, ...) {
            log.debug("submitJob")
            submission$fisJob <- createFISJobFromNamedList(list(executionType = "Mock-Execution", executionFile = "mock-file", id="MOCK_ID", status ='NEW'))
            submission
        },
        pollStep = function(submission, fisServer, ...) {
            log.debug("pollStep")
            submission$fisJob <- createFISJobFromNamedList(list(executionType = "Mock-Execution", executionFile = "mock-file", id="MOCK_ID", status ='FAILED'))
            submission
        },
        importFilesStep = function(submission, fisServer, ...) {
            log.debug("importFilesStep")
            submission$status <- "importing"
            submission
        },
        importSOStep = function(submission, fisServer, ...) {
            log.debug("importSOStep")
            submission$so <- new("StandardOutputObject")
            submission
        },
        clearUpStep = function(submission, fisServer, ...) {
            log.debug("clearUpStep")
            submission
        }
    )
    
    submission <- list()
    submission$start <- date()
    submission$status <- "New"
    submission$parameters <- list()
    submission$parameters$modelFile <- "MOCK_MODEL"
    submission$parameters$workingDirectory <- "MOCK_WORKING_DIR"
    #then
    expect_error(
        TEL.performExecutionWorkflow(
            submission, fisServer = mockServer, workflowSteps = telMock
        ), info = "Failed job should result in error."
    )
})

test_that("TEL.performExecutionWorkflow results in error if any step throws error.", {
    telMock <- list(
        submitJobStep = function(submission, fisServer, ...) {
            log.debug("submitJob")
            submission$fisJob <- createFISJobFromNamedList(list(executionType = "Mock-Execution", executionFile = "mock-file", id="MOCK_ID", status ='NEW'))
            submission
        },
        pollStep = function(submission, fisServer, ...) {
            log.debug("pollStep")
            submission$fisJob <- createFISJobFromNamedList(list(executionType = "Mock-Execution", executionFile = "mock-file", id="MOCK_ID", status ='COMPLETED'))
            submission
        },
        importFilesStep = function(submission, fisServer, ...) {
            log.debug("importFilesStep")
            stop("Mock error")
        },
        importSOStep = function(submission, fisServer, ...) {
            log.debug("importSOStep")
            submission$so <- new("StandardOutputObject")
            submission
        },
        clearUpStep = function(submission, fisServer, ...) {
            log.debug("clearUpStep")
            submission
        }
    )
    
    submission <- list()
    submission$start <- date()
    submission$status <- "New"
    submission$parameters <- list()
    submission$parameters$modelFile <- "MOCK_MODEL"
    submission$parameters$workingDirectory <- "MOCK_WORKING_DIR"

    #then
    expect_error(
        TEL.performExecutionWorkflow(
            submission, fisServer = mockServer, workflowSteps = telMock
        ), info = "Failed job should result in error."
    )
})

test_that("Final status of submission is correctly set - no FISJob", {
    submission <- list()
    result <- .setFinalSubmissionStatus(submission)
    expect_equal(result$status, SUBMISSION_FAILED, "Submission with no status should result in SUBMISSION_FAILED final status")
    
    submission$status <- NULL
    result <- .setFinalSubmissionStatus(submission)
    expect_equal(result$status, SUBMISSION_FAILED, "Submission with NULL should result in SUBMISSION_FAILED final status")
    
    submission$status <- "Some random status"
    result <- .setFinalSubmissionStatus(submission)
    expect_equal(result$status, SUBMISSION_FAILED, "Submission with some randome status should result in SUBMISSION_FAILED final status")
    
})

test_that("Final status of submission is correctly set - FISJob is present", {
    submission <- list()
    submission$fisJob <- createFISJobFromNamedList(list(executionType = "Mock-Execution", executionFile = "mock-file", id="MOCK_ID", status ='FAILED'))
    result <- .setFinalSubmissionStatus(submission)
    expect_equal(result$status, SUBMISSION_FAILED, "Submission with no status and job with status Failed should result in SUBMISSION_FAILED final status")
    
    submission$status <- NULL
    result <- .setFinalSubmissionStatus(submission)
    expect_equal(result$status, SUBMISSION_FAILED, "Submission with NULL and job with status Failed  should result in SUBMISSION_FAILED final status")
    
    submission$status <- "Some random status"
    result <- .setFinalSubmissionStatus(submission)
    expect_equal(result$status, SUBMISSION_FAILED, "Submission with some randome status and job with status Failed should result in SUBMISSION_FAILED final status")
    
    submission <- list()
    submission$fisJob <- createFISJobFromNamedList(list(executionType = "Mock-Execution", executionFile = "mock-file", id="MOCK_ID", status ='COMPLETED'))
    result <- .setFinalSubmissionStatus(submission)
    expect_equal(result$status, SUBMISSION_COMPLETED, "Submission with no status and job with status Completed should result in SUBMISSION_COMPLETED final status")
    
    submission$status <- NULL
    result <- .setFinalSubmissionStatus(submission)
    expect_equal(result$status, SUBMISSION_COMPLETED, "Submission with NULL and job with status Completed should result in SUBMISSION_COMPLETED final status")
    
    submission$status <- "Some random status"
    result <- .setFinalSubmissionStatus(submission)
    expect_equal(result$status, SUBMISSION_COMPLETED, "Submission with some randome status and job with status Completed should result in SUBMISSION_COMPLETED final status")
})