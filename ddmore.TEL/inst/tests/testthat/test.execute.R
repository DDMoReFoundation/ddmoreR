#' Tests functions involved in task execution
library("ddmore")
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
DDMORE.setServer(mockServer)
context("Monitoring job")

test_that("DDMORE.performExecutionWorkflow with default import flags results in non-null result for successful job", {
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
    result = DDMORE.performExecutionWorkflow(
        submission, fisServer = mockServer, workflowSteps = telMock, mockParam = "mock"
    )
    
    #then
    expect_true(!is.null(result), info = "Result should not be null.")
    expect_equal(class(result)[[1]],"list", info  = "Result should be of type list.")
    expect_true("so" %in% names(result), info  = "so element should be set on the result list")
})

test_that("DDMORE.performExecutionWorkflow without import results in non-null result but empty 'so' attribute", {
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
    result = DDMORE.performExecutionWorkflow(
        submission, fisServer = mockServer, workflowSteps = telMock
    )
    
    #then
    expect_true(!is.null(result), info = "Result should not be null.")
    expect_equal(class(result)[[1]],"list", info  = "Result should be of type list.")
    expect_false("so" %in% names(result), info  = "so element should NOT be set on the result list")
})

test_that("DDMORE.performExecutionWorkflow results in error for failed job", {
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
        DDMORE.performExecutionWorkflow(
            submission, fisServer = mockServer, workflowSteps = telMock
        ), info = "Failed job should result in error."
    )
})

test_that("DDMORE.performExecutionWorkflow results in error if any step throws error.", {
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
        DDMORE.performExecutionWorkflow(
            submission, fisServer = mockServer, workflowSteps = telMock
        ), info = "Failed job should result in error."
    )
})


test_that("Final status of submission - fail on no or NULL submission's status", {
    submission <- list()
    expect_error(.setFinalSubmissionStatus(submission), info = "Submission with no status should result in error")
    
    submission$status <- NULL
    expect_error(.setFinalSubmissionStatus(submission), info = "Submission with NULL status should result in error")
})

test_that("Final status of submission is correctly set - no FISJob", {
    submission <- list()
    
    submission$status <- "Some random status"
    result <- .setFinalSubmissionStatus(submission)
    expect_equal(result$status, SUBMISSION_FAILED, "Submission with some random status should result in SUBMISSION_FAILED final status")
    
})

test_that("Final status of submission is correctly set - FISJob is present", {
    submission <- list()
    submission$fisJob <- createFISJobFromNamedList(list(executionType = "Mock-Execution", executionFile = "mock-file", id="MOCK_ID", status ='FAILED'))

    submission$status <- "Some random status"
    result <- .setFinalSubmissionStatus(submission)
    expect_equal(result$status, SUBMISSION_FAILED, "Submission with some random status and job with status Failed should result in SUBMISSION_FAILED final status")
    
    submission <- list()
    submission$fisJob <- createFISJobFromNamedList(list(executionType = "Mock-Execution", executionFile = "mock-file", id="MOCK_ID", status ='COMPLETED'))

    submission$status <- "Some random status"
    result <- .setFinalSubmissionStatus(submission)
    expect_equal(result$status, SUBMISSION_COMPLETED, "Submission with some random status and job with status Completed should result in SUBMISSION_COMPLETED final status")
})

test_that("DDMORE.verifySOStep  - results in unmodified submission status if SO doesn't contain error messages", {
    submission <- list()
    submission$fisJob <- createFISJobFromNamedList(list(executionType = "Mock-Execution", executionFile = "mock-file", id="MOCK_ID", status ='COMPLETED'))
    submission$so <- new (Class = "StandardOutputObject")
    submission$status <- "Some status"
    result <- DDMORE.verifySOStep(submission)
    expect_equal(result$status, "Some status", "Submission with SO that doesn't contain error messages, should result in unchanged submission status.")
    
})

test_that("DDMORE.verifySOStep  - results in SUBMISSION_COMPLETED_WITH_ERRORS if SO contains error messages", {
    submission <- list()
    submission$fisJob <- createFISJobFromNamedList(list(executionType = "Mock-Execution", executionFile = "mock-file", id="MOCK_ID", status ='COMPLETED'))
    submission$so <- new (Class = "StandardOutputObject")
    submission$status <- SUBMISSION_COMPLETED
    submission$so@TaskInformation@ErrorMessages <- list("First Error", "Second Error")
    result <- DDMORE.verifySOStep(submission)
    expect_equal(result$status, SUBMISSION_COMPLETED_WITH_ERRORS, "Submission with SO that contains error messages, should result in SUBMISSION_COMPLETED_WITH_ERRORS status.")
    
})