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

# Tests functions involved in task execution

if (is(try(DDMORE.getServer(), silent = TRUE), class2 = "try-error")) {
    mockServer <- ddmore:::createMockFISServer(jobStatusPollingDelay=1)
    suppressWarnings(DDMORE.setServer(mockServer))
}

context("Monitoring job")

test_that("DDMORE.performExecutionWorkflow with default import flags results in non-null result for successful job", {
    completedJob <- createFISJobFromNamedList(
        list(executionType = "Mock-Execution", 
            executionFile = "mock-file", 
            id="MOCK_ID", 
            status = "COMPLETED"))
    telMock <- list(
        submitJobStep = function(submission, fisServer, ...) {
            ddmore:::log.debug("submitJob")
            submission$fisJob <- createFISJobFromNamedList(
                list(executionType = "Mock-Execution", 
                    executionFile = "mock-file", 
                    id="MOCK_ID", 
                    status ="NEW"))
            return(submission)
        },
        pollStep = function(submission, fisServer, ...) {
            ddmore:::log.debug("pollStep")
            submission$fisJob <- completedJob
            return(submission)
        },
        importFilesStep = function(submission, fisServer, ...) {
            ddmore:::log.debug("importFilesStep")
            if(list(...)$mockParam != "mock") {
                message("'mockParam' is not set")
                stop("Missing extra parameter 'mockParam'")
            }
            submission$status <- "importing"
            return(submission)
        },
        importSOStep = function(submission, fisServer, ...) {
            ddmore:::log.debug("importSOStep")
            submission$so <- "mockSO"
            return(submission)
        },
        clearUpStep = function(submission, fisServer, ...) {
            ddmore:::log.debug("clearUpStep")
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
    result <- suppressMessages(
        DDMORE.performExecutionWorkflow(
            submission, 
            fisServer = DDMORE.getServer(), 
            workflowSteps = telMock, 
            mockParam = "mock"))
    
    #then
    expect_true(!is.null(result), info = "Result should not be null.")
    expect_equal(class(result)[[1]],"list", info  = "Result should be of type list.")
    expect_true("so" %in% names(result), info  = "so element should be set on the result list")
})

test_that("DDMORE.performExecutionWorkflow without import results in non-null result but empty 'so' attribute", {
    completedJob <<- createFISJobFromNamedList(list(executionType = "Mock-Execution", executionFile = "mock-file", id="MOCK_ID", status ='COMPLETED'))
    telMock <- list(
        submitJobStep = function(submission, fisServer, ...) {
            ddmore:::log.debug("submitJob")
            submission$fisJob <- createFISJobFromNamedList(list(executionType = "Mock-Execution", executionFile = "mock-file", id="MOCK_ID", status ='NEW'))
            return(submission)
        },
        pollStep = function(submission, fisServer, ...) {
            ddmore:::log.debug("pollStep")
            submission$fisJob <- completedJob
            return(submission)
        },
        importFilesStep = function(submission, fisServer, ...) {
            ddmore:::log.debug("importFilesStep")
            submission$status <- "importing"
            return(submission)
        },
        clearUpStep = function(submission, fisServer, ...) {
            ddmore:::log.debug("clearUpStep")
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
        submission, fisServer = DDMORE.getServer(), workflowSteps = telMock
    )
    
    #then
    expect_true(!is.null(result), info = "Result should not be null.")
    expect_equal(class(result)[[1]],"list", info  = "Result should be of type list.")
    expect_false("so" %in% names(result), info  = "so element should NOT be set on the result list")
})

test_that("DDMORE.performExecutionWorkflow results in error for failed job", {
    telMock <- list(
        submitJobStep = function(submission, fisServer, ...) {
            ddmore:::log.debug("submitJob")
            submission$fisJob <- createFISJobFromNamedList(list(executionType = "Mock-Execution", executionFile = "mock-file", id="MOCK_ID", status ='NEW'))
            submission
        },
        pollStep = function(submission, fisServer, ...) {
            ddmore:::log.debug("pollStep")
            submission$fisJob <- createFISJobFromNamedList(list(executionType = "Mock-Execution", executionFile = "mock-file", id="MOCK_ID", status ='FAILED'))
            submission
        },
        importFilesStep = function(submission, fisServer, ...) {
            ddmore:::log.debug("importFilesStep")
            submission$status <- "importing"
            submission
        },
        importSOStep = function(submission, fisServer, ...) {
            ddmore:::log.debug("importSOStep")
            submission$so <- new("StandardOutputObject")
            submission
        },
        clearUpStep = function(submission, fisServer, ...) {
            ddmore:::log.debug("clearUpStep")
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
            submission, fisServer = DDMORE.getServer(), workflowSteps = telMock), 
        regex = "Execution of model MOCK_MODEL failed", 
        info = "Failed job should result in error."
    )
})

test_that("DDMORE.performExecutionWorkflow results in error if any step throws error.", {
    telMock <- list(
        submitJobStep = function(submission, fisServer, ...) {
            ddmore:::log.debug("submitJob")
            submission$fisJob <- createFISJobFromNamedList(list(executionType = "Mock-Execution", executionFile = "mock-file", id="MOCK_ID", status ='NEW'))
            submission
        },
        pollStep = function(submission, fisServer, ...) {
            ddmore:::log.debug("pollStep")
            submission$fisJob <- createFISJobFromNamedList(list(executionType = "Mock-Execution", executionFile = "mock-file", id="MOCK_ID", status ='COMPLETED'))
            submission
        },
        importFilesStep = function(submission, fisServer, ...) {
            ddmore:::log.debug("importFilesStep")
            stop("Mock error")
        },
        importSOStep = function(submission, fisServer, ...) {
            ddmore:::log.debug("importSOStep")
            submission$so <- new("StandardOutputObject")
            submission
        },
        clearUpStep = function(submission, fisServer, ...) {
            ddmore:::log.debug("clearUpStep")
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
            submission, fisServer = DDMORE.getServer(), workflowSteps = telMock), 
        regex = "Execution of model MOCK_MODEL failed",
        info = "Failed job should result in error."
    )
})


test_that("Final status of submission - fail on no or NULL submission's status", {
    submission <- list()
    expect_error(ddmore:::.setFinalSubmissionStatus(submission), 
        regex = "Illegal Argument submission.status. Is required and should be not.null.",
        info = "Submission with no status should result in error")
    
    submission$status <- NULL
    expect_error(ddmore:::.setFinalSubmissionStatus(submission), 
        regex = "Illegal Argument submission.status. Is required and should be not.null.",
        info = "Submission with NULL status should result in error")
})

test_that("Final status of submission is correctly set - no FISJob", {
    submission <- list()
    
    submission$status <- "Some random status"
    result <- ddmore:::.setFinalSubmissionStatus(submission)
    expect_equal(result$status, ddmore:::SUBMISSION_FAILED, "Submission with some random status should result in SUBMISSION_FAILED final status")
    
})

test_that("Final status of submission is correctly set - FISJob is present", {
    submission <- list()
    submission$fisJob <- createFISJobFromNamedList(list(executionType = "Mock-Execution", executionFile = "mock-file", id="MOCK_ID", status ='FAILED'))

    submission$status <- "Some random status"
    result <- ddmore:::.setFinalSubmissionStatus(submission)
    expect_equal(result$status, ddmore:::SUBMISSION_FAILED, "Submission with some random status and job with status Failed should result in SUBMISSION_FAILED final status")
    
    submission <- list()
    submission$fisJob <- createFISJobFromNamedList(list(executionType = "Mock-Execution", executionFile = "mock-file", id="MOCK_ID", status ='COMPLETED'))

    submission$status <- "Some random status"
    result <- ddmore:::.setFinalSubmissionStatus(submission)
    expect_equal(result$status, ddmore:::SUBMISSION_COMPLETED, "Submission with some random status and job with status Completed should result in SUBMISSION_COMPLETED final status")
})

test_that("ddmore:::DDMORE.verifySOStep  - results in unmodified submission status if SO doesn't contain error messages", {
    submission <- list()
    submission$fisJob <- createFISJobFromNamedList(list(executionType = "Mock-Execution", executionFile = "mock-file", id="MOCK_ID", status ='COMPLETED'))
    submission$so <- new (Class = "StandardOutputObject")
    submission$status <- "Some status"
    result <- ddmore:::DDMORE.verifySOStep(submission)
    expect_equal(result$status, "Some status", "Submission with SO that doesn't contain error messages, should result in unchanged submission status.")
    
})

test_that("ddmore:::DDMORE.verifySOStep  - results in SUBMISSION_COMPLETED_WITH_ERRORS if SO contains error messages", {
    submission <- list()
    submission$fisJob <- createFISJobFromNamedList(list(executionType = "Mock-Execution", executionFile = "mock-file", id="MOCK_ID", status ='COMPLETED'))
    submission$so <- new (Class = "StandardOutputObject")
    submission$status <- ddmore:::SUBMISSION_COMPLETED
    submission$so@TaskInformation@ErrorMessages <- list("First Error", "Second Error")
    result <- ddmore:::DDMORE.verifySOStep(submission)
    expect_equal(result$status, ddmore:::SUBMISSION_COMPLETED_WITH_ERRORS, "Submission with SO that contains error messages, should result in SUBMISSION_COMPLETED_WITH_ERRORS status.")
    
})
