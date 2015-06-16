#' Tests functions involved in task execution

library("DDMoRe.TEL")
require("methods")
require("testthat")

rm(list = ls())

context("Monitoring job")

test_that("TEL.monitor with default import flags results in non-null result for successful job", {
    # Given
    serverMock <- list(
        poll = function(...) {
            submission$fisJobStatus <- 'COMPLETED'
            submission
        },
        submitJob = function(...) {
            message("submitJob")
        },
        getJob = function(...) {
            list(jobId = "MOCK_ID", status = 'COMPLETED')
        }
    )
    
    telMock <- list(
        importFiles = function(...) {
            submission$resultsDir <- "mock/result/dir"
            submission
        },
        importSO = function(...) {
            new("StandardOutputObject")
        }
    )
    
    submission <- list()
    submission$start <- date()
    submission$requestID <- "MOCK_ID"
    submission$status <- "Submitted"
    # when
    result = TEL.monitor(
        submission, importDirectory = "mock/path", server = serverMock, tel = telMock
    )
    
    #then
    expect_true(!is.null(result), info = "Result should not be null.")
    expect_equal(class(result)[[1]], "StandardOutputObject", info  = "Result should be of type Standard Output Object.")
})

test_that("TEL.monitor with disabled import results in non-null result for successful job", {
    # Given
    serverMock <- list(
        poll = function(...) {
            submission$fisJobStatus <- 'COMPLETED'
            submission
        },
        submitJob = function(...) {
            message("submitJob")
        },
        getJob = function(...) {
            message("getJob")
            list(jobId = "MOCK_ID", status = 'COMPLETED')
        },
        getJobs = function(...) {
            message("getJobs")
        }
    )
    
    telMock <- list(
        importFiles = function(...) {
            submission$resultsDir <- "mock/result/dir"
            submission
        },
        importSO = function(...) {
            new("StandardOutputObject")
        }
    )
    
    submission <- list()
    submission$start <- date()
    submission$requestID <- "MOCK_ID"
    submission$status <- "Submitted"
    # when
    result = TEL.monitor(
        submission, importDirectory = "mock/path", importSO = FALSE, server = serverMock, tel = telMock
    )
    
    #then
    expect_true(!is.null(result), info = "Result should not be null.")
    expect_equal(class(result)[[1]],"list", info  = "Result should be of type list.")
})

test_that("TEL.monitor results in error for failed job", {
    # Given
    serverMock <- list(
        poll = function(...) {
            submission$fisJobStatus <- 'FAILED'
            submission
        },
        submitJob = function(...) {
            message("submitJob")
        },
        getJob = function(...) {
            message("getJob")
            list(jobId = "MOCK_ID", status = 'COMPLETED')
        },
        getJobs = function(...) {
            message("getJobs")
        }
    )
    
    telMock <- list(
        importFiles = function(...) {
            message("TEL.importFiles")
        },
        importSO = function(...) {
            message("importSO")
            "SO"
        }
    )
    
    submission <- list()
    submission$start <- date()
    submission$requestID <- "MOCK_ID"
    submission$status <- "Submitted"
    
    #then
    expect_error(
        TEL.monitor(
            submission, importDirectory = "mock/path", server = serverMock, tel = telMock
        ), info = "Failed job should result in error."
    )
})
