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

test_that("TEL.monitor with default import flags results in non-null result for successful job", {
    telMock <- list(
        poll = function(...) {
            submission$fisJob <- list(id="MOCK_ID", status ='COMPLETED')
            submission
        },
        submitJob = function(...) {
            message("submitJob")
        },
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
    submission$fisJob <- list()
    submission$fisJob$id <- "MOCK_ID"
    submission$status <- "Submitted"
    # when
    result = TEL.monitor(
        submission, importDirectory = "mock/path", fisServer = mockServer, tel = telMock
    )
    
    #then
    expect_true(!is.null(result), info = "Result should not be null.")
    expect_equal(class(result)[[1]], "StandardOutputObject", info  = "Result should be of type Standard Output Object.")
})

test_that("TEL.monitor with disabled import results in non-null result for successful job", {
    telMock <- list(
        poll = function(...) {
            submission$fisJob <- list(id="MOCK_ID", status ='COMPLETED')
            submission
        },
        submitJob = function(...) {
            message("submitJob")
        },
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
    submission$fisJob <- list()
    submission$fisJob$id <- "MOCK_ID"
    submission$status <- "Submitted"
    # when
    result = TEL.monitor(
        submission, importDirectory = "mock/path", importSO = FALSE, fisServer = mockServer, tel = telMock
    )
    
    #then
    expect_true(!is.null(result), info = "Result should not be null.")
    expect_equal(class(result)[[1]],"list", info  = "Result should be of type list.")
})

test_that("TEL.monitor results in error for failed job", {
    telMock <- list(
        poll = function(...) {
            submission$fisJob <- list(id="MOCK_ID", status ='FAILED')
            submission
        },
        submitJob = function(...) {
            message("submitJob")
        },
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
    submission$fisJob <- list()
    submission$fisJob$id <- "MOCK_ID"
    submission$status <- "Submitted"
    
    #then
    expect_error(
        TEL.monitor(
            submission, importDirectory = "mock/path", fisServer = mockServer, tel = telMock
        ), info = "Failed job should result in error."
    )
})
