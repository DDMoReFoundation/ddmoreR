#' Tests functions involved in task execution

library("DDMoRe.TEL")
require("methods")
require("testthat")
rm(list = ls())

TEL.setJobPollingDelay(1)
context("Server Integration")


test_that("TEL.poll should poll untill Job status is COMPLETED", {
    # Given
    pollCount <- 0
    pollMax <- 1
    serverMock <- list(
        getJob = function(...) {
            if (pollCount < pollMax) {
                pollCount <<- pollCount + 1
                list(jobId = "MOCK_ID", status = 'RUNNING')
            } else {
                list(jobId = "MOCK_ID", status = 'COMPLETED')
            }
        }
    )
    
    submission <- list()
    submission$start <- date()
    submission$fisJob <- list()
    submission$fisJob$id <- "MOCK_ID"
    submission$fisJob$status <- "NEW"
    submission$status <- "Submitted"
    # when
    result = TEL.poll(submission, server = serverMock)
    
    #then
    
    expect_true(!is.null(result), info = "Result should not be null.")
    expect_equal(class(result)[[1]],"list", info  = "Result should be of type list.")
    expect_equal(result$fisJob$status,"COMPLETED", info  = "FIS Job status property should be 'COMPLETED'.")
})

test_that("TEL.poll should poll untill Job status is FAILED", {
    # Given
    pollCount <- 0
    pollMax <- 1
    serverMock <- list(
        getJob = function(...) {
            if (pollCount < pollMax) {
                pollCount <<- pollCount + 1
                list(jobId = "MOCK_ID", status = 'RUNNING')
            } else {
                list(jobId = "MOCK_ID", status = 'FAILED')
            }
        }
    )
    
    submission <- list()
    submission$start <- date()
    submission$fisJob <- list()
    submission$fisJob$id <- "MOCK_ID"
    submission$fisJob$status <- "NEW"
    submission$status <- "Submitted"
    # when
    result = TEL.poll(submission, server = serverMock)
    
    #then
    
    expect_true(!is.null(result), info = "Result should not be null.")
    expect_equal(class(result)[[1]],"list", info = "Result should be of type list.")
    expect_equal(result$fisJob$status,"FAILED", info = "FIS Job status property should be 'FAILED'.")
})
