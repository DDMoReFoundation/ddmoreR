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

context("Server Integration")


test_that("TEL.pollStep should poll untill Job status is COMPLETED", {
    # Given
    pollCount <- 0
    pollMax <- 1
    setMethod("getJob", signature = signature("MockFISServer"),
              function(fisServer, jobID) {
                  if (pollCount < pollMax) {
                      pollCount <<- pollCount + 1
                      createFISJobFromNamedList(list(executionType = "Mock-Execution", executionFile = "mock-file", id = "MOCK_ID", status = 'RUNNING'))
                  } else {
                      createFISJobFromNamedList(list(executionType = "Mock-Execution", executionFile = "mock-file", id = "MOCK_ID", status = 'COMPLETED'))
                  }
              })
    submission <- list()
    submission$start <- date()
    submission$fisJob <- createFISJobFromNamedList(list(executionType = "Mock-Execution", executionFile = "mock-file", id =  "MOCK_ID", status = "NEW"))
    submission$status <- "Submitted"
    # when
    result = TEL.pollStep(submission, fisServer = mockServer)
    
    #then
    
    expect_true(!is.null(result), info = "Result should not be null.")
    expect_equal(class(result)[[1]],"list", info  = "Result should be of type list.")
    expect_equal(result$fisJob@status,"COMPLETED", info  = "FIS Job status property should be 'COMPLETED'.")
})

test_that("TEL.poll should poll untill Job status is FAILED", {
    # Given
    pollCount <- 0
    pollMax <- 1
    setMethod("getJob", signature = signature("MockFISServer"),
              function(fisServer, jobID) {
                  if (pollCount < pollMax) {
                      pollCount <<- pollCount + 1
                      createFISJobFromNamedList(list(executionType = "Mock-Execution", executionFile = "mock-file", id = "MOCK_ID", status = 'RUNNING'))
                  } else {
                      createFISJobFromNamedList(list(executionType = "Mock-Execution", executionFile = "mock-file", id = "MOCK_ID", status = 'FAILED'))
                  }
              })
    submission <- list()
    submission$start <- date()
    submission$fisJob <- createFISJobFromNamedList(list(executionType = "Mock-Execution", executionFile = "mock-file", id =  "MOCK_ID", status = "NEW"))
    submission$status <- "Submitted"
    # when
    result = TEL.pollStep(submission, fisServer = mockServer)
    
    #then
    
    expect_true(!is.null(result), info = "Result should not be null.")
    expect_equal(class(result)[[1]],"list", info = "Result should be of type list.")
    expect_equal(result$fisJob@status,"FAILED", info = "FIS Job status property should be 'FAILED'.")
})
