#' Tests functions involved in task execution

library("DDMoRe")
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

context("DDMORE.pollStep")


test_that("DDMORE.pollStep should poll untill Job status is COMPLETED", {
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
    result = DDMORE.pollStep(submission, fisServer = mockServer)
    
    #then
    
    expect_true(!is.null(result), info = "Result should not be null.")
    expect_equal(class(result)[[1]],"list", info  = "Result should be of type list.")
    expect_equal(result$fisJob@status,"COMPLETED", info  = "FIS Job status property should be 'COMPLETED'.")
    expect_false(result$status=="Failed", info = "Submission's 'status' should not be set to 'Failed'.")
})

test_that("DDMORE.pollStep should poll untill Job status is FAILED", {
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
    result = DDMORE.pollStep(submission, fisServer = mockServer)
    
    #then
    
    expect_true(!is.null(result), info = "Result should not be null.")
    expect_equal(class(result)[[1]],"list", info = "Result should be of type list.")
    expect_equal(result$fisJob@status,"FAILED", info = "FIS Job status property should be 'FAILED'.")
    expect_false(result$status=="Failed", info = "Submission's 'status' element should not be 'Failed'.")
})

context("DDMORE.printJobs")
# Given
setMethod("getJobs", signature = signature("MockFISServer"),
          function(fisServer) {
              list(createFISJobFromNamedList(list(executionType = "Mock-Execution", executionFile = "mock-file", id =  "MOCK_ID_NEW_1", status = "NEW")),
                   createFISJobFromNamedList(list(executionType = "Mock-Execution", executionFile = "mock-file", id =  "MOCK_ID_NEW_2", status = "NEW")),
                   createFISJobFromNamedList(list(executionType = "Mock-Execution", executionFile = "mock-file", id =  "MOCK_ID_RUNNING_1", workingDirectory = "/mock/working/dir", status = "RUNNING")),
                   createFISJobFromNamedList(list(executionType = "Mock-Execution", executionFile = "mock-file", id =  "MOCK_ID_RUNNING_2", extraInputFiles = list("mock-file1", "mock-file2"), status = "RUNNING")),
                   createFISJobFromNamedList(list(executionType = "Mock-Execution", executionFile = "mock-file", id =  "MOCK_ID_CANCELLING", status = "CANCELLING")),
                   createFISJobFromNamedList(list(executionType = "Mock-Execution", executionFile = "mock-file", id =  "MOCK_ID_CANCELLED", status = "CANCELLED")),
                   createFISJobFromNamedList(list(executionType = "Mock-Execution", executionFile = "mock-file", id =  "MOCK_ID_FAILED", status = "FAILED")),
                   createFISJobFromNamedList(list(executionType = "Mock-Execution", executionFile = "mock-file", id =  "MOCK_ID_COMPLETED", status = "COMPLETED"))
              )
          })

test_that("DDMORE.printJobs should print all if all statuses are specified", {
    # then
    allJobs <- DDMORE.printJobs()
    expect_equal(nrow(allJobs), 8, info = "Unexpected number of all jobs.")
})
test_that("DDMORE.printJobs should print NEW jobs if NEW status specified", {
    newJobs <- DDMORE.printJobs(statuses=c("NEW"))
    expect_equal(nrow(newJobs), 2, info = "Unexpected number of NEW jobs.")
})
test_that("DDMORE.printJobs should print RUNNING jobs if RUNNING status specified", {
    runningJobs <- DDMORE.printJobs( statuses=c("RUNNING"))
    expect_equal(nrow(runningJobs), 2, info = "Unexpected number of RUNNING jobs.")
})
test_that("DDMORE.printJobs should print COMPLETED jobs if COMPLETED status specified", {
    completedJobs <- DDMORE.printJobs(statuses=c("COMPLETED"))
    expect_equal(nrow(completedJobs), 1, info = "Unexpected number of COMPLETED jobs.")
})
test_that("DDMORE.printJobs should print FAILED jobs if FAILED status specified", {
    failedJobs <- DDMORE.printJobs(statuses=c("FAILED"))
    expect_equal(nrow(failedJobs), 1, info = "Unexpected number of FAILED jobs.")
})

context("DDMORE.cancelJob")
# Given
setMethod("cancelJob", signature = signature("MockFISServer"),
          function(fisServer, job) {
              job@status <- 'CANCELLING'
              job
          })

test_that("DDMORE.cancelJob should result in job with CANCELLING state", {
        fisJob <- createFISJobFromNamedList(list(executionType = "Mock-Execution", executionFile = "mock-file", id =  "MOCK_ID_RUNNING_1", workingDirectory = "/mock/working/dir", status = "RUNNING"))
        # then
        cancelledJob <- DDMORE.cancelJob(fisJob)
        expect_equal(cancelledJob@status, 'CANCELLING', info = "Invalid job status returned.")
})