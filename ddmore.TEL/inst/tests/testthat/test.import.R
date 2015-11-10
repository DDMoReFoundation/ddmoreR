#' Tests function involved in FIS job's results import
rm(list = ls())
library("DDMoRe")
require("methods")
require("testthat")
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
context("Importing Job Results")

test_that("importJobResultFiles should import job result files", {
    fisJob <- createFISJobFromNamedList(list(executionType = "Mock-Execution", executionFile = "mock-file", id="MOCK_ID", status ='COMPLETED'))

    #preparing job working directory
    jobWd <- file.path(tempdir(), "importJobResultFiles.MOCK_JOB")
    dir.create(jobWd)
    dir.create(file.path(jobWd,".fis"))
    stdErr <- file.path(jobWd, ".fis/stderr.txt")
    writeLines("THIS IS STDERR FILE", stdErr)
    stdOut <- file.path(jobWd, ".fis/stdout.txt")
    writeLines("THIS IS STDOUT FILE", stdOut)
    mockResultFile <- file.path(jobWd, "some-result")
    writeLines("THIS IS RESULT FILE", mockResultFile)
    
    
    fisJob@workingDirectory<-jobWd
    
    targetDirectory <- file.path(tempdir(), "importJobResultFiles.MOCK_JOB_IMPORTED")
    # when
    result = importJobResultFiles(fisJob, targetDirectory)
    
    #then
    expect_true(!is.null(result), info = "Result should not be null.")
    expect_equal(class(result)[[1]],"FISJob", info  = "Result should be of type FISJob")
    expect_true(file.exists(file.path(targetDirectory,"stdout.txt")), info  = "STD OUT file should be created in the target directory.")
    expect_true(file.exists(file.path(targetDirectory,"stderr.txt")), info  = "STD ERR file should be created in the target directory.")
    expect_true(file.exists(file.path(targetDirectory,"some-result")), info  = "some-result file should be created in the target directory.")
    expect_false(file.exists(file.path(targetDirectory,".fis")), info  = ".fis directory should not be created in the target directory.")
})

