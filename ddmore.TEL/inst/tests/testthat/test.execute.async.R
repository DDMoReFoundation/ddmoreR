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
context("Executing and monitoring a job manually.")



# Test data
testDataInputs <- system.file("tests/data/test.execute.async/inputs", package = "ddmore")
testDataOutputs <- system.file("tests/data/test.execute.async/outputs", package = "ddmore")
testInputFiles <- lapply(dir(testDataInputs), function(x) { file.path(testDataInputs,x) })
testResultFiles <- lapply(dir(testDataOutputs), function(x) { file.path(testDataOutputs,x) })


test_that("I can perform all steps involved in job execution manually.", {
    pollCount <- 1
    statuses = list("NEW", "RUNNING", "COMPLETED")
    
    setMethod("getJob", signature = signature("MockFISServer"),
              function(fisServer, jobID) {
                  #we are using a global job object here for brevity of the test
                  tmpJob@status = statuses[[pollCount]]
                  pollCount <<- pollCount + 1
                  if(tmpJob@status=='COMPLETED') {
                      # mocking execution
                      file.copy(testResultFiles, tmpJob@workingDirectory)
                      # mocking the result file name changes made by FIS
                      # FIXME - FIS should not change this one, now FIS just changes the extension, once PHEX is supported by converters it may not be the case
                      # FIS should use different attribute for communication between internal FIS/MIF components, not one that was submitted by FIS client
                      # FIXME - DDMORE R package submits absolute path to model file, not relative to the working dir. The API must be more explicit about it
                      tmpJob@executionFile <- paste0(file_path_sans_ext(basename(tmpJob@executionFile)), ".xml")
                  }
                  tmpJob
              })
    
    setMethod("submitJob", signature = signature("MockFISServer"),
              function(fisServer, job) {
                  if (is.null(job)) {
                      stop("Illegal Argument: job list can't be null")
                  }
                  job@status <- 'New'
                  job@id <- "MOCK_ID"
                  job
              })
    
    # Root of test directory
    testDirRoot <- tempfile("ddmore-test.execute.async.",tempdir())
    # Directory where fis job is executed
    fisJobDir <- tempfile("FIS-job.",testDirRoot)
    # Where user's files reside
    usersDir <- tempfile("usersDir",testDirRoot)
    
    #creating directories
    dir.create(testDirRoot)
    dir.create(usersDir)
    
    # Given some input model and data in user's working directory
    lapply(testInputFiles, function(x) { file.copy(x, usersDir) })
    inputModelFile <- file.path(usersDir,"Friberg_PANSS.mdl")
    
    # when I submit a job
    dir.create(fisJobDir) # FIXME Legacy, FIS expects the client to create a FIS job directory
    newJob <- createFISJob(executionType = "NONMEM", 
                              executionFile = inputModelFile, 
                              workingDirectory = fisJobDir, 
                              extraInputFiles = list(), 
                              commandParameters = "mock command line parameters")
    tmpJob <<- DDMORE.submitJob(newJob)
    
    expect_true(tmpJob@id == 'MOCK_ID', info = "Job id was incorrect.")
    
    # then I can monitor it
    while(!(tmpJob@status %in% c("COMPLETED"))) {
        tmpJob <<- DDMORE.getJob(tmpJob@id)
    }

    expect_true(tmpJob@status == 'COMPLETED', info = "FIS Job was not in 'COMPLETED' state.")
    
    # I can copy result files to users working directory
    outputDirectory <- file.path(usersDir, "out")
    tmpJob <<- importJobResultFiles(tmpJob, targetDirectory=outputDirectory, fisServer=mockServer)
    
    # and I can import SO
    so <- LoadSOObject(file.path(outputDirectory, paste0(file_path_sans_ext(basename(inputModelFile)), ".SO.xml")))
    expect_true(is.SOObject(so), info  = "'so' was not of type StandardOutputObject")
    
})
