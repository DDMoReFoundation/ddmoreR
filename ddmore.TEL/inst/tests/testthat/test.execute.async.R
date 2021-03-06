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

context("Executing and monitoring a job manually.")

# Test data
testDataInputs <- system.file("tests/data/test.execute.async/inputs", package = "ddmore")
testDataOutputs <- system.file("tests/data/test.execute.async/outputs", package = "ddmore")
testInputFiles <- lapply(dir(testDataInputs), function(x) { file.path(testDataInputs,x) })
testResultFiles <- lapply(dir(testDataOutputs), function(x) { file.path(testDataOutputs,x) })


test_that("perform all steps involved in job execution manually.", {
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
    
    # copy result files to users working directory
    outputDirectory <- file.path(usersDir, "out")
    tmpJob <<- importJobResultFiles(tmpJob, targetDirectory=outputDirectory, fisServer=mockServer)
    
    # and import SO
    # note model is v0.2, block invalidly named Likelihood; so suppress warnings
    so <- suppressWarnings(
        LoadSOObject(file = file.path(outputDirectory, 
                paste0(file_path_sans_ext(basename(inputModelFile)), ".SO.xml"))))
    expect_true(is.SOObject(so), info  = "'so' was not of type StandardOutputObject")
    
})
