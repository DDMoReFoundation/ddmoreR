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

# Tests function involved in FIS job's results import

if (is(try(DDMORE.getServer(), silent = TRUE), class2 = "try-error")) {
    mockServer <- ddmore:::createMockFISServer(jobStatusPollingDelay=1)
    suppressWarnings(DDMORE.setServer(mockServer))
}

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
    
    unlink(jobWd, recursive = TRUE)
})

