library("DDMoRe")
library("XML")
require("methods")
require("testthat")
rm(list=ls())
context("FIS Job's createFISJob.")

test_that("createFISJob creates a valid object for minimal set of required attributes", {
    
    instance <- createFISJob(executionType="MOCK_TYPE", executionFile="MOCK_FILE", workingDirectory = "MOCK_WD")
    
    expect_true(is.FISJob(instance), 
        info = "Instance was not of type FISJob"
    )
    
    expect_true(instance@executionType == "MOCK_TYPE", info = "executionType was incorrect.")
    expect_true(instance@executionFile == "MOCK_FILE", info = "executionFile was incorrect")
    expect_true(instance@workingDirectory == "MOCK_WD", info = "workingDirectory was incorrect")
    
})

test_that("FISJob can be converted to JSON", {
    
    instance <- createFISJob(executionType="MOCK_TYPE", executionFile="MOCK_FILE", workingDirectory = "MOCK_WD")
    
    expect_true(is.FISJob(instance), 
                info = "Instance was not of type FISJob"
    )
    json <- .fisJobToJSON(instance)
    expect_equal(json, "{\"executionType\":\"MOCK_TYPE\",\"commandParameters\":\"\",\"workingDirectory\":\"MOCK_WD\",\"executionFile\":\"MOCK_FILE\",\"resultsIncludeRegex\":\"\",\"resultsExcludeRegex\":\"\"}", info="Generated FISJob JSON was different than expected")
})

test_that("FISJob can be read from JSON", {
    
    instance <- createFISJobFromNamedList(fromJSON(file=system.file("tests/data/json/FISJob.json", package = "DDMoRe")))
    
    expect_true(is.FISJob(instance), 
                info = "Instance was not of type FISServer"
    )
    
    expect_true(instance@id == "0a10dcb4-81fd-45a9-9251-f753f08bb7a7", info = "id was incorrect")
    expect_true(instance@executionType == "NONMEM", info = "executionType was incorrect")
    expect_true(instance@commandParameters == "", info = "commandParameters was incorrect")
    expect_true(instance@workingDirectory == "C:\\path\\Temp\\RtmpO0JTs6\\TEL.job23c461f131b8", info = "workingDirectory was incorrect")
    expect_true(instance@executionFile == "Friberg_PANSS.xml", info = "executionFile was incorrect")
    expect_true(length(instance@extraInputFiles) == 0, info = "extraInputFiles was incorrect")
    expect_true(instance@submitTime == "2015-08-10T14:10:07.266+01:00", info = "submitTime was incorrect")
    expect_true(instance@status == "COMPLETED", info = "status was incorrect")
    expect_true(instance@resultsIncludeRegex == ".*", info = "resultsIncludeRegex was incorrect")
    expect_true(instance@resultsExcludeRegex == "(nonmem.exe|temp_dir)", info = "resultsExcludeRegex was incorrect")
    expect_true(instance@version == 3, info = "version was incorrect")
    
})

test_that("FISJob with extra input files can be read from JSON", {
    
    instance <- createFISJobFromNamedList(fromJSON(file=system.file("tests/data/json/FISJobWithExtraInputFiles.json", package = "DDMoRe")))
    
    expect_true(is.FISJob(instance), 
                info = "Instance was not of type FISServer"
    )
    
    expect_equal(instance@id,"32efd6a9-23c5-45d2-98a0-83ee957f1b50", info = "id was incorrect")
    expect_equal(instance@executionType,"PsNgeneric", info = "executionType was incorrect")
    expect_equal(instance@commandParameters,"vpc --samples=50 --seed=12345 -n_simulation=10 -idv=CP -sim_model=sim_model_test2.mod", info = "commandParameters was incorrect")
    expect_equal(instance@workingDirectory,"C:\\Users\\MROGAL~1\\AppData\\Local\\Temp\\RtmpElXPAP\\TEL.job378441127536", info = "workingDirectory was incorrect")
    expect_equal(instance@executionFile,"UseCase11_VPC.xml", info = "executionFile was incorrect")
    expect_equal(length(instance@extraInputFiles),1, info = "extraInputFiles was incorrect")
    expect_equal(instance@extraInputFiles[[1]],"sim_model_test2.mod", info = "extraInputFiles was incorrect")
    expect_equal(instance@submitTime,"2015-10-01T11:06:36.248+01:00", info = "submitTime was incorrect")
    expect_equal(instance@status,"RUNNING", info = "status was incorrect")
    expect_equal(instance@resultsIncludeRegex,".*", info = "resultsIncludeRegex was incorrect")
    expect_equal(instance@resultsExcludeRegex,"(rundir)", info = "resultsExcludeRegex was incorrect")
    expect_equal(instance@version,2, info = "version was incorrect")
    
})
