library("DDMoRe.TEL")
library("XML")
require("methods")

clearTestVariables <- function() {
    rm(list=ls())
}
clearTestVariables()


context("FIS Server's createFISServer.")

test_that("createFISServer creates a valid object with defaults", {
    
    instance <- createFISServer(startupScript = "MOCK")
    
    expect_true(is.FISServer(instance), 
        info = "Instance was not of type FISServer"
    )
    
    
    expect_true(instance@url == "http://localhost:9010", info = "URL was incorrect.")
    expect_true(instance@operationalUrl == "http://localhost:9011", info = "operationalUrl was incorrect")
    expect_true(instance@startupScript == "MOCK", info = "startupScript was incorrect")
    expect_true(instance@jobStatusPollingDelay == 20, info = "jobStatusPollingDelay was incorrect")
    expect_true(instance@startupPollingMax == 120, info = "startupPollingMax was incorrect")
    expect_true(instance@startupPollingDelay == 1, info = "startupPollingDelay was incorrect")
    
})

test_that("createFISServer creates a valid object from a properties file", {
    
    instance <- createFISServerFromProperties(propertiesFile=system.file("tests/data/json/FISServerProperties.json", package = "DDMoRe.TEL"))
    
    expect_true(is.FISServer(instance), 
                info = "Instance was not of type FISServer"
    )
    
    expect_true(instance@url == "mock-url", info = "URL was incorrect.")
    expect_true(instance@operationalUrl == "mock-operationalUrl", info = "operationalUrl was incorrect")
    expect_true(instance@startupScript == "mock-startupScript", info = "startupScript was incorrect")
    expect_true(instance@jobStatusPollingDelay == 1, info = "jobStatusPollingDelay was incorrect")
    expect_true(instance@startupPollingMax == 60, info = "startupPollingMax was incorrect")
    expect_true(instance@startupPollingDelay == 1, info = "startupPollingDelay was incorrect")
    
})


old.httpPost <- .httpPost
context("FIS Server's submitJob")

test_that("submitJob returns a job that was returned from FIS", {
    .httpPost <- function(url, body = "", headers= c()) {
        response <- list()
        response$header <- list()
        response$header['status'] <- 200
        response$body <- paste(readLines(system.file("tests/data/json/FISJob.json", package = "DDMoRe.TEL"), warn = FALSE), collapse = "")
        return(response)
    }
    assignInNamespace('.httpPost', .httpPost, ns="DDMoRe.TEL")
    instance <- createFISServerFromProperties(propertiesFile=system.file("tests/data/json/FISServerProperties.json", package = "DDMoRe.TEL"))
    
    fisJob <- createFISJob(executionType = "MOCK-EXEC", executionFile = "MOCK-FILE", workingDirectory = "MOCK-DIR")
    
    submitted <- submitJob(instance, fisJob)
    
    expect_equal(submitted@id,"0a10dcb4-81fd-45a9-9251-f753f08bb7a7", info = "Job ID was incorrect.")
})

test_that("submitJob throws error for non 200 HTTP response status", {
    .httpPost <- function(url, body = "", headers= c()) {
        response <- list()
        response$header <- list()
        response$header['status'] <- 400
        return(response)
    }
    assignInNamespace('.httpPost', .httpPost, ns="DDMoRe.TEL")
    instance <- createFISServerFromProperties(propertiesFile=system.file("tests/data/json/FISServerProperties.json", package = "DDMoRe.TEL"))
    
    fisJob <- createFISJob(executionType = "MOCK-EXEC", executionFile = "MOCK-FILE", workingDirectory = "MOCK-DIR")
    
    expect_error(
        submitJob(instance, fisJob), info = "The submission should fail."
    )
})

context("FIS Server's cancelJob")
test_that("cancelJob returns cancelled job that was returned from FIS", {
    .httpPost <- function(url, body = "", headers= c()) {
        response <- list()
        response$header <- list()
        response$header['status'] <- 200
        response$body <- paste(readLines(system.file("tests/data/json/FISJob.json", package = "DDMoRe.TEL"), warn = FALSE), collapse = "")
        return(response)
    }
    assignInNamespace('.httpPost', .httpPost, ns="DDMoRe.TEL")
    instance <- createFISServerFromProperties(propertiesFile=system.file("tests/data/json/FISServerProperties.json", package = "DDMoRe.TEL"))
    
    fisJob <- createFISJob(executionType = "MOCK-EXEC", executionFile = "MOCK-FILE", workingDirectory = "MOCK-DIR")
    fisJob@id <- "0a10dcb4-81fd-45a9-9251-f753f08bb7a7"
    cancelled <- cancelJob(instance, fisJob)
    
    expect_equal(cancelled@id,"0a10dcb4-81fd-45a9-9251-f753f08bb7a7", info = "Job ID was incorrect.")
})

test_that("cancelJob throws error for non 200 HTTP response status", {
    .httpPost <- function(url, body = "", headers= c()) {
        response <- list()
        response$header <- list()
        response$header['status'] <- 400
        return(response)
    }
    assignInNamespace('.httpPost', .httpPost, ns="DDMoRe.TEL")
    instance <- createFISServerFromProperties(propertiesFile=system.file("tests/data/json/FISServerProperties.json", package = "DDMoRe.TEL"))
    
    fisJob <- createFISJob(executionType = "MOCK-EXEC", executionFile = "MOCK-FILE", workingDirectory = "MOCK-DIR")
    fisJob@id <- "0a10dcb4-81fd-45a9-9251-f753f08bb7a7"
    expect_error(
        cancelJob(instance, fisJob), info = "The cancellation should fail."
    )
})

# Both are needed - first works in dev environment, where the package scripts are just sourced, the latter 
# works during ANT build where DDMoRe package is loaded
.httpPost <- old.httpPost
assignInNamespace('.httpPost', old.httpPost, ns="DDMoRe.TEL")
